// R console ("R-Konsole") in the practice view, PROTOTYPE: R runs in the
// browser with webR. The panel (mod_scratchpad_ui(), R/mod_scratchpad.R) is
// opened by the "R-Konsole" button on coding items; webR is only loaded on
// the first open. Loaded in ui.R; plain browser JS, no build step.
(function () {
  var DATA_DIR = '/home/web_user/.pandar';
  var consoles = {};   // panel id -> { webR, ready, busy }
  var serverData = {}; // panel id -> resolve() of the server's answer

  var editors = {};    // panel id -> Ace editor (absent: the plain textarea)
  var acePromise = null;

  function q(panel, sel) { return panel.querySelector(sel); }

  // ── Editor (Ace: R highlighting, auto-closed brackets and quotes) ──────────
  // Loaded on the first open, like webR. Until it's there (or if it fails),
  // the plain textarea works.
  function loadScript(src) {
    return new Promise(function (resolve, reject) {
      var s = document.createElement('script');
      s.src = src;
      s.onload = resolve;
      s.onerror = function () { reject(new Error('Laden fehlgeschlagen: ' + src)); };
      document.head.appendChild(s);
    });
  }

  function loadAce(base) {
    if (!acePromise) {
      acePromise = loadScript(base + 'ace.min.js')
        .then(function () {
          window.ace.config.set('basePath', base);
          return Promise.all([
            loadScript(base + 'mode-r.min.js'),
            loadScript(base + 'ext-language_tools.min.js') // completion popup
          ]);
        })
        .catch(function (e) { acePromise = null; throw e; });
    }
    return acePromise;
  }

  async function ensureEditor(panel) {
    if (editors[panel.id]) return;
    try {
      await loadAce(panel.dataset.aceUrl);
    } catch (e) {
      return; // stays the textarea
    }
    if (editors[panel.id]) return;
    var ta = q(panel, '.scratchpad-code');
    var cs = getComputedStyle(ta);
    var host = document.createElement('div');
    host.className = 'scratchpad-editor';
    ta.after(host);
    var ed = window.ace.edit(host, {
      mode: 'ace/mode/r',
      value: ta.value,
      placeholder: ta.placeholder,
      tabSize: 2,
      useSoftTabs: true,
      wrap: true,
      minLines: 6,
      maxLines: 18,
      showPrintMargin: false,
      showFoldWidgets: false,
      highlightActiveLine: false,
      fontSize: cs.fontSize,
      fontFamily: cs.fontFamily,
      // completion while typing (from 2 characters) and on Tab
      enableBasicAutocompletion: true,
      enableLiveAutocompletion: true,
      liveAutocompletionDelay: 250,
      liveAutocompletionThreshold: 2
    });
    ta.hidden = true;
    ed.textInput.getElement().setAttribute('aria-label', 'R-Code');
    // only R's completions (not Ace's word and snippet lists)
    ed.completers = [rCompleter(panel)];
    ed.commands.addCommand({
      name: 'tigerRunLineOrSelection',
      bindKey: { win: 'Ctrl-Enter', mac: 'Command-Enter|Ctrl-Enter' },
      exec: function (editor) { runFromEditor(panel, editor); }
    });
    // Tab after a name, "(" or "," completes (argument names too, like
    // RStudio), elsewhere it indents
    ed.commands.addCommand({
      name: 'tigerTab',
      bindKey: { win: 'Tab', mac: 'Tab' },
      exec: function (editor) {
        var pos = editor.getCursorPosition();
        var before = editor.session.getLine(pos.row).slice(0, pos.column);
        if (editor.selection.isEmpty() && /([A-Za-z0-9._$@]|\(|,\s*)$/.test(before)) {
          editor.execCommand('startAutocomplete');
        } else {
          editor.indent();
        }
      }
    });
    // F1: the help page of the name at the cursor (like RStudio)
    ed.commands.addCommand({
      name: 'tigerHelp',
      bindKey: { win: 'F1', mac: 'F1' },
      exec: function (editor) {
        var pos = editor.getCursorPosition();
        var name = nameAt(editor.session.getLine(pos.row), pos.column);
        if (name) helpFor(panel, name);
      }
    });
    // The signature of the call the cursor is in, below the editor
    var sigTimer = null;
    ed.selection.on('changeCursor', function () {
      clearTimeout(sigTimer);
      sigTimer = setTimeout(function () { updateSignature(panel, ed); }, 150);
    });
    editors[panel.id] = ed;
    if (panel.classList.contains('show')) ed.focus();
  }

  // ── Completion, signature, help (R side: .tiger_complete(),
  // .tiger_signature(), .tiger_help() in www/scratchpad/setup.R) ───────────
  // Only while R is idle: R handles one request at a time, so a completion
  // would otherwise wait for the running code.
  function idleConsole(panel) {
    var con = consoles[panel.id];
    return con && con.ready && !con.busy ? con : null;
  }

  var COMPLETION_META = { arg: 'Argument', fun: 'Funktion', obj: 'Objekt' };

  function rCompleter(panel) {
    return {
      // R names: dots, $ and @ (columns, slots), :: (pkg::fun)
      identifierRegexps: [/[a-zA-Z_0-9.$@:]/],
      getCompletions: function (editor, session, pos, prefix, callback) {
        var con = idleConsole(panel);
        if (!con) return callback(null, []);
        var line = session.getLine(pos.row).slice(0, pos.column);
        con.webR.evalRRaw('.tiger_complete(' + JSON.stringify(line) + ')', 'string[]').then(
          function (vals) {
            callback(null, vals.map(function (v, i) {
              var tab = v.indexOf('\t');
              var kind = v.slice(0, tab), val = v.slice(tab + 1);
              var arg = kind === 'arg';
              return {
                caption: val,
                // "na.rm=" is inserted as "na.rm = "
                value: arg ? val.replace(/=$/, ' = ') : val,
                meta: COMPLETION_META[kind] || '',
                // arguments first, otherwise R's order
                score: (arg ? 2000 : 1000) - i
              };
            }));
          },
          function () { callback(null, []); }
        );
      }
    };
  }

  // The name of the function whose call the cursor is in ("t.test" in
  // `t.test(x, al|`), or ''. Scans the line up to the cursor, skipping
  // strings.
  function enclosingCall(line, col) {
    var stack = [], quote = null;
    for (var i = 0; i < col; i++) {
      var c = line.charAt(i);
      if (quote) {
        if (c === '\\') i++;
        else if (c === quote) quote = null;
      } else if (c === '"' || c === "'") {
        quote = c;
      } else if (c === '(') {
        stack.push(i);
      } else if (c === ')') {
        stack.pop();
      }
    }
    if (!stack.length) return '';
    var m = line.slice(0, stack[stack.length - 1]).match(/([A-Za-z.][A-Za-z0-9._]*(?:::[A-Za-z.][A-Za-z0-9._]*)?)\s*$/);
    return m ? m[1] : '';
  }

  // The R name at a column ("t.test" anywhere in it), or ''
  function nameAt(line, col) {
    var re = /[A-Za-z0-9._]/;
    var from = col, to = col;
    while (from > 0 && re.test(line.charAt(from - 1))) from--;
    while (to < line.length && re.test(line.charAt(to))) to++;
    return line.slice(from, to);
  }

  var signatures = {}; // name -> signature ('' = none), per page
  async function updateSignature(panel, ed) {
    var el = q(panel, '.scratchpad-signature');
    var pos = ed.getCursorPosition();
    var name = enclosingCall(ed.session.getLine(pos.row), pos.column);
    if (!name) { el.textContent = ''; return; }
    if (!(name in signatures)) {
      var con = idleConsole(panel);
      if (!con) return;
      try {
        var s = await con.webR.evalRRaw('.tiger_signature(' + JSON.stringify(name) + ')', 'string');
        signatures[name] = s;
      } catch (e) {
        signatures[name] = '';
      }
    }
    el.textContent = signatures[name];
  }

  // A help page arrives as lines marked \003 (setup.R's help printing); the
  // first is "topic {package}"
  function showHelp(panel, lines) {
    var box = q(panel, '.scratchpad-help');
    q(box, '.scratchpad-help-title').textContent = 'Hilfe: ' + lines[0];
    var body = q(box, '.scratchpad-help-body');
    body.textContent = lines.slice(1).join('\n').replace(/^\n+/, '');
    box.hidden = false;
    body.scrollTop = 0;
  }

  async function helpFor(panel, topic) {
    var con = idleConsole(panel);
    if (!con) return;
    var shelter = await new con.webR.Shelter();
    try {
      var res = await shelter.captureR('.tiger_help(' + JSON.stringify(topic) + ')', {
        withAutoprint: false, captureStreams: true, captureConditions: false, captureGraphics: false
      });
      showOutput(panel, res.output);
      scrollOutput(panel);
    } catch (e) {
      // no help then
    } finally {
      shelter.purge();
    }
  }

  function getCode(panel) {
    var ed = editors[panel.id];
    return ed ? ed.getValue() : q(panel, '.scratchpad-code').value;
  }

  function focusEditor(panel) {
    var ed = editors[panel.id];
    if (ed) ed.focus(); else q(panel, '.scratchpad-code').focus();
  }

  // Ctrl/Cmd + Enter in Ace: the selection, or else the current line (then
  // the cursor moves to the next line, like in RStudio)
  function runFromEditor(panel, ed) {
    var sel = ed.getSelectedText();
    if (sel) return run(panel, sel);
    var row = ed.getCursorPosition().row;
    var line = ed.session.getLine(row);
    var next = Math.min(row + 1, ed.session.getLength() - 1);
    ed.gotoLine(next + 1, 0, false);
    return run(panel, line);
  }

  function setStatus(panel, text, kind) {
    var el = q(panel, '.scratchpad-status');
    el.textContent = text;
    el.dataset.kind = kind || '';
  }

  function b64ToBytes(b64) {
    var bin = atob(b64);
    var out = new Uint8Array(bin.length);
    for (var i = 0; i < bin.length; i++) out[i] = bin.charCodeAt(i);
    return out;
  }

  // ── Output ─────────────────────────────────────────────────────────────────
  // .tiger_run() (www/scratchpad/setup.R) marks echoed code lines with \001
  // (first line of an expression) and \002 (continuation lines)
  function appendLines(panel, type, text) {
    var out = q(panel, '.scratchpad-output');
    text.replace(/\n$/, '').split('\n').forEach(function (line) {
      var div = document.createElement('div');
      if (line.charAt(0) === '\u0001' || line.charAt(0) === '\u0002') {
        div.className = 'sp-line sp-echo';
        div.textContent = (line.charAt(0) === '\u0001' ? '> ' : '+ ') + line.slice(1);
      } else {
        div.className = 'sp-line ' + (type === 'stderr' ? 'sp-err' : 'sp-out');
        div.textContent = line;
      }
      out.appendChild(div);
    });
  }

  // captureR()'s output items, in order: help-page lines (\003, see
  // showHelp()) go to the help area, everything else to the output
  function showOutput(panel, items) {
    var help = [];
    items.forEach(function (o) {
      var rest = [];
      String(o.data).replace(/\n$/, '').split('\n').forEach(function (line) {
        if (line.charAt(0) === '\u0003') help.push(line.slice(1));
        else rest.push(line);
      });
      if (rest.length) appendLines(panel, o.type, rest.join('\n'));
    });
    if (help.length) showHelp(panel, help);
  }

  function appendImage(panel, bitmap) {
    var canvas = document.createElement('canvas');
    canvas.className = 'sp-plot';
    canvas.width = bitmap.width;
    canvas.height = bitmap.height;
    canvas.getContext('2d').drawImage(bitmap, 0, 0);
    canvas.setAttribute('role', 'img');
    canvas.setAttribute('aria-label', 'Grafik');
    q(panel, '.scratchpad-output').appendChild(canvas);
  }

  function scrollOutput(panel) {
    var out = q(panel, '.scratchpad-output');
    out.scrollTop = out.scrollHeight;
  }

  // ── Start-up (first open) ──────────────────────────────────────────────────
  async function start(panel) {
    var con = consoles[panel.id] = { webR: null, ready: false, busy: false };
    setStatus(panel, 'R wird geladen …', 'busy');
    try {
      // the server's answer (pandaR datasets, packages), requested in parallel
      var dataPromise = new Promise(function (resolve) { serverData[panel.id] = resolve; });
      Shiny.setInputValue(panel.dataset.initInput, Date.now(), { priority: 'event' });
      var setupPromise = fetch('scratchpad/setup.R', { cache: 'no-cache' }).then(function (r) { return r.text(); });

      var base = panel.dataset.webrUrl;
      var mod = await import(base + 'webr.mjs');
      var webR = new mod.WebR({ baseUrl: base });
      await webR.init();
      con.webR = webR;

      var data = await dataPromise;
      await webR.FS.mkdir(DATA_DIR);
      for (var i = 0; i < data.files.length; i++) {
        var f = data.files[i];
        await webR.FS.writeFile(DATA_DIR + '/' + f.name, b64ToBytes(f.b64));
      }
      await webR.evalRVoid(await setupPromise);
      if (data.packages && data.packages.length) {
        setStatus(panel, 'Pakete werden geladen …', 'busy');
        await webR.installPackages(data.packages, { quiet: true });
      }
      if (data.failed && data.failed.length) {
        appendLines(panel, 'stderr', 'Hinweis: Diese Datensätze konnten nicht geladen werden: ' + data.failed.join(', '));
      }
      con.ready = true;
      setStatus(panel, 'Bereit', 'ready');
      q(panel, '.scratchpad-run').disabled = false;
      focusEditor(panel);
    } catch (e) {
      setStatus(panel, 'Fehler beim Laden', 'error');
      appendLines(panel, 'stderr', 'R konnte nicht gestartet werden: ' + (e && e.message ? e.message : e));
      delete consoles[panel.id]; // the next open tries again
    }
  }

  // ── Running code ───────────────────────────────────────────────────────────
  async function run(panel, code) {
    var con = consoles[panel.id];
    if (!con || !con.ready || con.busy || !code.trim()) return;
    con.busy = true;
    var btn = q(panel, '.scratchpad-run');
    btn.disabled = true;
    setStatus(panel, 'Läuft …', 'busy');
    var shelter = await new con.webR.Shelter();
    try {
      // JSON string literals are valid R string literals
      var res = await shelter.captureR('.tiger_run(' + JSON.stringify(code) + ')', {
        withAutoprint: false,
        captureStreams: true,
        captureConditions: false,
        captureGraphics: { width: 504, height: 378 }
      });
      showOutput(panel, res.output);
      (res.images || []).forEach(function (img) { appendImage(panel, img); });
    } catch (e) {
      appendLines(panel, 'stderr', 'Fehler: ' + (e && e.message ? e.message : e));
    } finally {
      shelter.purge();
      con.busy = false;
      btn.disabled = false;
      setStatus(panel, 'Bereit', 'ready');
      scrollOutput(panel);
    }
  }

  // Ctrl/Cmd + Enter: the selection, or else the current line (then the
  // cursor moves to the next line, like in RStudio)
  function runLineOrSelection(panel, ta) {
    var s = ta.selectionStart, e = ta.selectionEnd, v = ta.value;
    if (s !== e) return run(panel, v.slice(s, e));
    var from = v.lastIndexOf('\n', s - 1) + 1;
    var to = v.indexOf('\n', s);
    if (to === -1) to = v.length;
    var next = Math.min(to + 1, v.length);
    ta.setSelectionRange(next, next);
    return run(panel, v.slice(from, to));
  }

  // ── Wiring ─────────────────────────────────────────────────────────────────
  $(document).on('shiny:connected', function () {
    Shiny.addCustomMessageHandler('tiger-scratchpad', function (msg) {
      var resolve = serverData[msg.id];
      if (resolve) { delete serverData[msg.id]; resolve(msg); }
    });
  });

  // The opener: moves the panel to <body> (so no hidden or transformed
  // ancestor traps the fixed panel), toggles it, starts R on the first open
  $(document).on('click', '.scratchpad-open-btn', function () {
    var panel = document.getElementById(this.dataset.panel);
    if (!panel) return;
    if (panel.parentNode !== document.body) document.body.appendChild(panel);
    bootstrap.Offcanvas.getOrCreateInstance(panel).toggle();
    ensureEditor(panel);
    if (!consoles[panel.id]) start(panel);
  });

  // While open, the page makes room for it on wide screens (CSS)
  document.addEventListener('show.bs.offcanvas', function (ev) {
    if (ev.target.classList.contains('scratchpad')) document.body.classList.add('scratchpad-open');
  });
  document.addEventListener('hide.bs.offcanvas', function (ev) {
    if (ev.target.classList.contains('scratchpad')) document.body.classList.remove('scratchpad-open');
  });
  document.addEventListener('shown.bs.offcanvas', function (ev) {
    if (ev.target.classList.contains('scratchpad')) {
      var ed = editors[ev.target.id];
      if (ed) ed.resize();
      focusEditor(ev.target);
    }
  });

  // Leaving the practice view (another tab, or back to the selector) closes it
  $(document).on('shown.bs.tab', function () {
    var btn = document.querySelector('.scratchpad-open-btn');
    if (btn && btn.offsetParent !== null) return;
    document.querySelectorAll('.scratchpad.show').forEach(function (p) {
      bootstrap.Offcanvas.getOrCreateInstance(p).hide();
    });
  });

  $(document).on('click', '.scratchpad-run', function () {
    var panel = this.closest('.scratchpad');
    run(panel, getCode(panel));
  });
  $(document).on('click', '.scratchpad-help-close', function () {
    var panel = this.closest('.scratchpad');
    q(panel, '.scratchpad-help').hidden = true;
    focusEditor(panel);
  });
  $(document).on('click', '.scratchpad-clear', function () {
    q(this.closest('.scratchpad'), '.scratchpad-output').textContent = '';
  });
  $(document).on('keydown', '.scratchpad-code', function (ev) {
    if (ev.key === 'Enter' && (ev.ctrlKey || ev.metaKey)) {
      ev.preventDefault();
      runLineOrSelection(this.closest('.scratchpad'), this);
    }
  });
})();
