// Practice view: MathJax typesetting, the item-ID badge's "Kopiert"
// confirmation, the code blocks' copy button, the image zoom, and
// scrolling the feedback into view after the check.
// Loaded in ui.R (app_ui()); plain browser JS, no build step.

// MathJax 3 (www/mathjax/, loaded with defer in ui.R). Typesets `els` (an
// array of elements, or null for the whole page) once MathJax has started;
// before it has loaded this does nothing, its startup typesets the page.
function mjTypeset(els) {
  var mj = window.MathJax;
  if (!mj || !mj.typesetPromise || !mj.startup) return Promise.resolve();
  return mj.startup.promise
    .then(function () { return mj.typesetPromise(els || undefined); })
    .catch(function (err) { console.warn('MathJax:', err); });
}

// Whole-page typeset, still used by the inspect view (mod_inspect.R)
Shiny.addCustomMessageHandler('mathjax_typeset', function(_) {
  mjTypeset(null);
});

// Practice view (stimulus, MC options, feedback; class .mj-output): typeset
// only the output that changed, and keep it invisible until it's ready
// (.mj-pending), so raw \( … \) and the reflow after it never show. Before,
// each of these outputs sent 'mathjax_typeset', which re-scanned the whole
// page two or three times per item while the raw LaTeX was visible.
//
// A new item (its data-mj-key differs from the last one shown) arrives as
// several outputs (stimulus, options). They're revealed together, once all
// are inserted, their math typeset, their images loaded and the fonts ready
// (Fira Mono loads the first time code shows), then fade in (.mj-enter; it
// stays on until the next new item, since removing it would restart the
// outputs' older slide-in animation). Meanwhile the view keeps its height,
// so the page doesn't collapse and jump while the old item is replaced, and
// if the card's top was scrolled out of view, the new item starts at its top.
// 'shiny:value' fires before Shiny inserts the HTML (possibly
// asynchronously, after its dependencies), so each output waits for its
// insertion (MutationObserver); a timeout shows everything anyway if
// something goes wrong.
(function () {
  var MATH = /\\\(|\\\[/;
  var batch = null; // the new item being prepared in the card

  function imagesLoaded(el) {
    var imgs = Array.prototype.slice.call(el.querySelectorAll('img'));
    return Promise.all(imgs.map(function (img) {
      if (img.complete) return null;
      return img.decode ? img.decode().catch(function () {}) : null;
    }));
  }

  function fontsReady() {
    return document.fonts && document.fonts.ready ? document.fonts.ready : Promise.resolve();
  }

  function show(els, fade) {
    els.forEach(function (el) {
      el.classList.remove('mj-pending');
      if (fade) {
        void el.offsetWidth; // restart the animation
        el.classList.add('mj-enter');
      }
    });
  }

  // Prepare one output: wait for its insertion, typeset its math, wait for
  // its images. Calls done() once (or never, then the timeout takes over).
  function prepare(el, hasMath, done) {
    var obs = new MutationObserver(function () {
      obs.disconnect();
      var typeset = hasMath ? mjTypeset([el]) : Promise.resolve();
      typeset.then(function () { return imagesLoaded(el); }).then(done, done);
    });
    obs.observe(el, { childList: true });
  }

  function finishBatch(b) {
    if (b.finished) return;
    b.finished = true;
    clearTimeout(b.timer);
    if (batch === b) batch = null;
    fontsReady().then(function () {
      var card = b.wrap && b.wrap.querySelector('.practice-card');
      if (card) {
        var top = card.getBoundingClientRect().top;
        if (top < 0) window.scrollBy(0, top - 16);
      }
      show(b.els, true);
      requestAnimationFrame(function () {
        if (b.wrap) b.wrap.style.minHeight = '';
      });
    });
  }

  $(document).on('shiny:value', function (e) {
    var el = e.target;
    if (!el.classList || !el.classList.contains('mj-output')) return;
    var html = e.value && typeof e.value === 'object' ? (e.value.html || '') : String(e.value || '');
    var m = html.match(/data-mj-key="([^"]*)"/);
    var key = m ? m[1] : '';
    var isNew = key !== el.getAttribute('data-mj-shown');
    el.setAttribute('data-mj-shown', key);
    var hasMath = MATH.test(html);
    if (!isNew && !hasMath) return;
    // forget the math being replaced (MathJax keeps a list of what it typeset)
    if (window.MathJax && MathJax.typesetClear) MathJax.typesetClear([el]);

    el.classList.remove('mj-enter');
    el.classList.add('mj-pending');

    // Outside the card (the feedback) or a re-render of the same item: on
    // its own, no batch
    var wrap = el.closest('.practice-wrap');
    var inCard = !!el.closest('.practice-card');
    if (!isNew || !inCard) {
      var once = false;
      var single = function () {
        if (once) return;
        once = true;
        clearTimeout(t);
        show([el], isNew && inCard);
      };
      var t = setTimeout(single, 1500);
      prepare(el, hasMath, single);
      return;
    }

    if (!batch || batch.key !== key || batch.finished) {
      if (batch && !batch.finished) finishBatch(batch);
      batch = { key: key, els: [], open: 0, wrap: wrap, finished: false };
      if (wrap) wrap.style.minHeight = wrap.offsetHeight + 'px';
      var b0 = batch;
      b0.timer = setTimeout(function () { finishBatch(b0); }, 1500);
    }
    var b = batch;
    b.els.push(el);
    b.open += 1;
    var counted = false;
    prepare(el, hasMath, function () {
      if (counted) return;
      counted = true;
      b.open -= 1;
      // the item's outputs arrive in one message, so by now all of them
      // have registered; a tick later in case one is still being inserted
      if (b.open === 0) setTimeout(function () { if (b.open === 0) finishBatch(b); }, 0);
    });
  });
})();

// MC options after the check (mod_mc_answer.R): mark the result on the
// options already shown instead of re-rendering them, so their typeset
// math stays as it is. m.sel is the selected option (null after a skip).
Shiny.addCustomMessageHandler('mc_mark_result', function (m) {
  var box = document.getElementById(m.id);
  if (!box) return;
  box.classList.add('is-checked');
  box.querySelectorAll('input[type="radio"]').forEach(function (r) {
    var row = r.closest('.answer-option, .answer-img-row');
    var isSel = m.sel !== null && m.sel !== undefined && String(r.value) === String(m.sel);
    r.disabled = true;
    r.checked = isSel;
    if (isSel) {
      r.classList.add(m.radio_class);
      if (row) row.classList.add(m.row_class);
    } else if (row) {
      row.classList.add('is-dimmed');
    }
  });
});

// Click confirmation for item_id_badge() (utils.R). Bound ONCE here at
// page load, deliberately not via a Shiny observeEvent on the button's
// input: both practice and inspect badges are rebuilt by a renderUI on
// every item change, and Shiny's client never forgets an input's last
// sent value on unbind (see InputNoResendDecorator / unbindInputs in
// shiny.js) — the fresh button re-sends its reset value (0) on every
// such re-render, which differs from the cached post-click value and
// so is resent as a genuine change. A server-side observer would fire
// on every item advance, not just on real clicks. This listener never
// talks to the server, so that doesn't apply here.
// rclipButton() also injects its own per-render `new ClipboardJS(...)`
// (see the comment on item_id_badge() in utils.R) — that instance just
// performs a redundant, harmless copy of the same text; it has no
// success handler, so it never causes a second confirmation flash.
document.addEventListener('DOMContentLoaded', function () {
  if (!window.ClipboardJS) return;
  var clip = new ClipboardJS('.practice-item-id-btn');
  clip.on('success', function (e) {
    var el = e.trigger;
    var label = el.querySelector('.action-label');
    if (!label || el.dataset.busy) return;
    el.dataset.busy = '1';
    var prev = label.innerHTML;
    label.innerHTML = '\u2713 Kopiert';
    el.classList.add('is-copied');
    setTimeout(function () {
      label.innerHTML = prev;
      el.classList.remove('is-copied');
      delete el.dataset.busy;
    }, 1200);
    e.clearSelection();
  });
});

// Practice: after the check, bring the feedback card into view if it
// lands below the screen (a long stimulus above it, or stacked below
// the options on narrow screens) or behind the button bar / footer. Its scroll-margin (app.css) keeps
// it clear of the sticky button bar.
$(document).on('shiny:value', function (e) {
  if (!$(e.target).closest('.practice-feedback').length) return;
  setTimeout(function () {
    var card = e.target.querySelector('.feedback-card');
    if (!card) return;
    var bar = document.querySelector('.practice-actions-bar');
    var foot = document.querySelector('.app-footer');
    var limit = window.innerHeight - (bar ? bar.offsetHeight : 0) - (foot ? foot.offsetHeight : 0);
    if (card.getBoundingClientRect().bottom <= limit) return;
    var reduce = window.matchMedia('(prefers-reduced-motion: reduce)').matches;
    card.scrollIntoView({ behavior: reduce ? 'auto' : 'smooth', block: 'nearest' });
  }, 50);
});

// Copy button of the code blocks in item texts (format_code_blocks() in
// utils.R): copies the block's text as typed (the highlighting spans don't
// add characters). Delegated via ClipboardJS's selector, so blocks from
// later renders work too; ClipboardJS (from rclipboard) also works without
// the async clipboard API, which needs HTTPS.
document.addEventListener('DOMContentLoaded', function () {
  if (!window.ClipboardJS) return;
  var clip = new ClipboardJS('.code-copy-btn', {
    text: function (btn) {
      var code = btn.closest('.code-block').querySelector('pre code');
      return code ? code.textContent : '';
    }
  });
  clip.on('success', function (e) {
    var btn = e.trigger, label = btn.querySelector('.code-copy-label');
    if (!label || btn.dataset.busy) return;
    btn.dataset.busy = '1';
    label.textContent = '\u2713 Kopiert';
    btn.classList.add('is-copied');
    setTimeout(function () {
      label.textContent = 'Kopieren';
      btn.classList.remove('is-copied');
      delete btn.dataset.busy;
    }, 1200);
    e.clearSelection();
  });
});

// Image zoom (stimulus_img() / answer_img() in utils.R): a click on a
// stimulus image, or on an answer image's magnifier button, shows the image
// enlarged in a native <dialog> (Esc, a click anywhere or the x closes it;
// the dialog keeps the focus inside while open and returns it afterwards).
// Scaled to fit the window, small images up to 3x their size. One dialog
// for the page, created on first use; delegated, so re-rendered items work.
(function () {
  var dlg = null, img = null, opener = null;
  function fit() {
    if (!img.naturalWidth) return;
    var maxW = window.innerWidth * 0.94, maxH = window.innerHeight * 0.86;
    var k = Math.min(maxW / img.naturalWidth, maxH / img.naturalHeight, 3);
    img.style.width = Math.round(img.naturalWidth * k) + 'px';
    img.style.height = Math.round(img.naturalHeight * k) + 'px';
  }
  function ensure() {
    if (dlg) return;
    dlg = document.createElement('dialog');
    dlg.className = 'img-zoom';
    dlg.setAttribute('aria-label', 'Vergr\u00f6\u00dfertes Bild');
    dlg.innerHTML = '<button type="button" class="img-zoom-close" aria-label="Schlie\u00dfen">\u00d7</button><img alt="">';
    document.body.appendChild(dlg);
    img = dlg.querySelector('img');
    img.addEventListener('load', fit);
    dlg.addEventListener('click', function () { dlg.close(); });
    dlg.addEventListener('close', function () { if (opener) opener.focus(); opener = null; });
    window.addEventListener('resize', function () { if (dlg.open) fit(); });
  }
  function open(src, from) {
    ensure();
    opener = from;
    img.style.width = img.style.height = '';
    img.src = src;
    if (img.complete) fit();
    dlg.showModal();
  }
  document.addEventListener('click', function (e) {
    var btn = e.target.closest('.img-zoom-btn');
    if (btn) {
      // inside the option's <label>: don't select the answer
      e.preventDefault(); e.stopPropagation();
      open(btn.getAttribute('data-zoom-src'), btn);
      return;
    }
    var im = e.target.closest('img.is-zoomable');
    if (im) open(im.getAttribute('src'), im);
  }, true);
  document.addEventListener('keydown', function (e) {
    if ((e.key === 'Enter' || e.key === ' ') && e.target.matches && e.target.matches('img.is-zoomable')) {
      e.preventDefault();
      open(e.target.getAttribute('src'), e.target);
    }
  });
})();
