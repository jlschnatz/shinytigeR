// Practice view: MathJax re-typesetting, the item-ID badge's "Kopiert"
// confirmation, the code blocks' copy button, the image zoom, and
// scrolling the feedback into view after the check.
// Loaded in ui.R (app_ui()); plain browser JS, no build step.

Shiny.addCustomMessageHandler('mathjax_typeset', function(_) {
  if (window.MathJax) MathJax.Hub.Queue(['Typeset', MathJax.Hub]);
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
