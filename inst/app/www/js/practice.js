// Practice view: MathJax re-typesetting, the item-ID badge's "Kopiert"
// confirmation, and scrolling the feedback into view after the check.
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
// the options on narrow screens). Its scroll-margin (app.css) keeps
// it clear of the sticky button bar.
$(document).on('shiny:value', function (e) {
  if (!$(e.target).closest('.practice-feedback').length) return;
  setTimeout(function () {
    var card = e.target.querySelector('.feedback-card');
    if (!card) return;
    var bar = document.querySelector('.practice-actions-bar');
    var limit = window.innerHeight - (bar ? bar.offsetHeight : 0);
    if (card.getBoundingClientRect().bottom <= limit) return;
    var reduce = window.matchMedia('(prefers-reduced-motion: reduce)').matches;
    card.scrollIntoView({ behavior: reduce ? 'auto' : 'smooth', block: 'nearest' });
  }, 50);
});
