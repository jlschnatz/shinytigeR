// Shared tooltips for every element with data-tip (see below).
// Loaded in ui.R (app_ui()); plain browser JS, no build step.

// Tooltips for every element with data-tip (dashboard calendar days,
// coverage cells, (i) buttons). One shared, fixed-position label
// instead of native `title`s: those appear late, look like the OS
// default and never show on touch screens. Mouse: hover; keyboard
// and touch: focus (a tap focuses the element). Fixed positioning so
// a scrolling container (the calendar) can't clip it.
(function () {
  var tip = null, current = null;
  function place() {
    if (!current || !tip) return;
    var r = current.getBoundingClientRect(), t = tip.getBoundingClientRect();
    var left = Math.min(Math.max(8, r.left + r.width / 2 - t.width / 2), window.innerWidth - t.width - 8);
    var top = r.top - t.height - 8, below = top < 8;
    if (below) top = r.bottom + 8;
    tip.style.left = left + 'px';
    tip.style.top = top + 'px';
    tip.classList.toggle('is-below', below);
  }
  function show(el) {
    if (!tip) {
      tip = document.createElement('div');
      tip.className = 'tiger-tip';
      tip.setAttribute('role', 'tooltip');
      document.body.appendChild(tip);
    }
    current = el;
    tip.textContent = el.getAttribute('data-tip');
    place();
    tip.classList.add('is-visible');
  }
  function hide() {
    current = null;
    if (tip) tip.classList.remove('is-visible');
  }
  document.addEventListener('pointerover', function (e) {
    if (e.pointerType !== 'mouse') return;
    var el = e.target.closest('[data-tip]');
    if (el) show(el);
  });
  document.addEventListener('pointerout', function (e) {
    if (e.pointerType !== 'mouse') return;
    var el = e.target.closest('[data-tip]');
    if (el && !el.contains(e.relatedTarget)) hide();
  });
  document.addEventListener('focusin', function (e) {
    var el = e.target.closest && e.target.closest('[data-tip]');
    if (el) show(el);
  });
  document.addEventListener('focusout', hide);
  // Follow the element when the page scrolls or shifts (scroll events
  // also fire on layout changes, so hiding on scroll would flicker)
  window.addEventListener('scroll', function () {
    if (current && !document.body.contains(current)) hide(); else place();
  }, true);
  window.addEventListener('resize', place);
})();
