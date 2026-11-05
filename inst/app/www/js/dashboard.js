// Dashboard ("Fortschritt"): answer tree (branches, direction switch,
// learning-area chips), the "Fähigkeitsverlauf" panels and their time
// window, the "Wie wird das berechnet?" explainer. All client-side state,
// delegated to `document` so it survives the dashboard's re-renders.
// Loaded in ui.R (app_ui()); plain browser JS, no build step.

// Dashboard answer tree (answer_tree_card() in dashboard_details.R):
// a click on a node opens or closes its branch (several can be open).
// Pure client-side state, delegated so it survives the dashboard's
// re-renders; a re-render resets the tree to its default branches.
document.addEventListener('click', function (e) {
  var btn = e.target.closest('.lt-node[data-node]');
  if (!btn) return;
  var open = btn.parentElement.classList.toggle('is-open');
  btn.setAttribute('aria-expanded', open ? 'true' : 'false');
});

// Its toggle between the two reading directions (from the first
// attempt / from the latest one back) and its learning-area filter.
// Every view is in the page; the one matching the card's data-view
// and data-area is shown, the others get `hidden`. The choice is
// remembered for the page's lifetime and re-applied after each
// dashboard re-render.
(function () {
  var view = null, area = null;
  function apply(card) {
    var v = view || card.getAttribute('data-view');
    var a = area || card.getAttribute('data-area');
    if (!card.querySelector('.lt-view[data-area="' + a + '"]')) a = 'all';
    card.setAttribute('data-view', v);
    card.setAttribute('data-area', a);
    card.querySelectorAll('[data-lt-view]').forEach(function (b) {
      b.setAttribute('aria-pressed', b.getAttribute('data-lt-view') === v ? 'true' : 'false');
    });
    card.querySelectorAll('[data-lt-area]').forEach(function (b) {
      b.setAttribute('aria-pressed', b.getAttribute('data-lt-area') === a ? 'true' : 'false');
    });
    card.querySelectorAll('.lt-view').forEach(function (el) {
      el.hidden = !(el.getAttribute('data-view') === v && el.getAttribute('data-area') === a);
    });
  }
  document.addEventListener('click', function (e) {
    var btn = e.target.closest('[data-lt-view]');
    if (!btn) return;
    view = btn.getAttribute('data-lt-view');
    apply(btn.closest('.lt-card'));
  });
  document.addEventListener('click', function (e) {
    var chip = e.target.closest('[data-lt-area]');
    if (!chip) return;
    area = chip.getAttribute('data-lt-area');
    apply(chip.closest('.lt-card'));
  });
  $(document).on('shiny:value', function () {
    if (view || area) setTimeout(function () {
      document.querySelectorAll('.lt-card[data-view]').forEach(apply);
    }, 0);
  });
})();

// The "Fähigkeitsverlauf" panels (trajectory_panel_spec() in
// mod_dashboard.R): each panel carries its plotly.js figure spec as
// JSON; it's drawn once visible (plotly needs the panel's width), so
// when the accordion opens, and after a dashboard re-render. The
// time-window switch swaps in that window's x range, labels and
// note from the spec, no round trip. The window is remembered for
// the page's lifetime.
(function () {
  var win = 'all';
  function draw(panel) {
    var el = panel.querySelector('.traj-plot'), src = panel.querySelector('script.traj-spec');
    if (!el || !src || !window.Plotly || !el.offsetParent) return;
    if (el.getAttribute('data-drawn') === win) return;
    var f = JSON.parse(src.textContent), w = f.windows[win] || f.windows.all;
    var layout = Object.assign({}, f.layout, { annotations: w.annotations });
    layout.xaxis = Object.assign({}, f.layout.xaxis, { range: w.range, tickvals: w.tickvals, ticktext: w.ticktext });
    Plotly.react(el, f.data, layout, f.config);
    el.setAttribute('data-drawn', win);
  }
  function drawAll() {
    document.querySelectorAll('.traj-switch [data-traj-window]').forEach(function (b) {
      b.setAttribute('aria-pressed', b.getAttribute('data-traj-window') === win ? 'true' : 'false');
    });
    document.querySelectorAll('.traj-panel').forEach(draw);
  }
  document.addEventListener('click', function (e) {
    var btn = e.target.closest('.traj-switch [data-traj-window]');
    if (!btn) return;
    win = btn.getAttribute('data-traj-window');
    drawAll();
  });
  // 'show' fires before the panel is displayed; a tick later it has
  // its width (and is still animating open), so the charts appear
  // with the opening instead of after it
  $(document).on('show.bs.collapse shown.bs.collapse', function () { setTimeout(drawAll, 0); });
  $(document).on('shiny:value', function () { setTimeout(drawAll, 0); });
})();

// The "Wie wird das berechnet?" button in "Dein Lernstand im Detail":
// opens/closes the explainer next to the strips (the grid's
// data-explain). Remembered for the page's lifetime and re-applied
// after a re-render, like the tree toggle.
(function () {
  var open = false;
  function apply(grid) {
    grid.setAttribute('data-explain', open ? 'open' : 'closed');
    grid.querySelectorAll('[data-explain-toggle]').forEach(function (b) {
      b.setAttribute('aria-expanded', open ? 'true' : 'false');
    });
  }
  document.addEventListener('click', function (e) {
    var btn = e.target.closest('[data-explain-toggle]');
    if (!btn) return;
    var grid = btn.closest('[data-explain]');
    open = grid.getAttribute('data-explain') !== 'open';
    apply(grid);
    // Below 992 px it opens below all strips, out of sight: scroll to it
    var ex = grid.querySelector('.la-explainer');
    if (open && ex && window.matchMedia('(max-width: 991.98px)').matches) {
      var reduce = window.matchMedia('(prefers-reduced-motion: reduce)').matches;
      ex.scrollIntoView({ behavior: reduce ? 'auto' : 'smooth', block: 'start' });
    }
  });
  $(document).on('shiny:value', function () {
    if (open) setTimeout(function () {
      document.querySelectorAll('[data-explain]').forEach(apply);
    }, 0);
  });
})();
