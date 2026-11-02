#' App UI
#' @return A [bslib::page_navbar()] UI definition.
#' @export
app_ui <- function() {
  bslib::page_navbar(
    title = actionLink(
      inputId = "brand_home",
      label = div(
        class = "d-flex align-items-center gap-2",
        tags$img(src = "img_app/tiger_logo_white.png", height = "26px"),
        tags$b("tigeR", class = "fw-bolder")
      ),
      class = "navbar-brand-link"
    ),
    id = "main_tabs",
    navbar_options = bslib::navbar_options(
      collapsible = TRUE,
      bg = PRIMARY_COLOR,
      underline = FALSE
    ),
    theme = bslib::bs_theme(
      version = 5,
      primary = PRIMARY_COLOR,
      font_scale = 1.0,
      base_font = bslib::font_google("Source Sans 3")
    ),
    header = tagList(
      shinyjs::useShinyjs(),
      rclipboard::rclipboardSetup(),
      tags$head(
        # Cache-busting query: without it browsers keep serving a stale
        # app.css after it changes, so new markup renders unstyled. runApp()
        # sets the working directory to the app dir (inst/app, or the
        # installed copy via run_app()), so this path resolves in both.
        tags$link(rel = "stylesheet", href = css_href("www/css/app.css", "css/app.css")),
        tags$link(rel = "icon", type = "image/png", href = "img_app/favicon.png"),
        tags$script(shiny::HTML(
          "Shiny.addCustomMessageHandler('mathjax_typeset', function(_) {",
          "  if (window.MathJax) MathJax.Hub.Queue(['Typeset', MathJax.Hub]);",
          "});",
          # Click confirmation for item_id_badge() (utils.R). Bound ONCE here at
          # page load, deliberately not via a Shiny observeEvent on the button's
          # input: both practice and inspect badges are rebuilt by a renderUI on
          # every item change, and Shiny's client never forgets an input's last
          # sent value on unbind (see InputNoResendDecorator / unbindInputs in
          # shiny.js) — the fresh button re-sends its reset value (0) on every
          # such re-render, which differs from the cached post-click value and
          # so is resent as a genuine change. A server-side observer would fire
          # on every item advance, not just on real clicks. This listener never
          # talks to the server, so that doesn't apply here.
          # rclipButton() also injects its own per-render `new ClipboardJS(...)`
          # (see the comment on item_id_badge() in utils.R) — that instance just
          # performs a redundant, harmless copy of the same text; it has no
          # success handler, so it never causes a second confirmation flash.
          "document.addEventListener('DOMContentLoaded', function () {",
          "  if (!window.ClipboardJS) return;",
          "  var clip = new ClipboardJS('.practice-item-id-btn');",
          "  clip.on('success', function (e) {",
          "    var el = e.trigger;",
          "    var label = el.querySelector('.action-label');",
          "    if (!label || el.dataset.busy) return;",
          "    el.dataset.busy = '1';",
          "    var prev = label.innerHTML;",
          "    label.innerHTML = '\\u2713 Kopiert';",
          "    el.classList.add('is-copied');",
          "    setTimeout(function () {",
          "      label.innerHTML = prev;",
          "      el.classList.remove('is-copied');",
          "      delete el.dataset.busy;",
          "    }, 1200);",
          "    e.clearSelection();",
          "  });",
          "});",
          # Dashboard answer tree (answer_tree_card() in dashboard_details.R):
          # a click on a node opens or closes its branch (several can be open).
          # Pure client-side state, delegated so it survives the dashboard's
          # re-renders; a re-render resets the tree to its default branches.
          "document.addEventListener('click', function (e) {",
          "  var btn = e.target.closest('.lt-node[data-node]');",
          "  if (!btn) return;",
          "  var open = btn.parentElement.classList.toggle('is-open');",
          "  btn.setAttribute('aria-expanded', open ? 'true' : 'false');",
          "});",
          # Its toggle between the two reading directions (from the first
          # attempt / from the latest one back). Both views are in the page;
          # the card's data-view picks one. The choice is remembered for the
          # page's lifetime and re-applied after each dashboard re-render.
          "(function () {",
          "  var view = null;",
          "  function apply(card, v) {",
          "    card.setAttribute('data-view', v);",
          "    card.querySelectorAll('[data-lt-view]').forEach(function (b) {",
          "      b.setAttribute('aria-pressed', b.getAttribute('data-lt-view') === v ? 'true' : 'false');",
          "    });",
          "  }",
          "  document.addEventListener('click', function (e) {",
          "    var btn = e.target.closest('[data-lt-view]');",
          "    if (!btn) return;",
          "    view = btn.getAttribute('data-lt-view');",
          "    apply(btn.closest('.lt-card'), view);",
          "  });",
          "  $(document).on('shiny:value', function () {",
          "    if (view) setTimeout(function () {",
          "      document.querySelectorAll('.lt-card[data-view]').forEach(function (c) { apply(c, view); });",
          "    }, 0);",
          "  });",
          "})();",
          # The "Wie wird das berechnet?" button in "Dein Lernstand im Detail":
          # opens/closes the explainer next to the strips (the grid's
          # data-explain). Remembered for the page's lifetime and re-applied
          # after a re-render, like the tree toggle.
          "(function () {",
          "  var open = false;",
          "  function apply(grid) {",
          "    grid.setAttribute('data-explain', open ? 'open' : 'closed');",
          "    grid.querySelectorAll('[data-explain-toggle]').forEach(function (b) {",
          "      b.setAttribute('aria-expanded', open ? 'true' : 'false');",
          "    });",
          "  }",
          "  document.addEventListener('click', function (e) {",
          "    var btn = e.target.closest('[data-explain-toggle]');",
          "    if (!btn) return;",
          "    var grid = btn.closest('[data-explain]');",
          "    open = grid.getAttribute('data-explain') !== 'open';",
          "    apply(grid);",
          # Below 992 px it opens below all strips, out of sight: scroll to it
          "    var ex = grid.querySelector('.la-explainer');",
          "    if (open && ex && window.matchMedia('(max-width: 991.98px)').matches) {",
          "      var reduce = window.matchMedia('(prefers-reduced-motion: reduce)').matches;",
          "      ex.scrollIntoView({ behavior: reduce ? 'auto' : 'smooth', block: 'start' });",
          "    }",
          "  });",
          "  $(document).on('shiny:value', function () {",
          "    if (open) setTimeout(function () {",
          "      document.querySelectorAll('[data-explain]').forEach(apply);",
          "    }, 0);",
          "  });",
          "})();",
          # Practice: after the check, bring the feedback card into view if it
          # lands below the screen (a long stimulus above it, or stacked below
          # the options on narrow screens). Its scroll-margin (app.css) keeps
          # it clear of the sticky button bar.
          "$(document).on('shiny:value', function (e) {",
          "  if (!$(e.target).closest('.practice-feedback').length) return;",
          "  setTimeout(function () {",
          "    var card = e.target.querySelector('.feedback-card');",
          "    if (!card) return;",
          "    var bar = document.querySelector('.practice-actions-bar');",
          "    var limit = window.innerHeight - (bar ? bar.offsetHeight : 0);",
          "    if (card.getBoundingClientRect().bottom <= limit) return;",
          "    var reduce = window.matchMedia('(prefers-reduced-motion: reduce)').matches;",
          "    card.scrollIntoView({ behavior: reduce ? 'auto' : 'smooth', block: 'nearest' });",
          "  }, 50);",
          "});",
          # Tooltips for every element with data-tip (dashboard calendar days,
          # coverage cells, (i) buttons). One shared, fixed-position label
          # instead of native `title`s: those appear late, look like the OS
          # default and never show on touch screens. Mouse: hover; keyboard
          # and touch: focus (a tap focuses the element). Fixed positioning so
          # a scrolling container (the calendar) can't clip it.
          "(function () {",
          "  var tip = null, current = null;",
          "  function place() {",
          "    if (!current || !tip) return;",
          "    var r = current.getBoundingClientRect(), t = tip.getBoundingClientRect();",
          "    var left = Math.min(Math.max(8, r.left + r.width / 2 - t.width / 2), window.innerWidth - t.width - 8);",
          "    var top = r.top - t.height - 8, below = top < 8;",
          "    if (below) top = r.bottom + 8;",
          "    tip.style.left = left + 'px';",
          "    tip.style.top = top + 'px';",
          "    tip.classList.toggle('is-below', below);",
          "  }",
          "  function show(el) {",
          "    if (!tip) {",
          "      tip = document.createElement('div');",
          "      tip.className = 'tiger-tip';",
          "      tip.setAttribute('role', 'tooltip');",
          "      document.body.appendChild(tip);",
          "    }",
          "    current = el;",
          "    tip.textContent = el.getAttribute('data-tip');",
          "    place();",
          "    tip.classList.add('is-visible');",
          "  }",
          "  function hide() {",
          "    current = null;",
          "    if (tip) tip.classList.remove('is-visible');",
          "  }",
          "  document.addEventListener('pointerover', function (e) {",
          "    if (e.pointerType !== 'mouse') return;",
          "    var el = e.target.closest('[data-tip]');",
          "    if (el) show(el);",
          "  });",
          "  document.addEventListener('pointerout', function (e) {",
          "    if (e.pointerType !== 'mouse') return;",
          "    var el = e.target.closest('[data-tip]');",
          "    if (el && !el.contains(e.relatedTarget)) hide();",
          "  });",
          "  document.addEventListener('focusin', function (e) {",
          "    var el = e.target.closest && e.target.closest('[data-tip]');",
          "    if (el) show(el);",
          "  });",
          "  document.addEventListener('focusout', hide);",
          # Follow the element when the page scrolls or shifts (scroll events
          # also fire on layout changes, so hiding on scroll would flicker)
          "  window.addEventListener('scroll', function () {",
          "    if (current && !document.body.contains(current)) hide(); else place();",
          "  }, true);",
          "  window.addEventListener('resize', place);",
          "})();"
        ))
      )
    ),

    # ── Login ─────────────────────────────────────────────────────────────────
    bslib::nav_panel(
      title = div(bsicons::bs_icon("lock-fill"), "Login"),
      value = "login_panel",
      div(
        class = "login-page",
        div(
          class = "login-card",
          # Header with hex logo + title on colored background
          div(
            class = "login-card-header",
            tags$img(
              src = "img_app/tigeR_hex.png",
              height = "90px",
              class = "d-block mx-auto mb-3"
            ),
            tags$h3(class = "fw-bold mb-1", "tigeR"),
            tags$p(
              class = "mb-0 login-subtitle",
              "Statistik & R üben mit individuellem Feedback"
            )
          ),
          # Form area
          div(
            class = "login-card-body",
            # ── Login form (default view) ─────────────────────────────────────
            div(
              id = "login-form-wrap",
              shinyauthr::loginUI(
                id = "login",
                title = NULL,
                user_title = "Benutzername",
                pass_title = "Passwort",
                login_title = "Einloggen",
                error_message = "Ungültiger Benutzername oder Passwort.",
                additional_ui = NULL
              ),
              tags$p(
                class = "login-hint mt-3 mb-0",
                bsicons::bs_icon("info-circle"),
                " Noch kein Konto? ",
                actionLink("show_register", "Registriere"),
                " dich mit dem Semester-Code aus dem Praktikum."
              )
            ),
            # ── Registration form (hidden, toggled in) ────────────────────────
            mod_register_ui("register_1")
          )
        )
      )
    ),

    # ── Start ─────────────────────────────────────────────────────────────────
    bslib::nav_panel(
      title = div(bsicons::bs_icon("house-fill"), "Start"),
      value = "home_panel",
      mod_home_ui("home_1")
    ),

    # ── Üben ──────────────────────────────────────────────────────────────────
    bslib::nav_panel(
      title = div(bsicons::bs_icon("ui-radios"), "Üben"),
      value = "train_panel",
      div(class = "main-content", uiOutput("train_view"))
    ),

    # ── Fortschritt ───────────────────────────────────────────────────────────
    bslib::nav_panel(
      title = div(bsicons::bs_icon("bar-chart-fill"), "Fortschritt"),
      value = "progress_panel",
      mod_dashboard_ui("dashboard_1")
    ),

    # ── FAQ ───────────────────────────────────────────────────────────────────
    bslib::nav_panel(
      title = div(bsicons::bs_icon("question-circle-fill"), "FAQ"),
      value = "faq_panel",
      div(class = "main-content", mod_faq_ui("faq_1"))
    ),

    # ── Spacer + Logout ───────────────────────────────────────────────────────
    bslib::nav_spacer(),
    bslib::nav_item(
      shinyjs::hidden(
        div(
          id = "logout_wrap",
          style = "padding: 6px 0;",
          shinyauthr::logoutUI(
            "logout",
            label = "Ausloggen",
            class = "btn-sm btn-outline-light",
            icon = shiny::icon("right-from-bracket")
          )
        )
      )
    )
  )
}
