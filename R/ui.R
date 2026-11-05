#' App UI
#' @return A [bslib::page_navbar()] UI definition.
#' @export
app_ui <- function() {
  bslib::page_navbar(
    title = actionLink(
      inputId = "brand_home",
      label = div(
        class = "d-flex align-items-center gap-2",
        tags$img(src = "logo/tiger_logo_white.svg", height = "26px", alt = ""),
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
      font_scale = 1,
      # Fonts are bundled in www/fonts (@font-face rules in css/fonts.css),
      # not bslib::font_google(): that downloads them from Google when the app
      # starts, which fails silently in a container without internet access
      # and would send students' browsers to Google (GDPR).
      base_font = "'Fira Sans', system-ui, -apple-system, 'Segoe UI', sans-serif",
      # Fira Mono, not Fira Code: its ligatures would show R's `<-` as an arrow
      code_font = "'Fira Mono', SFMono-Regular, Menlo, Consolas, monospace"
    ),
    footer = app_footer(),
    header = tagList(
      shinyjs::useShinyjs(),
      rclipboard::rclipboardSetup(),
      tags$head(
        # Cache-busting query: without it browsers keep serving a stale
        # app.css after it changes, so new markup renders unstyled. runApp()
        # sets the working directory to the app dir (inst/app, or the
        # installed copy via run_app()), so this path resolves in both.
        tags$link(rel = "stylesheet", href = css_href("www/css/fonts.css", "css/fonts.css")),
        tags$link(rel = "stylesheet", href = css_href("www/css/app.css", "css/app.css")),
        # The logo as favicon; the PNG (180 px, rendered from the SVG) for
        # browsers without SVG favicons and for iOS home-screen icons
        tags$link(rel = "icon", type = "image/svg+xml", href = "logo/tiger_logo.svg"),
        tags$link(rel = "icon", type = "image/png", href = "logo/favicon.png"),
        tags$link(rel = "apple-touch-icon", href = "logo/favicon.png"),
        # The page scripts (inst/app/www/js/, served like css/), cache-busted
        # like the stylesheets
        tags$script(src = css_href("www/js/practice.js", "js/practice.js")),
        tags$script(src = css_href("www/js/dashboard.js", "js/dashboard.js")),
        tags$script(src = css_href("www/js/tooltips.js", "js/tooltips.js"))
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
          # Header with the logo + title on colored background
          div(
            class = "login-card-header",
            tags$img(
              src = "logo/tiger_logo_white.svg",
              height = "80px",
              alt = "",
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
                " dich mit dem ",
                # kept on one line: it broke at the hyphen
                tags$span(class = "text-nowrap", "Semester-Code"),
                " aus dem Praktikum."
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

    # ── Impressum & Datenschutz ───────────────────────────────────────────────
    # Public like the FAQ, not a tab: reachable via the footer link on every
    # page (app_footer(), R/legal.R); its nav link is hidden in app.css
    bslib::nav_panel(
      title = "Impressum & Datenschutz",
      value = "legal_panel",
      div(class = "main-content", legal_page())
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
