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
      tags$head(
        tags$link(rel = "stylesheet", href = "css/app.css"),
        tags$link(rel = "icon", type = "image/png", href = "img_app/favicon.png"),
        tags$script(shiny::HTML(
          "Shiny.addCustomMessageHandler('mathjax_typeset', function(_) {",
          "  if (window.MathJax) MathJax.Hub.Queue(['Typeset', MathJax.Hub]);",
          "});"
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
