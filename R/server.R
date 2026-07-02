#' App server
#' @param input,output,session Shiny session objects.
#' @export
app_server <- function(input, output, session) {
  # ── Auth ────────────────────────────────────────────────────────────────────
  user_base <- db_get_credentials()

  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = "user_name",
    pwd_col = "password_hashed",
    sodium_hashed = TRUE,
    reload_on_logout = TRUE,
    log_out = reactive(logout_init())
  )

  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )

  # ── Static item data (loaded once) ────────────────────────────────────────
  data_item <- db_get_items()

  # ── Post-login setup — runs exactly once per authenticated session ─────────
  observeEvent(credentials()$user_auth, {
    if (!isTRUE(credentials()$user_auth)) {
      return()
    }

    # Reveal authenticated tabs; hide login; show logout button
    protected <- c("home_panel", "train_panel", "progress_panel")
    lapply(protected, function(p) {
      shinyjs::show(selector = sprintf('.navbar-nav a[data-value="%s"]', p))
    })
    shinyjs::hide(selector = '.navbar-nav a[data-value="login_panel"]')
    shinyjs::show("logout_wrap")
    bslib::nav_select("main_tabs", "home_panel")

    # ── Shared state ─────────────────────────────────────────────────────────
    # practice_ids: NULL = show selector; integer vector = show practice
    practice_ids <- reactiveVal(NULL)
    # write_trigger: incremented after each confirmed DB write so dashboard
    # knows to re-fetch user data without being tightly coupled to practice
    write_trigger <- reactiveVal(0L)

    # ── Train view — switches between selector and practice ───────────────────
    output$train_view <- renderUI({
      if (is.null(practice_ids())) {
        mod_selector_ui("selector_1", data_item)
      } else {
        mod_practice_ui("practice_1")
      }
    })

    # ── Module wiring ─────────────────────────────────────────────────────────
    app_session <- session
    mod_home_server(
      "home_1",
      data_item = data_item,
      credentials = credentials,
      go_train = function() {
        bslib::nav_select("main_tabs", "train_panel", session = app_session)
      }
    )

    mod_selector_server(
      "selector_1",
      data_item = data_item,
      practice_ids = practice_ids,
      credentials = credentials,
      write_trigger = write_trigger
    )

    mod_practice_server(
      "practice_1",
      data_item = data_item,
      practice_ids = practice_ids,
      credentials = credentials,
      write_trigger = write_trigger
    )

    mod_dashboard_server(
      "dashboard_1",
      data_item = data_item,
      credentials = credentials,
      write_trigger = write_trigger
    )
  })
}
