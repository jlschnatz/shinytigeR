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

  # ── Registration — wired before auth gate so it works pre-login ───────────
  toggle_login_register <- function() {
    shinyjs::runjs("$('#login-form-wrap').toggle(); $('#register-form-wrap').toggle();")
  }

  observeEvent(input$show_register, toggle_login_register())

  mod_register_server("register_1", on_show_login = toggle_login_register)

  # ── Static item data (loaded once) ────────────────────────────────────────
  data_item <- db_get_items()

  # ── Post-login setup — runs exactly once per authenticated session ─────────
  observeEvent(credentials()$user_auth, {
    if (!isTRUE(credentials()$user_auth)) {
      return()
    }

    # Reveal authenticated tabs; hide login; show logout button
    # home_panel is deliberately excluded — it's not a tab, only reachable via
    # the navbar brand/logo link (see input$brand_home below).
    protected <- c("train_panel", "progress_panel", "faq_panel")
    lapply(protected, function(p) {
      shinyjs::show(selector = sprintf('.navbar-nav a[data-value="%s"]', p))
    })
    shinyjs::hide(selector = '.navbar-nav a[data-value="login_panel"]')
    shinyjs::show("logout_wrap")
    bslib::nav_select("main_tabs", "home_panel")

    # ── Shared state ─────────────────────────────────────────────────────────
    # practice_ids: NULL = show selector; integer vector = show practice
    practice_ids <- reactiveVal(NULL)
    # inspect_id: single item ID looked up directly by ID; shows a read-only
    # overview instead of entering the practice queue. Takes priority over
    # practice_ids when both would otherwise apply.
    inspect_id <- reactiveVal(NULL)
    # write_trigger: incremented after each confirmed DB write so dashboard
    # knows to re-fetch user data without being tightly coupled to practice
    write_trigger <- reactiveVal(0L)
    # ability_computed_this_session: latches TRUE once the dashboard's refresh
    # button has been used during this login, so it's never clicked more than
    # once per session even if new data keeps arriving — ability isn't
    # expected to shift within a single sitting. Deliberately NOT set by the
    # silent login catch-up below: that only clears out *stale* data from a
    # prior session, and must not consume this session's one allowed
    # button-triggered recompute for practice the student hasn't done yet.
    ability_computed_this_session <- reactiveVal(FALSE)

    # ── Ability auto-catch-up — silent, runs once at login ──────────────────
    # If the student practiced last session without ever visiting the
    # dashboard (or clicking its refresh button), catch the estimate up now.
    uid <- credentials()$info$user_name
    if (isTRUE(ability_needs_update(uid))) {
      compute_and_save_ability(uid, session$token, data_item)
    }

    # ── Train view — switches between selector, practice, and inspect ─────────
    output$train_view <- renderUI({
      if (!is.null(inspect_id())) {
        mod_inspect_ui("inspect_1")
      } else if (is.null(practice_ids())) {
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

    # Navbar brand/logo click — home_panel has no tab of its own, this is the
    # only way back to it (standard "logo goes home" convention).
    observeEvent(input$brand_home, {
      bslib::nav_select("main_tabs", "home_panel", session = app_session)
    })

    mod_faq_server("faq_1", data_item = data_item)

    mod_selector_server(
      "selector_1",
      data_item = data_item,
      practice_ids = practice_ids,
      inspect_id = inspect_id,
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

    mod_inspect_server(
      "inspect_1",
      data_item = data_item,
      inspect_id = inspect_id
    )

    mod_dashboard_server(
      "dashboard_1",
      data_item = data_item,
      credentials = credentials,
      write_trigger = write_trigger,
      ability_computed_this_session = ability_computed_this_session
    )
  })
}
