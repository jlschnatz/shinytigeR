mod_register_ui <- function(id) {
  ns <- NS(id)
  div(
    id = "register-form-wrap",
    style = "display:none;",
    tags$h5(
      class = "fw-semibold mb-3 mt-1",
      "Konto erstellen"
    ),
    textInput(
      ns("reg_username"),
      label = "Benutzername",
      placeholder = "z.B. max.mustermann"
    ),
    passwordInput(ns("reg_password"), label = "Passwort"),
    passwordInput(ns("reg_password2"), label = "Passwort bestätigen"),
    passwordInput(
      ns("reg_code"),
      label = "Semester-Code",
      placeholder = "Im Praktikum erhalten"
    ),
    uiOutput(ns("reg_feedback")),
    div(
      class = "d-grid mt-3",
      actionButton(
        ns("reg_submit"),
        div(bsicons::bs_icon("person-plus-fill"), tags$b("Konto erstellen")),
        class = "btn btn-primary btn-lg"
      )
    ),
    tags$p(
      class = "text-center mt-3 mb-0",
      style = "font-size: 0.85rem;",
      actionLink(ns("show_login"), "← Zurück zum Login")
    )
  )
}

mod_register_server <- function(id, on_show_login) {
  moduleServer(id, function(input, output, session) {
    attempts <- reactiveVal(0L)
    locked   <- reactiveVal(FALSE)

    observeEvent(input$show_login, {
      on_show_login()
    })

    observeEvent(input$reg_submit, {
      if (locked()) {
        output$reg_feedback <- renderUI(reg_msg(
          "Zu viele fehlgeschlagene Versuche. Bitte Seite neu laden.",
          type = "error"
        ))
        return()
      }

      Sys.sleep(REG_ATTEMPT_DELAY_S)

      un  <- trimws(input$reg_username)
      pw  <- input$reg_password
      pw2 <- input$reg_password2

      # Validate all fields filled
      if (!nzchar(un) || !nzchar(pw) || !nzchar(pw2) || !nzchar(input$reg_code)) {
        output$reg_feedback <- renderUI(reg_msg(
          "Bitte alle Felder ausfüllen.",
          type = "error"
        ))
        return()
      }

      # Validate username pattern
      if (!grepl(REG_USERNAME_PATTERN, un)) {
        output$reg_feedback <- renderUI(reg_msg(
          "Benutzername muss 3–30 Zeichen lang sein und darf nur Buchstaben, Ziffern, Punkte, Bindestriche und Unterstriche enthalten.",
          type = "error"
        ))
        return()
      }

      # Validate password length
      if (nchar(pw) < REG_PW_MIN_LENGTH) {
        output$reg_feedback <- renderUI(reg_msg(
          sprintf("Passwort muss mindestens %d Zeichen lang sein.", REG_PW_MIN_LENGTH),
          type = "error"
        ))
        return()
      }

      # Validate passwords match
      if (!identical(pw, pw2)) {
        output$reg_feedback <- renderUI(reg_msg(
          "Passwörter stimmen nicht überein.",
          type = "error"
        ))
        return()
      }

      # Check semester code
      expected <- Sys.getenv("TIGER_REG_CODE")
      if (!nzchar(expected)) {
        output$reg_feedback <- renderUI(reg_msg(
          "Registrierung ist derzeit nicht verfügbar.",
          type = "error"
        ))
        return()
      }
      if (!identical(input$reg_code, expected)) {
        new_attempts <- attempts() + 1L
        attempts(new_attempts)
        if (new_attempts >= REG_MAX_ATTEMPTS) locked(TRUE)
        output$reg_feedback <- renderUI(reg_msg(
          if (locked()) {
            "Ungültiger Semester-Code. Zu viele Versuche — bitte Seite neu laden."
          } else {
            sprintf(
              "Ungültiger Semester-Code. (%d von %d Versuchen)",
              new_attempts, REG_MAX_ATTEMPTS
            )
          },
          type = "error"
        ))
        return()
      }

      # Write to DB
      tryCatch(
        {
          db_register_user(un, pw)
          output$reg_feedback <- renderUI(reg_msg(
            "Konto erfolgreich erstellt! Du wirst zum Login weitergeleitet…",
            type = "success"
          ))
          # Reload page so loginServer picks up the new credential
          shinyjs::delay(1800, session$reload())
        },
        error = function(e) {
          if (conditionMessage(e) == "username_taken") {
            # Not a security failure — don't count toward lockout
            output$reg_feedback <- renderUI(reg_msg(
              "Dieser Benutzername ist bereits vergeben. Bitte wähle einen anderen.",
              type = "error"
            ))
          } else {
            output$reg_feedback <- renderUI(reg_msg(
              "Fehler beim Erstellen des Kontos. Bitte versuche es erneut.",
              type = "error"
            ))
            attempts(attempts() + 1L)
            if (attempts() >= REG_MAX_ATTEMPTS) locked(TRUE)
          }
        }
      )
    })
  })
}

reg_msg <- function(text, type = c("error", "success")) {
  type <- match.arg(type)
  cls <- if (type == "error") "reg-error" else "reg-success"
  icon_name <- if (type == "error") "exclamation-circle-fill" else "check-circle-fill"
  div(
    class = cls,
    bsicons::bs_icon(icon_name),
    " ",
    text
  )
}
