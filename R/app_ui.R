#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_navbar(
      title = shiny::tags$b("PsyBSc2 — tigeR"),
      id = "tabs", # must add id here to add/remove tabs in server
      collapsible = TRUE,
      fluid = TRUE,
      theme = bslib::bs_theme(
        bootswatch = "zephyr",
        primary = "#285f8a",
        success = "#285f8a",
        danger = "#D81B60",
        `navbar-bg`= "#285f8a",
      ),
      header = tags$head(shinyjs::inlineCSS(
        list(
          ".default_answer" = "color: black",
          ".correct_answer_txt" = "color: #1E88E5; font-weight: 500;",
          ".incorrect_answer_txt" = "color: #D81B60; font-weight: 500;",
          ".skip_answer_txt" = "color: #FFA000; font-weight: 500;",
          ".skip_answer_img" = "outline: 2px solid #FFA000", # Use outline instead of border
          ".correct_answer_img" = "outline: 2px solid #1E88E5", # Use outline instead of border
          ".incorrect_answer_img" = "outline: 2px solid #D81B60", # Use outline instead of border
          ".label_img img" = "outline: 2px solid #1E88E5", # Apply outline to img elements inside labels
          ".center" = "display: flex; justify-content: center;",
          ".value-box-showcase"  = "overflow: hidden;"
        )
      ), tags$style("
    ul.nav-pills{
      display: flex !important;
      justify-content: center !important;
    }
    ")),

      # login tab to be rendered on launch (hiding the other tabs)
      bslib::nav_panel(
        title = "Login",
        value = "login_panel",
        icon = bsicons::bs_icon("lock-fill", size = 15),
        loginUI(
          id = "login",
          title = "Login",
          user_title = "Benutzername",
          pass_title = "Passwort",
          login_title = "Einloggen",
          error_message = "Invalider Benutzername oder Passwort!",
          additional_ui = tagList(
            tags$p(
              HTML(
                "Bitte melde dich mit deinem im R-Praktikum erhaltenen Benutzernamen und Passwort an.
                Wenn du deinen Benutzernamen/Passwort vergessen haben solltest, wende dich an
                <a href='mailto:beitner@psych.uni-frankfurt.de'>Julia Beitner</a>,
                um einen neuen Zugang zu erhalten. Falls du die App einfach nur probieren möchtest, melde dich mit der unten stehenden Benutzerkennung an."
              ),
              align = "justify"
            ),
            tags$p(
              HTML(
                knitr::kable(
                  data.frame(
                    Benutzername = "test",
                    Passwort = "test123"
                  ),
                  format = "html",
                  col.names = c("Benutzername", "Passwort"),
                  table.attr = "style='width:100%;'"
                )
              ),
              class = "center"
            )
          )
        )
      ),

      # Home Panel
      bslib::nav_panel(
        id = "home_test",
        title = "Home",
        value = "home_panel",
        icon = bsicons::bs_icon("house-fill", size = 15),
        mod_home_ui("home_1")
      ),

      # Training Panel
      bslib::nav_panel(
        title = "Üben",
        value = "train_panel",
        id = "train",
        icon = bsicons::bs_icon("ui-radios", size = 15),
        bslib::navset_pill(
          id = "navset_train",
          tab(
            value = "auswahl",
            title = bslib::tooltip(
              span(
                HTML("Auswahlbereich &nbsp;"),
                bsicons::bs_icon("info-circle")
              ),
              "Hier wählst du Themengebiete aus, aus denen du üben möchtest.",
              placement = "bottom",
            ),
            fluidRow(mod_select_item_ui("select_item_1"))
          ),
          tab(
            value = "item",
            title = bslib::tooltip(
              span(
                HTML("Übungsbereich &nbsp;"),
                bsicons::bs_icon("info-circle")
              ),
              "Hier kannst du die Aufgaben beantworten.",
              placement = "bottom",
            ),
            fluidRow(
              col_1(),
              col_10(
                bslib::card(
                  bslib::card_header(tags$h6(tags$b(textOutput("cardheader_train")))),
                  mod_display_item_ui("display_item_1"),
                  mod_check_item_ui("check_item_1")
                )
              ),
              col_1()
            )
          )
        )
      ),

      # Progress panel
      bslib::nav_panel(
        title = "Fortschritt",
        value = "progress_panel",
        icon = bsicons::bs_icon("check-circle-fill", size = 15),
        fluidRow(
        col_1(),
        col_10(mod_progress_dashboard_ui("progress_dashboard_1")),
        col_1()
        )
      ),

    bslib::nav_panel(
      title = "Datenset",
      value = "data_panel",
      icon = bsicons::bs_icon("database-fill"),
      fluidRow(
        col_1(),
        col_12(
          h4("Download"),
          div("Um die R Aufgaben lösen zu können, benötigst du einen von uns bereit gestellten Datensatz. Diesen Datensatz haben wir extra für tigeR simuliert.
                 Das heißt, die Daten sind ausgedacht und nicht echt. Mithilfe dieses Datensatzes kannst du dann die für die Fragen erforderlichen Analysen
                 durchführen und so deine R-Fähigkeiten üben. Der Datensatz kann mittels folgendem Befehl heruntergeladen werden:",
              tags$code('load(url("https://pandar.netlify.app/daten/df_tiger.rda"))')),
        ),
        col_1()
      ),
      fluidRow(
        #col_4(downloadButton("download", label = "Datensatz herunterladen")),
        rep_br(2),
        tags$p("Folgende Pakete werden empfohlen:", code("car"), code("WebPower"), "."),
        div(
          h4("Codebuch"),
         "Der vorliegende Datensatz",
         tags$code("df_tiger"),
         "befasst sich mit den Schlafgewohnheiten, der akademischen Leistung und den Lebensgewohnheiten von Studierenden.
         Dazu wurden weitere verschiedene Aspekte erhoben, wie sportliche Aktivitäten und generelles Wohlbefinden.
         Hier ist eine Übersicht der Variablen."
         )
        ),
      fluidRow(
        div(
          tags$table(
            tags$tr(
              tags$th("Variable"),
              tags$th("Beschreibung")
            ),
            tags$tr(
              tags$td(code("alter")),
              tags$td("Das Alter der Studierenden.")
            ),
            tags$tr(
              tags$td(code("alleineSchlafen")),
              tags$td("Informationen darüber, ob die Studierenden alleine schlafen, mit einem Partner, mit Kind, oder mit Partner und Kind.")
            ),
            tags$tr(
              tags$td(code("schlafumgebung")),
              tags$td("Beschreibt die Schlafumgebung als ruhig, laut oder gemischt.")
            ),
            tags$tr(
              tags$td(code("schlafstoerungen")),
              tags$td("Gibt an, ob die Studierenden unter spezifischen Schlafstörungen (Insomnie oder Schlafapnoe) leiden oder nicht.")
            ),
            tags$tr(
              tags$td(code("schlafqualitaet")),
              tags$td("Eine ordinal skalierte Bewertung der Schlafqualität von 1 (sehr schlecht) bis 5 (sehr gut).")
            ),
            tags$tr(
              tags$td(code("schlafdauer")),
              tags$td("Die durchschnittliche Schlafdauer pro Nacht in Stunden.")
            ),
            tags$tr(
              tags$td(code("durchschnittsnote")),
              tags$td("Die durchschnittliche Note, auf einer Skala von 1.0 (sehr gut) bis 5.0 (ungenügend).")
            ),
            tags$tr(
              tags$td(code("generellesWohlbefinden")),
              tags$td("Eine mittlere Bewertung des generellen Wohlbefindens von 1 (sehr niedrig) bis 7 (sehr hoch).")
            ),
            tags$tr(
              tags$td(code("sportlicheAktivitaet")),
              tags$td("Die Anzahl der Minuten pro Woche, die für sportliche Aktivitäten aufgewendet wird.")
            ), class = "center"
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "shinytigeR"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
