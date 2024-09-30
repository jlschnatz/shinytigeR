#' @title Item Selection Module UI
#' @param id Module ID
#' @description
#' Module creates checkbox contigency table `checkbox_table` with learning_area 
#' and type_item as dimensions. It also allows users to specify the number of items
#' to train in the learning session.
mod_select_item_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
        fluidRow(
          shiny::tags$p("Du hast die Möglichkeit, aus dem Fragenpool die Themen auszuwählen, die du filtern und speziell für deine Übungen verwenden möchtest. Hierbei kannst du gezielt jene Themen oder Kategorien selektieren, die deinem individuellen Lernbedarf entsprechen. Zusätzlich dazu, ist es empfehlenswert, die App in regelmäßigen Abständen zu besuchen, da alle zwei Wochen neue Themen und Lerninhalte hinzugefügt werden. Dies bedeutet, dass der Fragenpool kontinuierlich erweitert wird, um dir eine immer breitere und aktuelle Auswahl an Übungsmaterialien zur Verfügung zu stellen.")
        ),
        fluidRow(bslib::layout_columns(
          tagList(
            shiny::div(
              shiny::div(
                DT::DTOutput(ns("checkbox_table")), 
                style = "font-size: 100%; width: 100%; display: inline-block; text-align: left;"
                ),
                style = "text-align: center;"
                )
          ),
          tagList(
            uiOutput(ns("choose_number")),
            column(6, actionButton(
              ns("submit_btn"),
                span(
                  HTML("Weiter zum Übungsbereich"),
                  bsicons::bs_icon("shuffle")
                  ),
              class = "btn-primary"
              ))
          ),
          col_widths = c(8, 3), #height = 800, 
        ))
  )
}

#' @title Item Selection Module Server
#' @param id Module id
#' @param data_item Item data dataframe
mod_select_item_server <- function(id, data_item, credentials) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # datatable options
    dt_options <- list(
      columnDefs = list(
        list(targets = "_all", orderable = FALSE, className = "dt-nowrap", width = "300px", autoWidth = FALSE)
        ),
      dom = 't',
      fixedColumns = FALSE,
      preDrawCallback = DT::JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
      drawCallback = DT::JS('function() { Shiny.bindAll(this.api().table().node()); } ')
    )

    learning_areas <- sort(unique(data_item$learning_area))
    type_item <- unique(data_item$type_item)
    bool_data <- as.data.frame(matrix(FALSE, nrow = length(learning_areas), ncol = length(type_item), dimnames = list(learning_areas, type_item)))

    # rowwise checkboxes
    id_rows <- paste0("row-", seq_along(learning_areas))
    table_rownames <- mapply(
        FUN = function(inputId, label) as.character(shiny::checkboxInput(ns(inputId), label, width = "10px")), 
        inputId = id_rows, label = learning_areas, 
        SIMPLIFY = FALSE, 
        USE.NAMES = FALSE
      ) 
    
    # generate html container for table
    id_cols <- paste0("col-", seq_along(type_item))
    header_cols <- c("allboxes", paste0("col-", seq_along(type_item)))
    table_colnames <- c("Alle auswählen", type_item)
    container <- htmltools::withTags(
      table(
        class = "display",
        thead(
          tr(
          mapply(
            FUN = function(inputId, label, ...) th(shiny::HTML(as.character(shiny::checkboxInput(ns(inputId), label, width = "10%", value = FALSE)))), 
            inputId = header_cols, label = table_colnames,
            SIMPLIFY = FALSE))
          )
        )
      )
    checkbox_data <- apply(
      FUN = function(inputId) as.character(shiny::checkboxInput(ns(inputId), label = NULL, width = "10%", value = FALSE)), 
      X = outer(X = seq_along(learning_areas), Y = seq_along(type_item), FUN = function(i, j) paste0("row", i, "_col", j)), 
      MARGIN = c(1, 2)
    ) 
    rownames(checkbox_data) <- table_rownames
    colnames(checkbox_data) <- id_cols
    # Outputs
    output$checkbox_table <- DT::renderDataTable({
      DT::datatable(
        data = checkbox_data, 
        container = container,
        escape = FALSE,
        select = "none",
        options = dt_options,
        rownames = TRUE,
        style = "default",
        width = "100%",
        height = "100%",
        fillContainer = FALSE
      )
    })

    max_item <- reactive(length(data_handler$data_item$id_item))
    output$choose_number <- renderUI({
      tagList(
        numericInput(ns("n_item"), label = "Bitte wähle die Anzahl an Aufgaben, die du üben möchtest.", value = NULL)
      )
    })

    observeEvent(
      eventExpr = input$submit_btn,
      handlerExpr = {
        if(max_item() == 0) {
          shinyalert::shinyalert(
            title = "Achtung",
            text = "Du musst mindestend ein Aufgabenfeld auswählen.",
            type = "error",
            closeOnEsc = TRUE,
            closeOnClickOutside = FALSE
          )
        } else if (is.na(input$n_item)) {
          shinyalert::shinyalert(
            title = "Achtung",
            text = HTML(paste0("Du musst eine Anzahl an Aufgabe für deine Übungssession ausgewählt. Du kannst maximal ", tags$b(max_item(), "Aufgaben"), "wählen.")),
            type = "error",
            html = TRUE
          )
        } else {
          if(input$n_item > max_item()) {
            shinyalert::shinyalert(
              title = "Stop",
              text = HTML(paste0("Es sind bis jetzt nur ", tags$b(max_item(), "Aufgaben "), "für diesen Auswahl vorhanden.")),
              type = "warning",
              html = TRUE
            )
            shiny::updateNumericInput(session, "n_item", value = max_item())
          } else if (input$n_item <= 0) {
              shinyalert::shinyalert(
              title = "Stop",
              text = paste0("Bitte wähle mindestens eine Aufgabe aus. Du kannst maximal ", tags$b(max_item(), "Aufgaben"), "für diese Auswahl wählen."),
              type = "error",
              html = TRUE
            )
          }
        }
      }
    )

  data_handler <- shiny::reactiveValues(data_bool = bool_data, data_item = data_item)
  margin_ids <- c(id_rows, id_cols)
  
  # update single cells if margins are clicked
  lapply(margin_ids, function(x) observe_margins(input, x, checkbox_data, session))
  shiny::observeEvent(
    eventExpr = input[["allboxes"]],
    handlerExpr = lapply(margin_ids, function(x) shiny::updateCheckboxInput(session, x, value = input[["allboxes"]]))
  )

  all_ids <- regmatches(as.vector(checkbox_data), regexpr('(?<=id\\=")(.*?)(?=")', as.vector(checkbox_data), perl = TRUE))
  all_ids <- sub("([a-zA-Z0-9_]+-)([a-zA-Z0-9_-]+)", "\\2", all_ids)
  lapply(all_ids, function(x) update_bool_table(input, x, data_handler, data_item))
  lapply(
    X = all_ids, 
    FUN = function(inputId, session, input) {
      shiny::observeEvent(
        eventExpr = input[[inputId]],
        handlerExpr = {
          shiny::updateNumericInput(session, "n_item", label = paste0("Bitte wähle die Anzahl an Aufgaben, die du üben möchtest. (max. ", max_item(), ")"), value = ceiling(max_item() / 2))
        }
      )
      }, 
    session = session, 
    input = input
    )

  index_display <- shiny::reactive(safe_sample(as.integer(data_handler$data_item$id_item)))    
    
  # output
  out <- list(
    index_display = index_display,
    submit_btn_value = reactive(input$submit_btn),
    max_item = max_item,
    n_item = reactive(input$n_item)
  )
  return(out)
  })
}
