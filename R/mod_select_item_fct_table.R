#' @title title
#' @param input description
#' @param id description
#' @param data_handler description
#' @param data_item description
#' @description
#' A short description...
#' @return description
update_bool_table <- function(input, id, data_handler, data_item) {
  shiny::observeEvent(
    eventExpr = input[[id]],
    handlerExpr = {
      row_col_vec <- as.integer(gsub("\\D", "", unlist(strsplit(id, "_"))))
      data_handler$data_bool[rbind(row_col_vec)] <- input[[id]]
      index_matrix <- which(data_handler$data_bool == TRUE, arr.ind = TRUE, useNames = FALSE)
      id_cd <- rownames(data_handler$data_bool)[index_matrix[, 1L]]
      id_ti <- colnames(data_handler$data_bool)[index_matrix[, 2L]]
      id_rows_filter <- which(with(data_item, paste(learning_area, type_item)) %in% paste(id_cd, id_ti))
      data_handler$data_item <- data_item[id_rows_filter, ]
    }
  )
}
  
#' @title title
#' @param id description
#' @param checkbox_data description
#' @param session description
#' @description
#' A short description...
#' @return description
observe_margins <- function(input, id, checkbox_data, session) {
  regex <- 'id="[a-zA-Z0-9_]+-([a-zA-Z0-9_-]+)"'
  if (grepl("row", id)) {
    pos <- grep(id, rownames(checkbox_data))
    cellids <- sapply(regmatches(checkbox_data[pos, ], gregexpr(regex, checkbox_data[pos, ])), function(x) sub(regex, "\\1", x))

  } else if (grepl("col", id)) {
    pos <- grep(id, colnames(checkbox_data))
    cellids <- sapply(regmatches(checkbox_data[, pos], gregexpr(regex, checkbox_data[, pos])), function(x) sub(regex, "\\1", x))
  } 

  shiny::observeEvent(
    eventExpr = input[[id]],
    handlerExpr = lapply(cellids, function(x) shiny::updateCheckboxInput(session, x, value = input[[id]]))
  )
}