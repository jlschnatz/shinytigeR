#' check_item UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_check_item_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' check_item Server Functions
#'
#' @noRd 
mod_check_item_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_check_item_ui("check_item_1")
    
## To be copied in the server
# mod_check_item_server("check_item_1")
