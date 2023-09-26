#' select_item UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_select_item_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' select_item Server Functions
#'
#' @noRd 
mod_select_item_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_select_item_ui("select_item_1")
    
## To be copied in the server
# mod_select_item_server("select_item_1")
