#' display_item UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_display_item_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' display_item Server Functions
#'
#' @noRd 
mod_display_item_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_display_item_ui("display_item_1")
    
## To be copied in the server
# mod_display_item_server("display_item_1")
