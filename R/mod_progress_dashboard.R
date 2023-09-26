#' progress_dashboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_progress_dashboard_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' progress_dashboard Server Functions
#'
#' @noRd 
mod_progress_dashboard_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_progress_dashboard_ui("progress_dashboard_1")
    
## To be copied in the server
# mod_progress_dashboard_server("progress_dashboard_1")
