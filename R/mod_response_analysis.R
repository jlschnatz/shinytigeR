#' response_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_response_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' response_analysis Server Functions
#'
#' @noRd 
mod_response_analysis_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_response_analysis_ui("response_analysis_1")
    
## To be copied in the server
# mod_response_analysis_server("response_analysis_1")
