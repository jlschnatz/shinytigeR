#' login_author UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_login_author_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' login_author Server Functions
#'
#' @noRd 
mod_login_author_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_login_author_ui("login_author_1")
    
## To be copied in the server
# mod_login_author_server("login_author_1")
