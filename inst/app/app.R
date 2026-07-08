if (!isNamespaceLoaded("shinytigeR")) {
  library(shinytigeR)
}

shiny::shinyApp(ui = app_ui(), server = app_server)
