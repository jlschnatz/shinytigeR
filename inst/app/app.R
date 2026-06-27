if (!isNamespaceLoaded("shinytigeR")) library(shinytigeR)

shiny::addResourcePath("css",      system.file("app/www/css",      package = "shinytigeR"))
shiny::addResourcePath("img",      system.file("app/www/img",      package = "shinytigeR"))
shiny::addResourcePath("img_item", system.file("app/www/img_item", package = "shinytigeR"))

shiny::shinyApp(ui = app_ui(), server = app_server)
