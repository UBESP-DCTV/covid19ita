library(shiny)

golem::detach_all_attached()
golem::document_and_reload()

ui <- mod_icuve_static_ui("icuve_static")

server <- function(input, output, session){
  mod_icuve_static_server("icuve_static")
}

shinyApp(ui, server)
