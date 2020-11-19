library(shiny)

golem::detach_all_attached()
golem::document_and_reload()

ui <- mod_tsicuve_ui("tsicuve")

server <- function(input, output, session){
  mod_tsicuve_server("tsicuve")
}

shinyApp(ui, server)
