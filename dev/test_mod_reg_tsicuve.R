library(shiny)

golem::detach_all_attached()
golem::document_and_reload()

ui <- mod_reg_tsicuve_ui("tsicuve")

server <- function(input, output, session){
  mod_reg_tsicuve_server("tsicuve")
}

shinyApp(ui, server)
