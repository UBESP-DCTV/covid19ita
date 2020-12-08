library(shiny)

golem::detach_all_attached()
golem::document_and_reload()

ui <- mod_reg_tsicuve_nocritica_ui("sitrep")

server <- function(input, output, session){
  mod_reg_tsicuve_nocritica_server("sitrep")
}

shinyApp(ui, server)
