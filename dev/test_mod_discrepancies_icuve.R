golem::detach_all_attached()
golem::document_and_reload(export_all = TRUE)

ui <- mod_discrepancies_icuve_ui("test")

server <- function(input, output, session){
  mod_discrepancies_icuve_server("test")
}

shiny::shinyApp(ui, server)
