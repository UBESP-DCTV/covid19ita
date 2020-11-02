library(shiny)

golem::detach_all_attached()
golem::document_and_reload()

ui <- mod_icuve_sitrep_ui("sitrep")

server <- function(input, output, session){
  mod_icuve_sitrep_server("sitrep")
}

shinyApp(ui, server)
