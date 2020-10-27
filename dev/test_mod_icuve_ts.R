library(shiny)

golem::detach_all_attached()
golem::document_and_reload()

ui <- mod_icuve_ts_ui("icuve_ts")

server <- function(input, output, session){
  mod_icuve_ts_server("icuve_ts")
}

shinyApp(ui, server)
