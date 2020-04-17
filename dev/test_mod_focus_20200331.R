library(shiny)

golem::detach_all_attached()
golem::document_and_reload()

ui <- mod_0331_ui("test_1")

server <- function(input,output,session){
  mod_0331_server("test_1")
}

shinyApp(ui, server)
