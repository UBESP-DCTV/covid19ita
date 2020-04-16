library(shiny)

golem::detach_all_attached()
golem::document_and_reload()

ui <- mod_0323_picco_ui("test")

server <- function(input,output,session){
  mod_0323_picco_server("test")
}

shinyApp(ui, server)
