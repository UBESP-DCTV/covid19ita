library(shiny)

golem::detach_all_attached()
golem::document_and_reload()

ui <- mod_sir_models_ui("test")

server <- function(input,output,session){
  mod_sir_models_server("test")
}

shinyApp(ui, server)
