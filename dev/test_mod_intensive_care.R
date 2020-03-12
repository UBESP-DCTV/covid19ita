library(shiny)

golem::detach_all_attached()
golem::document_and_reload()

ui <- mod_intensive_care_ui("test_1")

server <- function(input,output,session){
    mod_intensive_care_server("test_1")
}

shinyApp(ui, server)
