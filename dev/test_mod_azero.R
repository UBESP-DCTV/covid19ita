library(shiny)

golem::detach_all_attached()
golem::document_and_reload(export_all = TRUE)

ui <- mod_azero_ui("test_1")

server <- function(input,output,session){
    mod_azero_server("test_1")
}

shinyApp(ui, server)
