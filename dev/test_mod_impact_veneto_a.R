library(shiny)

golem::detach_all_attached()
golem::document_and_reload()

ui <- mod_impact_veneto_a_ui("test_1", "A")

server <- function(input,output,session){
    mod_impact_veneto_a_server("test_1")
}

shinyApp(ui, server)
