library(shiny)

golem::detach_all_attached()
golem::document_and_reload()

ui <- mod_impact_veneto_b_ui("test_1", "gam")

server <- function(input,output,session){
    mod_impact_veneto_b_server("test_1", "gam")
    # mod_impact_veneto_b_server("test_1", "loess")
}

shinyApp(ui, server)
