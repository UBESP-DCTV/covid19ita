library(shiny)

golem::detach_all_attached()
golem::document_and_reload()

ui <- eng-mod_0325_hosp_ui("test_1")

server <- function(input,output,session){
    eng-mod_0325_hosp_server("test_1")
}

shinyApp(ui, server)
