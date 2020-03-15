library(shiny)

golem::detach_all_attached()
golem::document_and_reload()

ui <- mod_ind_ita_ui("test_1")

server <- function(input,output,session){
    mod_ind_ita_server("test_1")
}

shinyApp(ui, server)
