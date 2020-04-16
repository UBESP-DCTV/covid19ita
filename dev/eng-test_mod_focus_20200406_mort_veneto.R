library(shiny)

golem::detach_all_attached()
golem::document_and_reload()

ui <- eng_mod_0406_mort_ve_ui("test_1")

server <- function(input,output,session){
  eng_mod_0406_mort_ve_server("test_1")
}

shinyApp(ui, server)
