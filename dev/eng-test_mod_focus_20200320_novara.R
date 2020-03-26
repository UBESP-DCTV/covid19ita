library(shiny)

golem::detach_all_attached()
golem::document_and_reload()

ui <- eng_mod_focus_20200320_novara_ui("test_1")

server <- function(input,output,session){
  eng_mod_focus_20200320_novara_server("test_1")
}

shinyApp(ui, server)
