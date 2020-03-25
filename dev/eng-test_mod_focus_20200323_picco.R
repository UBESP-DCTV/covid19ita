library(shiny)

golem::detach_all_attached()
golem::document_and_reload()

ui <- eng_mod_focus_20200323_picco_ui("test")

server <- function(input,output,session){
  eng_mod_focus_20200323_picco_server("test")
}

shinyApp(ui, server)
