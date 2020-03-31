library(shiny)

golem::detach_all_attached()
golem::document_and_reload()

ui <- eng_mod_ts_ita_ui("ts", title = "title")

server <- function(input, output ,session){
  eng_mod_ts_ita_server("ts", "cum")
  # mod_ts_ita_server("ts", "inc")
}

shinyApp(ui, server)
