library(shiny)

golem::detach_all_attached()
golem::document_and_reload()

ui <- mod_ts_ita_ui("ts", title = "title")

server <- function(input, output ,session){
    mod_ts_ita_server("ts", "cum")
    # mod_ts_ita_server("ts", "inc")
}

shinyApp(ui, server)
