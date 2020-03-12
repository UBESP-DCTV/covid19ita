library(shiny)

golem::detach_all_attached()
golem::document_and_reload()

ui <- mod_info_header_ui("info")

server <- function(input,output,session){
    # mod_ts_ita_server("ts", "cum")
    # mod_ts_reg_server("ts", "inc", color_var = "measure")
    mod_info_header_server("info")
}

shinyApp(ui, server)
