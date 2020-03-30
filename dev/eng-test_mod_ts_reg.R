library(shiny)

golem::detach_all_attached()
golem::document_and_reload()

ui <- eng_mod_ts_reg_ui("ts")

server <- function(input,output,session){
    # mod_ts_ita_server("ts", "cum")
    # mod_ts_reg_server("ts", "inc", color_var = "measure")
    eng_mod_ts_reg_server("ts", "inc", color_var = "region")
}

shinyApp(ui, server)
