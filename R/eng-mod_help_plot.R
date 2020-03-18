#' help_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
eng_mod_help_plot_ui <- function(id){
  ns <- NS(id)
  actionButton(ns("show"), "How to interact with graphs", icon("question"))
}

#' help_plot Server Function
#'
#' @noRd
eng_mod_help_plot_server <- function(id) {
  callModule(id = id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$show, {
      showNotification(plottly_help_txt,
        duration = 10,
        type = "message"
      )
    })

  })
}

## To be copied in the UI
# mod_help_plot_ui("help_plot_ui_1")

## To be copied in the server
# callModule(mod_help_plot_server, "help_plot_ui_1")

