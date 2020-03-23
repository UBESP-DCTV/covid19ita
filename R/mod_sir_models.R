#' sir_models UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sir_models_ui <- function(id){
  ns <- NS(id)

  tagList(
    textOutput(ns("foo"))
  )
}

#' sir_models Server Function
#'
#' @noRd
mod_sir_models_server <- function(id) {
  callModule(id = id, function(input, output, session) {
    ns <- session$ns

    output$foo <- renderText("Bar.")
  })
}

## To be copied in the UI
# mod_sir_models_ui("sir_1")

## To be copied in the server
# mod_sir_models_server("sir_1")

