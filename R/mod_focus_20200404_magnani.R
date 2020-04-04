#' focus_20200404_magnani UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_focus_20200404_magnani_ui <- function(id){
  ns <- NS(id)
  fluidPage(

  )
}

#' focus_20200404_magnani Server Function
#'
#' @noRd
mod_focus_20200404_magnani_server <- function(id) {
  callModule(id = id, function(input, output, session) {
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_focus_20200404_magnani_ui("magnani_1")

## To be copied in the server
# mod_focus_20200404_magnani_server("magnani_1")

