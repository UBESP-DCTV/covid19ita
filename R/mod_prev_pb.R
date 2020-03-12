#' prev_pb UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_prev_pb_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' prev_pb Server Function
#'
#' @noRd
mod_prev_pb_server <- function(id) {
  callModule(id = id, function(input, output, session) {
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_prev_pb_ui("prev_pb_ui_1")

## To be copied in the server
# callModule(mod_prev_pb_server, "prev_pb_ui_1")

