#' focus_20200323_picco UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_focus_20200323_picco_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' focus_20200323_picco Server Function
#'
#' @noRd
mod_focus_20200323_picco_server <- function(input, output, session){
  ns <- session$ns

}

## To be copied in the UI
# mod_focus_20200323_picco_ui("focus_20200323_picco_ui_1")

## To be copied in the server
# callModule(mod_focus_20200323_picco_server, "focus_20200323_picco_ui_1")

