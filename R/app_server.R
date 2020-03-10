#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
    mod_ts_ita_server("ts_cum", "cum")
    mod_ts_ita_server("ts_inc", "inc")
}
