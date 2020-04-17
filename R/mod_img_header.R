#' img_header UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_img_header_ui <- function(id) {
  ns <- NS(id)
  imageOutput(ns("img"))
}

#' img_header Server Function
#'
#' @noRd
mod_img_header_server <- function(id, img_name) {
  callModule(id = id, function(input, output, session) {
    ns <- session$ns

    output$img <- renderImage({
        list(
          src = app_sys(glue::glue("app/www/{img_name}")),
          contentType = "image/png",
          alt = stringr::str_remove(img_name, "\\.[^\\.]{3,}$")
        )
      }, deleteFile = FALSE)
  })
}

## To be copied in the UI
#> mod_img_header_ui("img_header_ui_1")

## To be copied in the server
#> callModule(mod_img_header_server, "img_header_ui_1")
