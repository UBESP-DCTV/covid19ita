#' info_header UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_info_header_ui <- function(id) {
  ns <- NS(id)

  tagList(
    selectInput(
      ns("who"), label = glue::glue("Oggi ({as.Date(last_data_update)}) in:"),
      choices  = c("Italia", regions()),
      selected = "Veneto"
    ),

    fluidRow(valueBoxOutput(ns("totalTest"))),
    fluidRow(valueBoxOutput(ns("totalCases"))),
    fluidRow(valueBoxOutput(ns("intensiveCare"))),
    fluidRow(valueBoxOutput(ns("totalDeath"))),
    fluidRow(valueBoxOutput(ns("totalRecovered")))
  )
}

#' info_header Server Function
#'
#' @noRd
mod_info_header_server <- function(id) {
  callModule(id = id, function(input, output, session) {
    ns <- session$ns

    who <- reactive({
      req(input$who)
    })

    data_to_use <- reactive({
      switch(who(),
        Italia = dpc_covid19_ita_andamento_nazionale,

        dpc_covid19_ita_regioni %>%
          dplyr::filter(denominazione_regione == who())
      ) %>%
      dplyr::filter(data == max(.data$data))
    })


    output$totalTest <- renderValueBox({
      valueBox(subtitle = "Tamponi effettuati", icon = icon("info-circle"),
        color = "purple",# fill = TRUE,
        value = data_to_use()[["tamponi"]]
      )
    })

    output$totalCases <- renderValueBox({
      valueBox(subtitle = "Contagiati accertati", icon = icon("info-circle"),
        color = "orange",# fill = TRUE,
        value = data_to_use()[["totale_casi"]]
      )
    })

    output$intensiveCare <- renderValueBox({
      valueBox(subtitle = "In terapia intensiva", icon = icon("info-circle"),
        color = "fuchsia",
        value = data_to_use()[["terapia_intensiva"]]
      )
    })

    output$totalDeath <- renderValueBox({
      valueBox(subtitle = "Decessi totali", icon = icon("info-circle"),
        color = "red",
        value = data_to_use()[["deceduti"]]
      )
    })

    output$totalRecovered <- renderValueBox({
      valueBox(subtitle = "Guariti totali", icon = icon("info-circle"),
        color = "green",
        value = data_to_use()[["dimessi_guariti"]]
      )
    })

  })
}

## To be copied in the UI
# mod_info_header_ui("info_header_ui_1")

## To be copied in the server
# callModule(mod_info_header_server, "info_header_ui_1")

