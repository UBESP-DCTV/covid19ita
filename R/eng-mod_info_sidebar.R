#' info_sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
eng_mod_info_sidebar_ui <- function(id) {
  ns <- NS(id)

  tagList(
    selectInput(
      ns("who"),
      label = HTML(glue::glue("
        --------</br>
        <strong>Here below the data as of </br>
        {as.Date(last_data_update)}</strong>
        (Select area):
      ")),
      choices  = c("Italy", regions()),
      selectize = TRUE,
      selected = "Italy"
    ),

    fluidRow(valueBoxOutput(ns("totalTest"))),
    fluidRow(valueBoxOutput(ns("totalCases"))),
    fluidRow(valueBoxOutput(ns("currentPositives"))),
    fluidRow(valueBoxOutput(ns("intensiveCare"))),
    fluidRow(valueBoxOutput(ns("totalDeath"))),
    fluidRow(valueBoxOutput(ns("totalRecovered")))
  )
}

#' info_sidebar Server Function
#'
#' @noRd
eng_mod_info_sidebar_server <- function(id) {
  callModule(id = id, function(input, output, session) {
    ns <- session$ns

    who <- reactive({
      req(input$who)
    })

    data_to_use <- reactive({
      switch(who(),
        Italy = dpc_covid19_ita_andamento_nazionale,

        dpc_covid19_ita_regioni %>%
          dplyr::filter(.data$denominazione_regione == who())
      ) %>%
      dplyr::filter(.data$data == max(.data$data))
    })


    output$totalTest <- renderValueBox({
      valueBox(subtitle = "Tests performed", icon = icon("info-circle"),
        color = "aqua",# fill = TRUE,
        value = data_to_use()[["tamponi"]]
      )
    })

    output$totalCases <- renderValueBox({
      valueBox(subtitle = "Confirmed cases", icon = icon("info-circle"),
        color = "purple",# fill = TRUE,
        value = data_to_use()[["totale_casi"]]
      )
    })

    output$currentPositives <- renderValueBox({
      valueBox(subtitle = "Active cases", icon = icon("info-circle"),
        color = "orange",# fill = TRUE,
        value = data_to_use()[["totale_positivi"]]
      )
    })

    output$intensiveCare <- renderValueBox({
      valueBox(subtitle = "In the ICU", icon = icon("info-circle"),
        color = "fuchsia",
        value = data_to_use()[["terapia_intensiva"]]
      )
    })

    output$totalDeath <- renderValueBox({
      valueBox(subtitle = "Deaths", icon = icon("info-circle"),
        color = "red",
        value = data_to_use()[["deceduti"]]
      )
    })

    output$totalRecovered <- renderValueBox({
      valueBox(subtitle = "Recovered", icon = icon("info-circle"),
        color = "green",
        value = data_to_use()[["dimessi_guariti"]]
      )
    })

  })
}

## To be copied in the UI
# mod_info_sidebar_ui("info_sidebar_ui_1")

## To be copied in the server
# callModule(mod_info_sidebar_server, "info_sidebar_ui_1")

