#' icuve_sitrep UI Function
#'
#' Situation report for ICU in Veneto
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS fluidRow fluidPage
#' @importFrom shinydashboard box
mod_icuve_sitrep_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    fluidRow(
      h3(HTML("Report regionale (Veneto)")),
      column(12,
        shiny::selectInput(
          ns("whichInfoReg"), "(De-)selezionare le variabili da mostrare",
          choices = list(
            "CoViD-19" = list(
              "Deaths" = "covid_dead",
              "New" = "covid_new",
              "Discharged" = "covid_discharged",
              "Beds occupied" = "covid_occupied",
              "Beds variation" = "covid_variation"
            ),
            "Non-CoViD-19" = list(
              "Beds occupied" = "other_occupied"
            ),
            "Overall" = list(
              "Free beds" = "overall_free",
              "Beds occupied" = "overall_occupied",
              "Total number of beds" = "overall_total"
            )
          ),
          multiple = TRUE,
          selected = c(
            "covid_occupied", "other_occupied", "overall_occupied",
            "overall_total"
          ),
          width = "100%"
        )
      ),
      box(
        width = 12,
        plotly::plotlyOutput(ns("gg_icuve_sitrep")),
        title = "Dettaglio regionale della situazione corrente (punti) e stima andamento a 15 giorni (linee) dei posti letto nelle terapie intensive venete.",
        footer = "Linee orizzontali tratteggiate a 400 (rosso) e 500 (nero) posti letto."
      )
    ),
    fluidRow(
      h3(HTML("Report provinciale (Veneto)")),
      column(6,
        shiny::selectInput(
          ns("whichProvince"), "(De-)selezionare le province di interesse",
          choices = "PROVINCESTOSHOW",
          multiple = TRUE,
          selected = NULL,
          width = "100%"
        )
      ),
      column(6,
        shiny::selectInput(
          ns("whichInfoProv"), "(De-)selezionare le variabili da mostrare",
          choices = "INFOTOSHOW",
          multiple = TRUE,
          selected = NULL,
          width = "100%"
        )
      ),
      box(
        width = 12,
        plotly::plotlyOutput(ns("gg_icuve_sitrep_prov")),
        title = "Dettaglio provinciale della situazione corrente (punti) e stima andamento a 15 giorni (linee) dei posti letto nelle terapie intensive venete.",
        footer = "Linee orizzontali tratteggiate a 400 (rosso) e 500 (nero) posti letto."
      )
    ),
    fluidRow(
      h3(HTML("Report per centro (Veneto)")),
      column(6,
        shiny::selectInput(
          ns("whichCentre"), "Selezionare il centro di interesse",
          choices = "CENTRESTOSHOW",
          selected = NULL,
          width = "100%"
        )
      ),
      column(6,
        shiny::selectInput(
          ns("whichInfoCntr"), "(De-)selezionare le variabili da mostrare",
          choices = "INFOTOSHOW",
          multiple = TRUE,
          selected = NULL,
          width = "100%"
        )
      ),
      box(
        width = 12,
        plotly::plotlyOutput(ns("gg_icuve_sitrep_centre")),
        title = "Dettaglio per centro della situazione corrente (punti) e stima andamento a 15 giorni (linee) dei posti letto nelle terapie intensive venete.",
        footer = "Linee orizzontali tratteggiate a 400 (rosso) e 500 (nero) posti letto."
      )
    )
  )
}

#' icuve_sitrep Server Function
#'
#' @import ggplot2
#' @noRd
mod_icuve_sitrep_server <- function(id) {

  stopifnot(`package {covid19.icuve} required for this function` =
              requireNamespace("covid19.icuve"))

  icuve_sitrep <- covid19.icuve::fetch_gsheet()


  callModule(id = id, function(input, output, session) {
    ns <- session$ns

    siterep_gg <- reactive({
      which_info_reg <- req(input$whichInfoReg)

      icuve_sitrep_long <- sitrep2long(icuve_sitrep, which_info_reg)
      pred_db <- pred_siterep(icuve_sitrep_long)
      gg_siterep(icuve_sitrep_long, pred_db)
    })

    output$gg_icuve_sitrep <- renderPlotly(
      plotly::ggplotly(siterep_gg()) %>%
        plotly::config(modeBarButtonsToRemove = c(
          "zoomIn2d", "zoomOut2d", "pan2d", "select2d", "lasso2d")) %>%
        plotly::config(displaylogo = FALSE)
    )
  })
}

## To be copied in the UI
# mod_icuve_sitrep_ui("sitrep")

## To be copied in the server
# mod_icuve_sitrep_server("sitrep")

