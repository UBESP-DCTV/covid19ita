#' tsicuve UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS fluidRow fluidPage
#' @importFrom shinydashboard box
mod_icuve_ts_ui <- function(id){
  ns <- NS(id)

  fluidPage(
    fluidRow(
      box(
        width = 12,
        plotly::plotlyOutput(ns("fig1")),
        title = "Figure 1. Andamento stimato (linea rossa in grassetto,
        l'area rossa indica gli intervalli di confidenza al 95%) delle
        proporzioni di posti letto totali in terapia intensiva occupati
        da pazienti Covid. Andamento osservato (punti blu) fino alla
        data odierna."
      )
    ),
    fluidRow(
      box(
        width = 12,
        plotly::plotlyOutput(ns("fig2")),
        title = "Figure 2. Andamento stimato (linea rossa in grassetto,
        l'area rossa indica gli intervalli di confidenza al 95%) del
        numero di posti letto totali in terapia intensiva occupati
        da pazienti Covid aggiustato per proporzione di tamponi positivi.
        Andamento osservato (punti blu) fino alla data odierna."
      )
    ),
    fluidRow(
      box(
        width = 12,
        plotOutput(# plotly::plotlyOutput(
          ns("fig3")
        ),
        title = "Figure 3. Relazione stimata (linea rossa in grassetto,
        l'area rossa indica gli intervalli di confidenza al 95%) tra
        numero di posti letto totali in terapia intensiva occupati
        da pazienti Covid e proporzione di tamponi positivi."
      )
    ),
    fluidRow(
      box(
        width = 12,
        plotly::plotlyOutput(ns("fig4")),
        title = "Figure 4. Andamento stimato (linea rossa in grassetto,
        l'area rossa indica gli intervalli di confidenza al 95%) della
        differenza tra il numero di posti letto in terapia intensiva
        in data odierna e 3 giorni precedenti. Andamento osservato
        (punti blu) fino alla data odierna."
      )
    )
  )
}

#' icuve_ts Server Function
#'
#' @import ggplot2
#' @import covid19.icuve
#' @import mgcv
#' @noRd
mod_icuve_ts_server <- function(id) {



  callModule(id = id, function(input, output, session) {
    ns <- session$ns

    output$fig1 <- plotly::renderPlotly({
      plotly::ggplotly(ggprop)
    })

    output$fig2 <- plotly::renderPlotly({
      plotly::ggplotly(ggbeds)
    })

    output$fig3 <- renderPlot({# plotly::renderPlotly({
      ggswab # plotly::ggplotly(ggswab)
    })

    output$fig4 <- plotly::renderPlotly({
      plotly::ggplotly(ggdelta_days)
    })

  })
}

## To be copied in the UI
# mod_icuve_ts_ui("icuve_ts_cl")

## To be copied in the server
# mod_icuve_ts_server("icuve_ts_cl")

