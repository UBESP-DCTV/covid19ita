#' icuve_ts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS fluidRow fluidPage
#' @importFrom shinydashboard box
mod_icuve_static_ui <- function(id){
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
        plotly::plotlyOutput(ns("fig3")),
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
mod_icuve_static_server <- function(id) {

  icuve_static <- covid19.icuve::icuve

  # 1) Data preparation ------------------------------------------------
  df_weekly <- icuve_static %>%
    # Select only relevant columns
    dplyr::select(
      .data$icu_addmission, .data$icu_discharge, .data$age,
      dplyr::starts_with("comorb")
    ) %>%
    # Compute Length of stay (LOS) in days and N comorb ----------------
    dplyr::mutate(
      los = difftime(
        .data$icu_discharge, .data$icu_addmission,  units = "days"
      )
    ) %>%
    dplyr::mutate_at(
      dplyr::vars(dplyr::starts_with("comorb")), ~ as.double(.)
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      n_comorb = sum(
        dplyr::across(dplyr::starts_with("comorb")),
        na.rm = TRUE
      )
    ) %>%
    dplyr::ungroup() %>%
    # Arrange db by date of ICU admission
    dplyr::arrange(.data$icu_addmission) %>%
    # Column with the week of the month
    dplyr::mutate(
      week_month = ceiling(lubridate::day(.data$icu_addmission)/7)
    ) %>%
    dplyr::group_by(week_month) %>%
    dplyr::mutate(
      min_date = min(.data$icu_addmission, na.rm = TRUE),
      max_date = max(.data$icu_addmission, na.rm = TRUE)
    )

  # 2) Proportion of deaths in patients admitted to ICU ----------------


  # 3) Weekly ICU lengths of stay (median) -----------------------------
    #


  callModule(id = id, function(input, output, session) {
    ns <- session$ns

    output$fig1 <- plotly::renderPlotly({
      plotly::ggplotly(ggprop)
    })

    output$fig2 <- plotly::renderPlotly({
      plotly::ggplotly(ggbeds)
    })

    output$fig3 <- plotly::renderPlotly({
      plotly::ggplotly(ggswab)
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

