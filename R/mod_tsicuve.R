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

#' tsicuve Server Function
#'
#' @import ggplot2
#' @noRd
mod_icuve_ts_server <- function(id) {

  # 1) Data preparation ------------------------------------------------
  veneto <- dpc_covid19_ita_regioni %>%
    # Get the Veneto ICU data
    dplyr::filter(.data$denominazione_regione == "Veneto") %>%
    # Select relevant variables
    dplyr::select(.data$data, .data$terapia_intensiva) %>%
    # Dates in lubridate format
    dplyr::mutate(data = lubridate::as_date(.data$data))

  # Define inputs for the functions ------------------------------------
  data <- veneto
  n_ahead <- 7L
  d <- 20L
  tstart <- min(veneto[["data"]])

  # 2) TS models -------------------------------------------------------
  d_seq <- as.integer(round(seq(
    from = 10L,
    to = length(veneto$data) - d - n_ahead,
    by = 14
  )))
  # 2A) Holter ---------------------------------------------------------
  # Plot ---------------------------------------------------------------
  ts_holter <- holter_plot(veneto, n_ahead, d, tstart)

  # Error plot ---------------------------------------------------------
  df_error <- purrr::map_dfr(
    .x = d_seq, ~ holter_error(veneto, n_ahead, .x, tstart)
  )

  error_holter <- ggplot(
    data = df_error,
    mapping = aes(x = .data$data, y = .data$error)
  ) +
    geom_point(size = 1.1) +
    geom_smooth(se = FALSE) +
    ylab("Squared error") +
    xlab("") +
    scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
    theme(
      axis.text.x = element_text(
        angle = 60, hjust = 1, vjust = 0.5
      )
    )






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

