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
mod_tsicuve_ui <- function(id){
  ns <- NS(id)

  date_range <- range(dpc_covid19_ita_regioni$data, na.rm = TRUE) %>%
    lubridate::as_date()
  first_date <- date_range[[1]]
  last_date  <- date_range[[2]]

  day_step <- lubridate::days(1)
  slider_min <- first_date + lubridate::days(10)
  slider_max <- last_date - lubridate::days(8)

  fluidPage(
    fluidRow(
      h2(HTML("1) Holter-Winters Filtering")),
      sliderInput(
        width = "45%", ns("lastDate_h"),
        label = "Selezionare l'ultima data da considerare per la stima del modello",
        value = last_date,
        min = slider_min,
        max = slider_max,
        step = day_step,
        animate = animationOptions(interval = 400)
      ),
      box(plotlyOutput(ns("fig1a")),
          width = 12,
          title = "Figura 1A. Andamento stimato (linea rossa in grassetto,
        l'area rossa indica gli intervalli di confidenza al 95%) del
        numero di posti occupati in terapia intensiva.
        Andamento osservato (linea blu) fino alla data odierna."
      ),
      box(plotlyOutput(ns("fig1b")),
          width = 12,
          title = "Figura 1B. Andamento dell'errore quadratico del
        modello fino alla data odierna. La linea blu rappresenta lo smoothing con metodo local polinomial regression (LOESS, span = 0.75, degree = 2)."
      )
    ),
    fluidRow(
      h2(HTML("2) Exponential smoothing state space model")),
      sliderInput(
        width = "45%", ns("lastDate_d"),
        label = "Selezionare l'ultima data da considerare per la stima del modello",
        value = last_date,
        min = slider_min,
        max = slider_max,
        step = day_step,
        animate = animationOptions(interval = 400)
      ),
      box(plotlyOutput(ns("fig2a")),
          width = 12,
          title = "Figura 2A. Andamento stimato (linea rossa in grassetto,
        l'area rossa indica gli intervalli di confidenza al 95%) del
        numero di posti occupati in terapia intensiva.
        Andamento osservato (linea blu) fino alla data odierna.",
          footer = "NOTE: il modello è stato stimato ipotizzando un damped trend."
      ),
      box(plotlyOutput(ns("fig2b")),
          width = 12,
          title = "Figura 2B. Andamento dell'errore quadratico del
        modello fino alla data odierna. La linea blu rappresenta lo smoothing con metodo local polinomial regression (LOESS, span = 0.75, degree = 2).",
          footer = "NOTE: il modello è stato stimato ipotizzando un damped trend."
      )
    ),
    fluidRow(
      h2(HTML("3) ARIMA")),
      sliderInput(
        width = "45%", ns("lastDate_a"),
        label = "Selezionare l'ultima data da considerare per la stima del modello",
        value = last_date,
        min = slider_min,
        max = slider_max,
        step = day_step,
        animate = animationOptions(interval = 400)
      ),
      box(plotlyOutput(ns("fig3a")),
          width = 12,
          title = "Figura 3A. Andamento stimato (linea rossa in grassetto,
        l'area rossa indica gli intervalli di confidenza al 95%) del
        numero di posti occupati in terapia intensiva.
        Andamento osservato (linea blu) fino alla data odierna.",
          footer = "NOTE: il modello è stato stimato con un metodo automatico basato
          sull'AIC corretto."
      ),
      box(plotlyOutput(ns("fig3b")),
          width = 12,
          title = "Figura 3B. Andamento dell'errore quadratico del
        modello fino alla data odierna.",
          footer = "NOTE: il modello è stato stimato con un metodo automatico basato
          sull'AIC corretto. La linea blu rappresenta lo smoothing con metodo local polinomial regression (LOESS, span = 0.75, degree = 2)."
      )
    )
  )
}

#' tsicuve Server Function
#'
#' @import ggplot2
#' @noRd
mod_tsicuve_server <- function(id) {

  # 1) Data preparation ------------------------------------------------
  veneto <- dpc_covid19_ita_regioni %>%
    # Get the Veneto ICU data
    dplyr::filter(.data$denominazione_regione == "Veneto") %>%
    # Select relevant variables
    dplyr::select(.data$data, .data$terapia_intensiva) %>%
    # Dates in lubridate format
    dplyr::mutate(data = lubridate::as_date(.data$data))

  # Define inputs for the functions ------------------------------------
  n_ahead <- 7L
  tstart <- min(veneto[["data"]])
  tstop <- tstart + 20L

  # 2) TS models -------------------------------------------------------
  d_seq <- as.integer(round(seq(
    from = 10L,
    to = length(veneto$data) -
      lubridate::interval(tstart, tstop)/lubridate::ddays(1) -
      n_ahead,
    by = 3
  )))

  # 2A) Holter ---------------------------------------------------------
  error_holter <- purrr::map_dfr(
    .x = d_seq, ~ partial_ts_error(veneto, n_ahead, .x, tstart, "hw")
  ) %>%
    ts_plot_error()

  # 2B) Damped ---------------------------------------------------------
  error_damped <- purrr::map_dfr(
    .x = d_seq, ~ partial_ts_error(veneto, n_ahead, .x, tstart, "ets")
  ) %>%
    ts_plot_error()

  # 2C) ARIMA ----------------------------------------------------------
  error_arima <- purrr::map_dfr(
    .x = d_seq, ~ partial_ts_error(veneto, n_ahead, .x, tstart, "arima")
  ) %>%
    ts_plot_error()



  callModule(id = id, function(input, output, session) {
    ns <- session$ns

    output$fig1a <- renderPlotly({
      gg_holter <- partial_ts_plot(
        data = veneto,
        n_ahead = n_ahead,
        d = NULL,
        tstart = tstart,
        tstop = input$lastDate_h,
        method = "hw"
      )

      ggplotly(gg_holter, originalData = FALSE)
    })

    output$fig1b <- plotly::renderPlotly({
      plotly::ggplotly(error_holter)
    })

    output$fig2a <- renderPlotly({
      gg_damped <- partial_ts_plot(
        data = veneto,
        n_ahead = n_ahead,
        d = NULL,
        tstart = tstart,
        tstop = input$lastDate_d,
        method = "ets"
      )

      ggplotly(gg_damped, originalData = FALSE)
    })

    output$fig2b <- plotly::renderPlotly({
      plotly::ggplotly(error_damped)
    })

    output$fig3a <- renderPlotly({
      gg_arima <- partial_ts_plot(
        data = veneto,
        n_ahead = n_ahead,
        d = NULL,
        tstart = tstart,
        tstop = input$lastDate_a,
        method = "arima"
      )

      ggplotly(gg_arima, originalData = FALSE)
    })

    output$fig3b <- plotly::renderPlotly({
      plotly::ggplotly(error_arima)
    })

  })
}


