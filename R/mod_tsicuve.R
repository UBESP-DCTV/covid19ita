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

  fluidPage(
    fluidRow(
      h2(HTML("1) Holter-Winters Filtering")),
      sliderInput(
        width = "45%", ns("lastDate"),
        label = "Selezionare l'ultima data da considerare per la stima del modello",
        value = lubridate::as_date(max(dpc_covid19_ita_regioni$data)),
        min = lubridate::as_date(min(dpc_covid19_ita_regioni$data)) +
          lubridate::days(10),
        max = lubridate::as_date(max(dpc_covid19_ita_regioni$data)) -
          lubridate::days(8),
        step = lubridate::days(1),
        animate = animationOptions(interval = 400)
      ),
      box(plotlyOutput(ns("fig1a")),
          width = 12,
          title = "Figura 1A. Andamento stimato (linea rossa in grassetto,
        l'area rossa indica gli intervalli di confidenza al 95%) del
        numero di posti occupati in terapia intensiva.
        Andamento osservato (punti blu) fino alla data odierna."
      ),
      box(plotlyOutput(ns("fig1b")),
          width = 12,
          title = "Figura 1B. Andamento dell'errore quadratico del
        modello fino alla data odierna."
      )
    ),
    fluidRow(
      h2(HTML("2) Exponential smoothing state space model")),
      sliderInput(
        width = "45%", ns("lastDate"),
        label = "Selezionare l'ultima data da considerare per la stima del modello",
        value = lubridate::as_date(max(dpc_covid19_ita_regioni$data)),
        min = lubridate::as_date(min(dpc_covid19_ita_regioni$data)) +
          lubridate::days(10),
        max = lubridate::as_date(max(dpc_covid19_ita_regioni$data)) -
          lubridate::days(8),
        step = lubridate::days(1),
        animate = animationOptions(interval = 400)
      ),
      box(plotlyOutput(ns("fig2a")),
          width = 12,
          title = "Figura 2A. Andamento stimato (linea rossa in grassetto,
        l'area rossa indica gli intervalli di confidenza al 95%) del
        numero di posti occupati in terapia intensiva.
        Andamento osservato (punti blu) fino alla data odierna.",
          footer = "NOTE: il modello è stato stimato ipotizzando un damped trend."
      ),
      box(plotlyOutput(ns("fig2b")),
          width = 12,
          title = "Figura 2B. Andamento dell'errore quadratico del
        modello fino alla data odierna.",
          footer = "NOTE: il modello è stato stimato ipotizzando un damped trend."
      )
    ),
    fluidRow(
      h2(HTML("3) ARIMA")),
      sliderInput(
        width = "45%", ns("lastDate"),
        label = "Selezionare l'ultima data da considerare per la stima del modello",
        value = lubridate::as_date(max(dpc_covid19_ita_regioni$data)),
        min = lubridate::as_date(min(dpc_covid19_ita_regioni$data)) +
          lubridate::days(10),
        max = lubridate::as_date(max(dpc_covid19_ita_regioni$data)) -
          lubridate::days(8),
        step = lubridate::days(1),
        animate = animationOptions(interval = 400)
      ),
      box(plotlyOutput(ns("fig3a")),
          width = 12,
          title = "Figura 3A. Andamento stimato (linea rossa in grassetto,
        l'area rossa indica gli intervalli di confidenza al 95%) del
        numero di posti occupati in terapia intensiva.
        Andamento osservato (punti blu) fino alla data odierna.",
          footer = "NOTE: il modello è stato stimato con un metodo automatico basato
          sull'AIC corretto."
      ),
      box(plotlyOutput(ns("fig3b")),
          width = 12,
          title = "Figura 3B. Andamento dell'errore quadratico del
        modello fino alla data odierna.",
          footer = "NOTE: il modello è stato stimato con un metodo automatico basato
          sull'AIC corretto."
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
    by = 14
  )))

  # 2A) Holter ---------------------------------------------------------
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

  # 2B) Damped ---------------------------------------------------------
  df_error <- purrr::map_dfr(
    .x = d_seq, ~ damped_error(veneto, n_ahead, .x, tstart)
  )

  error_damped <- ggplot(
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

  # 2C) ARIMA ----------------------------------------------------------
  df_error <- purrr::map_dfr(
    .x = d_seq, ~ arima_error(veneto, n_ahead, .x, tstart)
  )

  error_arima <- ggplot(
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

    output$fig1a <- renderPlotly({
      gg_holter <- holter_plot(
        data = dpc_covid19_ita_regioni %>%
          # Get the Veneto ICU data
          dplyr::filter(.data$denominazione_regione == "Veneto") %>%
          # Select relevant variables
          dplyr::select(.data$data, .data$terapia_intensiva) %>%
          # Dates in lubridate format
          dplyr::mutate(data = lubridate::as_date(.data$data)),
        n_ahead = n_ahead,
        tstart = tstart,
        tstop = input$lastDate
      )

      ggplotly(gg_holter, originalData = FALSE)
    })

    output$fig1b <- plotly::renderPlotly({
      plotly::ggplotly(error_holter)
    })

    output$fig2a <- renderPlotly({
      gg_damped <- damped_plot(
        data = dpc_covid19_ita_regioni %>%
          # Get the Veneto ICU data
          dplyr::filter(.data$denominazione_regione == "Veneto") %>%
          # Select relevant variables
          dplyr::select(.data$data, .data$terapia_intensiva) %>%
          # Dates in lubridate format
          dplyr::mutate(data = lubridate::as_date(.data$data)),
        n_ahead = n_ahead,
        tstart = tstart,
        tstop = input$lastDate
      )

      ggplotly(gg_damped, originalData = FALSE)
    })

    output$fig2b <- plotly::renderPlotly({
      plotly::ggplotly(error_damped)
    })

    output$fig3a <- renderPlotly({
      gg_arima <- arima_plot(
        data = dpc_covid19_ita_regioni %>%
          # Get the Veneto ICU data
          dplyr::filter(.data$denominazione_regione == "Veneto") %>%
          # Select relevant variables
          dplyr::select(.data$data, .data$terapia_intensiva) %>%
          # Dates in lubridate format
          dplyr::mutate(data = lubridate::as_date(.data$data)),
        n_ahead = n_ahead,
        tstart = tstart,
        tstop = input$lastDate
      )

      ggplotly(gg_arima, originalData = FALSE)
    })

    output$fig3b <- plotly::renderPlotly({
      plotly::ggplotly(error_arima)
    })

  })
}


