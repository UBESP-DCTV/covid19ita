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
  slider_max <- last_date - lubridate::days(2)

  fluidPage(
    fluidRow(
      h2(HTML("Andamento ricoveri in terapia intensiva per la regione Veneto")),
      box(
        width = 12,
        p(
          "Sono stati impiegati tre approcci descritti in letteratura per la
          predizione delle serie di ricoveri in terapia intensiva per COVID-19:
          Holter-Winters Filtering, Exponential Smoothing state space
          model e ARIMA (1). I risultati dei tre approcci sono riportati
          rispettivamente in Figura 1A, Figura 2A e Figura 3A."
        ),
        p(
          "Si \u00E8 proceduto alla predizione dei casi ricoverati in terapia
          intensiva a partire dal 24 febbraio 2020 utilizzando le osservazioni
          via via accumulate nei giorni precedenti. I ricoveri osservati
          sono rappresentati dalla linea blu, quelli attesi (ovvero
          predetti dal modello) sono rappresentati dalla linea rossa.
          Sfiorando il grafico col cursore si pu\u00F2 visualizzare, per
          ciascun giorno selezionato, il numero di casi osservati e
          quello predetto dal modello (\"attesi\")."
        ),
        p(
          "Le figure 1B, 2B e 3B rappresentano l'andamento dell'errore
          quadratico di stima dei modelli corrispondenti. Il modello che
          mostra un pi\u00F2 basso errore di stima \u00E8 l'Holter-Winters Filtering
          che si rivela quindi l'approccio con la migliore capacit\u00E0
          predittiva."
        ),
        p(
          "In Tabella 1, Tabella 2 e Tabella 3 sono riportati i ricoveri
          previsti dai modelli (con relativi intervalli di confidenza al
          95%) nei 15 giorni successivi all'ultimo dato disponibile."
        )
      )
    ),
    fluidRow(
      h3(HTML("1) Holter-Winters Filtering")),
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
        numero di ricoveri in terapia intensiva.
        Andamento osservato (linea blu) fino all'ultimo dato disponibile."
      ),
      box(DT::DTOutput(ns("tab1")),
          width = 12,
          title = "Tabella 1. Numero di ricoveri attesi in base alle stime
          del modello nei 15 giorni successivi all'ultimo dato disponibile. Tra
          parentesi quadre sono riportati gli intervalli di confidenza
          al 95%."
      ),
      box(plotlyOutput(ns("fig1b")),
          width = 12,
          title = "Figura 1B. Andamento dell'errore quadratico del
        modello fino all'ultimo dato disponibile.",
          footer = "NOTE: la linea blu rappresenta lo smoothing con metodo
          local polinomial regression (LOESS, span = 0.75, degree = 2)."
      )
    ),
    fluidRow(
      h3(HTML("2) Exponential smoothing state space model")),
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
        numero di ricoveri in terapia intensiva.
        Andamento osservato (linea blu) fino all'ultimo dato disponibile.",
          footer = "NOTE: il modello \u00E8 stato stimato ipotizzando un damped trend, con parametri di Errore, Trend e Stagionalit\u00E0 indicati, nell'ordine in figura, con la seguente convenzione:
          \"N\" = nessuna, \"A\"/\"Ad\" = additiva, \"M\" = moltiplicativa e \"Z\" = selezione automatica"
      ),
      box(DT::DTOutput(ns("tab2")),
          width = 12,
          title = "Tabella 2. Numero di ricoveri attesi in base alle stime
          del modello nei 15 giorni successivi all'ultimo dato disponibile.
          Tra parentesi quadre sono riportati gli intervalli di confidenza
          al 95%."
      ),
      box(plotlyOutput(ns("fig2b")),
          width = 12,
          title = "Figura 2B. Andamento dell'errore quadratico del
        modello fino all'ultimo dato disponibile.",
          footer = "NOTE: il modello \u00E8 stato stimato ipotizzando un damped
          trend. La linea blu rappresenta lo smoothing con metodo local
          polinomial regression (LOESS, span = 0.75, degree = 2)."
      )
    ),
    fluidRow(
      h3(HTML("3) ARIMA")),
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
        numero di ricoveri in terapia intensiva.
        Andamento osservato (linea blu) fino all'ultimo dato disponibile.",
          footer = "NOTE: il modello \u00E8 stato stimato con un metodo automatico
          basato sull'AIC corretto."
      ),
      box(DT::DTOutput(ns("tab3")),
          width = 12,
          title = "Tabella 3. Numero di ricoveri attesi in base alle stime
          del modello nei 15 giorni successivi alla data odierna. Tra
          parentesi quadre sono riportati gli intervalli di confidenza
          al 95%."
      ),
      box(plotlyOutput(ns("fig3b")),
          width = 12,
          title = "Figura 3B. Andamento dell'errore quadratico del
        modello fino all'ultimo dato disponibile.",
          footer = "NOTE: il modello \u00E8 stato stimato con un metodo
          automatico basato sull'AIC corretto. La linea blu rappresenta
          lo smoothing con metodo local polinomial regression (LOESS,
          span = 0.75, degree = 2)."
      )
    ),
    fluidRow(
      box(
        width = 12, title = "Bibliografia",
        p(HTML("
          <ol>
            <li>Simone AA., Arruda EF., Goldwasser R., Lobo MSC., Salles A., Lapa e Silva, JR., . Demand Forecast and Optimal Planning of Intensive Care Unit (ICU) capacity. Pequisa Operacional. 2017;37(2):229-245.</li>
          </ol>
        "))
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

  # 2) Plots with errors -----------------------------------------------
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

  # 3) Table with forecast ---------------------------------------------
  # 3A) Holter ---------------------------------------------------------
  fc_holter <- partial_forecast(veneto, 15L, "hw")

  # 3B) Damped ---------------------------------------------------------
  fc_damped <- partial_forecast(veneto, 15L, "ets")

  # 3C) ARIMA ----------------------------------------------------------
  fc_arima <- partial_forecast(veneto, 15L, "arima")



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

    output$tab1 <- DT::renderDT({fc_holter})

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

    output$tab2 <- DT::renderDT({fc_damped})

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

    output$tab3 <- DT::renderDT({fc_arima})

    output$fig3b <- plotly::renderPlotly({
      plotly::ggplotly(error_arima)
    })

  })
}


