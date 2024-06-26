#' reg_tsicuve UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS fluidRow fluidPage
#' @importFrom shinydashboard box
mod_reg_nocritica_ui <- function(id) {
  ns <- NS(id)

  date_range <- range(dpc_covid19_ita_regioni$data, na.rm = TRUE) %>%
    lubridate::as_date()
  first_date <- date_range[[1]]
  last_date  <- date_range[[2]]

  day_step <- lubridate::days(1)
  slider_min <- first_date + lubridate::days(10)
  slider_max <- last_date

  fluidPage(
    fluidRow(
      shiny::selectInput(
        ns("whichRegion"), "Selezionare la regione di interesse",
        choices = regions(),
        selected = "Veneto",
        width = "100%"
      )
    ),
    fluidRow(
      h2(HTML("Andamento ricoveri in area non critica per la regione Selezionata")),
      box(
        width = 12,
        p(
          "\u00C8 stato impiegato un approcco descritto in letteratura per la
          predizione delle serie di ricoveri in area non critica per COVID-19
          Exponential Smoothing state space model. I risultati sono riportati
          rispettivamente in Figura 1A."
        ),
        p(
          "Si \u00E8 proceduto alla predizione dei casi ricoverati in
          area non critica a partire dal 24 febbraio 2020 utilizzando le
          osservazioni via via accumulate nei giorni precedenti.
          I ricoveri osservati sono rappresentati dalla linea blu,
          quelli attesi (ovvero predetti dal modello) sono rappresentati
          dalla linea rossa.
          Sfiorando il grafico col cursore si pu\u00F2 visualizzare, per
          ciascun giorno selezionato, il numero di casi osservati e
          quello predetto dal modello (\"attesi\")."
        ),
        p(
          "La figura 1B rappresenta l'andamento dell'errore
          quadratico di stima del modello corrispondente."
        ),
        p(
          "In Tabella 1 sono riportati i ricoveri
          previsti dal modello (con relativi intervalli di confidenza al
          95%) nei 15 giorni successivi all'ultimo dato disponibile."
        )
      )
    ),
    fluidRow(
      h3(HTML("Exponential smoothing state space model")),
      p(HTML("\u00C8 stato impiegato un approccio Estimation Smothing State Space Model per la predizione della serie dei ricoveri COVID-19 in area non critica.

Il Modello si caratterizza per i parametri di Errore (E), Trend (T) e Stagionalit\u00E0 (S).

 La notazione del modello di definisce come ETS(Errore, Trend, Stagionalit\u00E0).

Le tre componenti possono essere additive (A), o moltiplicative (M).

Il Trend pu\u00F2 essere anche Additivo Damped (Ad). Le previsioni generate dal metodo Additivo mostrano una trend costante (in aumento o in diminuzione) a tempo indeterminato verso il futuro. Il parametro Damped Ad, invece, smorza lo shape della curva verso un appiattimento certo periodo di tempo nel futuro.

Ad esempio un modello ETS(A,Ad,N) indica un modello con Errore Additivo, Trend Damped e Nessuna stagionalit\u00E0 (N).

La parametrizzazione ottimale viene scelta in modo automatico utilizzando come criterio di selezione dei parametri il BIC (Bayesian Information Criterion).")),
      checkboxInput(ns("maxdate"), "Usa fino all'ultimo dato disponibile per la stima del modello."),
      sliderInput(
        width = "45%", ns("lastDate_d"),
        label = "Selezionare l'ultima data da considerare per la stima del modello",
        value = slider_max - lubridate::days(2),
        min = slider_min,
        max = slider_max,
        step = day_step,
        animate = animationOptions(interval = 400)
      ),
      box(plotlyOutput(ns("fig1a")) %>%
            shinycssloaders::withSpinner(hide.ui = FALSE),
          width = 12,
          title = "Figura 1A. Andamento stimato (linea rossa in grassetto,
        l'area rossa indica gli intervalli di confidenza al 95%) del
        numero di ricoveri in area non critica.
        Andamento osservato (linea blu) fino all'ultimo dato disponibile."
      ),
      box(DT::DTOutput(ns("tab1")) %>%
            shinycssloaders::withSpinner(),
          width = 12,
          title = "Tabella 1. Numero di ricoveri attesi in base alle stime
          del modello nei 15 giorni successivi all'ultimo dato disponibile.
          Tra parentesi quadre sono riportati gli intervalli di confidenza
          al 95%."
      ),
      box(plotlyOutput(ns("fig1b")) %>%
            shinycssloaders::withSpinner(),
          width = 12,
          title = "Figura 1B. Andamento dell'errore quadratico del
        modello fino all'ultimo dato disponibile.",
          footer = "NOTE: il modello \u00E8 stato stimato ipotizzando un damped
          trend. La linea blu rappresenta lo smoothing con metodo local
          polinomial regression (LOESS, span = 0.75, degree = 2)."
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
mod_reg_nocritica_server <- function(id) {

  callModule(id = id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$maxdate, {
      if (input$maxdate) {
        shiny::updateSliderInput(
          session = session,
          inputId = "lastDate_d",
          value = max(region()[["data"]], na.rm = TRUE)
        )
      } else {
        shiny::updateSliderInput(
          session = session,
          inputId = "lastDate_d",
          value = max(region()[["data"]], na.rm = TRUE) - lubridate::days(2)
        )
      }
    })

    observeEvent(input$lastDate_d, {
      if (input$lastDate_d != max(region()[["data"]], na.rm = TRUE)) {
        shiny::updateCheckboxInput(session = session, "maxdate", value = FALSE)
      }
    })


    # 1) Data preparation ------------------------------------------------
    region <- reactive({
      req(input$whichRegion)

      dpc_covid19_ita_regioni %>%
        # Get the region ICU data
        dplyr::filter(.data$denominazione_regione == input$whichRegion) %>%
        # Select relevant variables
        dplyr::select(.data$data,
          .data[["terapia_intensiva"]],
          .data[["ricoverati_con_sintomi"]]
        ) %>%
        # Dates in lubridate format
        dplyr::mutate(data = lubridate::as_date(.data$data))
    })

    # Define inputs for the functions ------------------------------------
    n_ahead <- 7L
    tstart <- reactive({
      min(region()[["data"]])
    })
    tstop <- reactive({
      tstart() + 20L
    })

    # 2) TS models -------------------------------------------------------
    error_ets <- reactive({

      d_seq <- as.integer(round(seq(
        from = 10L,
        to = length(region()[["data"]]) -
          lubridate::interval(tstart(), tstop()) / lubridate::ddays(1) -
          n_ahead,
        by = 3
      )))

      purrr::map_dfr(
        .x = d_seq, ~ partial_ts_error(
          region(), n_ahead, .x, tstart(), "ets_auto"), critica = FALSE
      ) %>%
        ts_plot_error()
    })


    # 3) Table with forecast -------------------------------------------
    # 3B) ets auto -----------------------------------------------------
    fc_ets <- reactive({
      partial_forecast(region(), 15L, "ets_auto", critica = FALSE)
    })


    output$fig1a <- renderPlotly({
      req(input$lastDate_d)

      gg_ets <- partial_ts_plot(
        data = region(),
        n_ahead = n_ahead,
        d = NULL,
        tstart = tstart(),
        tstop = input$lastDate_d,
        method = "ets_auto",
        critica = FALSE
      )

      ggplotly(gg_ets, originalData = FALSE)
    })

    output$tab1 <- DT::renderDT(fc_ets())

    output$fig1b <- plotly::renderPlotly({
      plotly::ggplotly(error_ets())
    })

  })
}
