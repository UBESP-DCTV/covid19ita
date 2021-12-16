utils::globalVariables("where")

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
mod_reg_tsicuve_ui <- function(id) {
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
      h2(HTML("Andamento ricoveri in terapia intensiva per la regione Selezionata")),
      box(
        width = 12,
        p(
          "\u00C8 stato impiegato un approcco descritto in letteratura per la
          predizione delle serie di ricoveri in terapia intensiva per COVID-19
          Exponential Smoothing state space model. I risultati sono riportati
          rispettivamente in Figura 1A."
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
      p(HTML("\u00C8 stato impiegato un approccio Estimation Smothing State Space Model per la predizione della serie dei ricoveri COVID-19 in terapia intensiva.

Il Modello si caratterizza per i parametri di Errore (E), Trend (T) e Stagionalit\u00E0 (S).

 La notazione del modello di definisce come ETS(Errore, Trend, Stagionalit\u00E0).

Le tre componenti possono essere additive (A), o moltiplicative (M).

Il Trend pu\u00F2 essere anche Additivo Damped (Ad). Le previsioni generate dal metodo Additivo mostrano una trend costante (in aumento o in diminuzione) a tempo indeterminato verso il futuro. Il parametro Damped Ad, invece, smorza lo shape della curva verso un appiattimento certo periodo di tempo nel futuro.

Ad esempio un modello ETS(A,Ad,N) indica un modello con Errore Additivo, Trend Damped e Nessuna stagionalit\u00E0 (N).

La parametrizzazione ottimale viene scelta in modo automatico utilizzando come criterio di selezione dei parametri il BIC (Bayesian Information Criterion).")),
      checkboxInput(ns("maxdate"), "Usa fino all'ultimo dato disponibile per la stima del modello."),
      sliderInput(
        width = "60%", ns("lastDate_d"),
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
        numero di ricoveri in terapia intensiva.
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
      sliderInput(
        width = "60%", ns("shock"),
        label = "Selezionare la variazione percentuale attesa (shock) nei ricoveri CoViD-19 in area non critica (es: -50 = dimezzamento, 100 = raddoppio)",
        value = 0,
        min = -100,
        max = 100,
        step = 5 #, animate = animationOptions(interval = 400)
      ),
      sliderInput(
        width = "60%", ns("shock_delay"),
        label = "Selezionare dopo quanti giorni (ritardo) da \"oggi\" si ipotizza che lo shock sull'area non critica entri in azione (0 altera direttamente il valore odierno)",
        value = 1,
        min = 0,
        max = 7,
        step = 1 #, animate = animationOptions(interval = 400)
      ),
      box(plotlyOutput(ns("fig2")) %>%
            shinycssloaders::withSpinner(),
            width = 12,
            title = "Figura 2. \u00C8 stato stimato un Exponential Time Series smoothing model sui ricoveri COVID in terapia intensiva con variabile esogena (ETSX model).
          La variabile esogena utilizzata \u00E8 la serie storica dei ricoveri ordinari.
          \u00C8 stato identificato e fissato (non modificabile interattivamente) un lag (ritardo) ottimale di 5 giorni dell'impatto causato dai ricoveri ordinari sull'occupazione in terapia intensiva, scegliendo il valore che minimizza il BIC
          sul modello ETSX.

          Il valore del lag a 5 giorni indica che una variazione improvvisa dei ricoveri ordinari al tempo T,
          impatterebbe sugli accessi in terapia intensiva dopo 5 giorni.

          D'altra parte, lo shock sui ricoveri ordinari (rispetto per esempio alla misura di ristrezione o rilassamento adottata) pu\u00F2 essere selezionato dal secondo cursore, rispetto alla fine della serie osservata.

          L'ammontare di questo effetto improvviso sui ricoveri in area non critica (variabile esogena) pu\u00F2 essere definito dinamicamente tramite lo slider.

          Tale shock \u00E8 stato proiettato in avanti con un forecast ets fino alla fine della finestra di previsione di 15 giorni.
          La variabile esogena che include lo shock sui ricoveri ordinari \u00E8 stata inserita nel modello ETSX come componente ausiliaria.
          Si riportano le stime previsive a 15 giorni le quali considerano l'impatto di una variazione improvvisa
          dei ricoveri ordinari sui ricoveri in terapia intensiva."),
      box(DT::DTOutput(ns("tab2")) %>%
            shinycssloaders::withSpinner(),
          width = 12,
          title = "Tabella 2")
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
mod_reg_tsicuve_server <- function(id) {

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
                      .data$terapia_intensiva,
                      .data$ricoverati_con_sintomi
        ) %>%
        # Dates in lubridate format
        dplyr::mutate(data = lubridate::as_date(.data$data)) %>%
        dplyr::arrange(.data$data)
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
        .x = d_seq, ~ partial_ts_error(region(), n_ahead, .x, tstart(), "ets_auto")
      ) %>%
        ts_plot_error()
    })


    current_exogen_db <- reactive({
      shock <- req(input$shock) / 100
      delay <- req(input$shock_delay)
      current_exogen_icu_model <- exogen_icu_model(
        region(), shock, n_ahead, delay = delay
      )

      exogen_db(region(), shock, current_exogen_icu_model)
    })


    # 3) Table with forecast -------------------------------------------
    # 3B) ets auto -----------------------------------------------------
    fc_ets <- reactive({
      partial_forecast(region(), 15L, "ets_auto", tstop = input$lastDate_d)
    })


    output$fig1a <- renderPlotly({
      req(input$lastDate_d)

      gg_ets <- partial_ts_plot(
        data = region(),
        n_ahead = n_ahead,
        d = NULL,
        tstart = tstart(),
        tstop = input$lastDate_d,
        method = "ets_auto"
      )

      ggplotly(gg_ets, originalData = FALSE)
    })

    output$tab1 <- DT::renderDT(fc_ets())
    output$fig1b <- plotly::renderPlotly({
      plotly::ggplotly(error_ets())
    })

    output$fig2 <- plotly::renderPlotly({
      plotly::ggplotly(gg_shock(current_exogen_db()))
    })
    output$tab2 <- DT::renderDT({

      current_exogen_db()[["forecast_df"]] %>%
        dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, 3)))
    })

  })
}
