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

  province_veneto <- c(
    "Belluno" = "belluno",
    "Padova" = "padova",
    "Rovigo" = "rovigo",
    "Treviso" = "treviso",
    "Venezia" = "venezia",
    "Verona" = "verona",
    "Vicenza" = "vicenza"
  )

  regional_info <- list(
    "CoViD-19" = list(
      "CoViD-19 Deaths" = "covid_dead",
      "CoViD-19 New" = "covid_new",
      "CoViD-19 Discharged" = "covid_discharged",
      "CoViD-19 Beds occupied" = "covid_occupied",
      "CoViD-19 Beds variation" = "covid_variation"
    ),
    "Non-CoViD-19" = list(
      "Non-CoViD-19 Beds occupied" = "other_occupied"
    ),
    "Overall" = list(
      "Overall Free beds" = "overall_free",
      "Overall Beds occupied" = "overall_occupied",
      "Overall Total number of beds" = "overall_total"
    )
  )

  live_info <- list(
    "CoViD-19" = list(
      "CoViD-19 Free beds" = "covid_free",
      "CoViD-19 Beds occupied" = "covid_occupied",
      "CoViD-19 Total number of beds" = "covid_total"
    ),
    "Non-CoViD-19" = list(
      "Non-CoViD-19 Free beds" = "other_free",
      "Non-CoViD-19 Beds occupied" = "other_occupied",
      "Non-CoViD-19 Total number of beds" = "other_total"
    ),
    "Stand-by" = list(
      "Stand-by Free beds" = "general_free"
    ),
    "Overall" = list(
      "Overall Free beds" = "overall_free",
      "Overall Beds occupied" = "overall_occupied",
      "Overall Total number of beds" = "overall_total"
    ),
    "Supplementary CoViD-19 info" = list(
      "Suspected CoViD-19 cases" = "covid_suspect",
      "ECMO" = "covid_ecmo",
      "IOT" = "covid_iot",
      "NIV" = "covid_niv",
      "CoViD-19 Negativized" = "covid_negativized"
    )
  )

  centri_veneto <- c(
    "ABANO", "ADRIA", "ARZIGNANO", "BASSANO", "BELLUNO", "CAMPOSAM",
    "CASTELFRANCO", "CHIOGGIA", "CITTADELLA", "CONEGLIANO", "DOLO",
    "FELTRE", "ISTAR 3", "ISTAR 4", "ISTAR 5", "JESOLO", "LEGNAGO",
    "MESTRE", "MIRANO", "MONTEBELLUNA", "NEGRAR", "ODERZO",
    "PADOVA NCH", "PD CENTRALE", "PD IOV", "PD SA", "PESCHIERA",
    "PIEVE DI CADORE", "PIOVE DI S.", "PORTO VIRO", "PORTOGRUARO",
    "ROVIGO", "SAN BONIFACIO", "SAN DONA'", "SANTORSO", "SCHIAVONIA",
    "TRECENTA", "TREVISO 1", "TREVISO 5", "TREVISO CCH", "TREVISO INF",
    "TREVISO NCH", "VENEZIA SGP", "VICENZA", "VILLAFRANCA",
    "VITTORIO V.TO", "VR B.ROMA", "VR B.TRENTO A", "VR B.TRENTO B",
    "VR B.TRENTO POSTOP", "VR B.TRENTO SPINA"
  )



  fluidPage(
    fluidRow(
      h1(HTML("Analisi delle serie storiche per l'occupazione dei posti letto nelle terapie intensive in Veneto")),
      h3(HTML("Exponential smoothing state space model")),
      p(HTML("\u00C8 stato impiegato un approccio Estimation Smothing State Space Model per la predizione delle serie dei ricoveri COVID-19 in terapia intensiva.

Il Modello si caratterizza per i parametri di Errore (E), Trend (T) e Stagionalit\u00E0 (S).

 La notazione del modello di definisce come ETS(Errore, Trend, Stagionalit\u00E0).

Le tre componenti possono essere additive (A), o moltiplicative (M).

Il Trend pu\u00F2 essere anche Additivo Damped (Ad). Le previsioni generate dal metodo Additivo mostrano una trend costante (in aumento o in diminuzione) a tempo indeterminato verso il futuro. Il parametro Damped Ad, invece, smorza lo shape della curva verso un appiattimento certo periodo di tempo nel futuro.

Ad esempio un modello ETS(A,Ad,N) indica un modello con Errore Additivo, Trend Damped e Nessuna stagionalit\u00E0 (N).

La parametrizzazione ottimale viene scelta in modo automatico utilizzando come criterio di selezione dei parametri il BIC (Bayesian Information Criterion).")),
    ),
    fluidRow(
      h1(HTML("Report regionale (Veneto)")),
      column(8,
        shiny::selectInput(
          ns("whichInfoReg"), "(De-)selezionare le variabili da mostrare",
          choices = regional_info,
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
        title = "Dettaglio regionale della situazione corrente (punti) e stima andamento a 15 giorni (linee) dei posti letto nelle terapie intensive venete con intervalli di confidenza al 95% (bande).",
        footer = "Linee orizzontali tratteggiate a 400 (rosso) e 500 (nero) posti letto."
      ),
      box(DT::DTOutput(ns("tab_icuve_sitrep")),
          width = 12,
          title = "Numero di ricoveri attesi in base alle stime
          del modello nei 15 giorni successivi all'ultimo dato disponibile. Tra
          parentesi quadre sono riportati gli intervalli di confidenza
          al 95%."
      )
    ),
    fluidRow(
      h1(HTML("Report provinciale (Veneto)")),
      column(8,
        shiny::selectInput(
          ns("whichProvince"), "(De-)selezionare le province di interesse",
          choices = province_veneto,
          multiple = TRUE,
          selected = "padova",
          width = "100%"
        )
      ),
      column(8,
        shiny::selectInput(
          ns("whichInfoProv"), "(De-)selezionare le variabili da mostrare",
          choices = live_info,
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
        plotly::plotlyOutput(ns("gg_icuve_sitrep_prov"), height = "600px"),
        title = "Dettaglio provinciale della situazione corrente (punti) e stima andamento a 15 giorni (linee) dei posti letto nelle terapie intensive venete con intervalli di confidenza al 95% (bande)."
      ),
      box(DT::DTOutput(ns("tab_icuve_sitrep_prov")),
          width = 12,
          title = "Numero di ricoveri attesi in base alle stime
          del modello nei 15 giorni successivi all'ultimo dato disponibile. Tra
          parentesi quadre sono riportati gli intervalli di confidenza
          al 95%."
      )
    ),
    fluidRow(
      h1(HTML("Report per centro (Veneto)")),
      column(8,
        shiny::selectInput(
          ns("whichCentre"), "Selezionare il centro di interesse",
          choices = centri_veneto,
          selected = NULL,
          width = "100%"
        )
      ),
      column(8,
        shiny::selectInput(
          ns("whichInfoCntr"), "(De-)selezionare le variabili da mostrare",
          choices = live_info,
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
        plotly::plotlyOutput(ns("gg_icuve_sitrep_centre")),
        title = "Dettaglio per centro della situazione corrente (punti) e stima andamento a 15 giorni (linee) dei posti letto nelle terapie intensive venete con intervalli di confidenza al 95% (bande)."
      ),
      box(DT::DTOutput(ns("tab_icuve_sitrep_centre")),
          width = 12,
          title = "Numero di ricoveri attesi in base alle stime
          del modello nei 15 giorni successivi all'ultimo dato disponibile. Tra
          parentesi quadre sono riportati gli intervalli di confidenza
          al 95%."
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

#' icuve_sitrep Server Function
#'
#' @import ggplot2
#' @noRd
mod_icuve_sitrep_server <- function(id) {

  stopifnot(`package {covid19.icuve} required for this function` =
              requireNamespace("covid19.icuve"))

  icuve_sitrep <- covid19.icuve::fetch_gsheet()

  glive_ts <- withr::with_db_connection(
    con = list(con = covid19.icuve::dblive_connect()),
    code = covid19.icuve::dblive_read(con, verbose = FALSE)
  )

  callModule(id = id, function(input, output, session) {
    ns <- session$ns




    # siterep ----------------------------------------------------------

    sitrep <- reactive({
      which_info_reg <- req(input$whichInfoReg)
      gg_siterep(icuve_sitrep, which_info_reg, ic = TRUE)
    })

    output$gg_icuve_sitrep <- renderPlotly({

      plotly::ggplotly(sitrep()[["gg"]]) %>%
        plotly::config(modeBarButtonsToRemove = c(
          "zoomIn2d", "zoomOut2d", "pan2d", "select2d", "lasso2d")) %>%
        plotly::config(displaylogo = FALSE)
    })

    output$tab_icuve_sitrep <- DT::renderDT({
      sitrep()[["db_pred"]] %>%
        dplyr::filter(.data$date >= max(sitrep()[["db_long"]]$date)) %>%
        dplyr::select(-.data$method) %>%
        dplyr::mutate(dplyr::across(c(lower, upper), ~round(.x, 1))) %>%
        dplyr::rename(`Posti letto attesi` = `N beds`)
    })



    # provincial -------------------------------------------------------

    live_prov <- reactive({
      which_prov <- req(input$whichProvince)
      which_info_prov <- req(input$whichInfoProv)

      gg_live(glive_ts, which_prov, which_info_prov, "province",
                      ic = TRUE)
    })

    output$gg_icuve_sitrep_prov <- renderPlotly({

      plotly::ggplotly(live_prov()[["gg"]]) %>%
        plotly::config(modeBarButtonsToRemove = c(
          "zoomIn2d", "zoomOut2d", "pan2d", "select2d", "lasso2d")) %>%
        plotly::config(displaylogo = FALSE)
    })

    output$tab_icuve_sitrep_prov <- DT::renderDT({
      live_prov()[["db_pred"]] %>%
        dplyr::filter(.data$date >= max(live_prov()[["db_long"]]$date)) %>%
        dplyr::select(-.data$method) %>%
        dplyr::mutate(dplyr::across(c(lower, upper), ~round(.x, 1))) %>%
        dplyr::rename(`Posti letto attesi` = `N beds`)
    })



    # centre -----------------------------------------------------------

    live_cntr <- reactive({
      which_cntr <- req(input$whichCentre)
      which_info_cntr <- req(input$whichInfoCntr)

      gg_live(glive_ts, which_cntr, which_info_cntr, "centre",
              ic = TRUE)
    })

    output$gg_icuve_sitrep_centre <- renderPlotly({

      plotly::ggplotly(live_cntr()[["gg"]]) %>%
        plotly::config(modeBarButtonsToRemove = c(
          "zoomIn2d", "zoomOut2d", "pan2d", "select2d", "lasso2d")) %>%
        plotly::config(displaylogo = FALSE)
    })

    output$tab_icuve_sitrep_centre <- DT::renderDT({
      live_cntr()[["db_pred"]] %>%
        dplyr::filter(.data$date >= max(live_cntr()[["db_long"]]$date)) %>%
        dplyr::select(-.data$method) %>%
        dplyr::mutate(dplyr::across(c(lower, upper), ~round(.x, 1))) %>%
        dplyr::rename(`Posti letto attesi` = `N beds`)
    })


  })
}

## To be copied in the UI
# mod_icuve_sitrep_ui("sitrep")

## To be copied in the server
# mod_icuve_sitrep_server("sitrep")

