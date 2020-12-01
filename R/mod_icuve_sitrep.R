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
  )

  live_info <- list(
    "CoViD-19" = list(
      "Free beds" = "covid_free",
      "Beds occupied" = "covid_occupied",
      "Total number of beds" = "covid_total"
    ),
    "Non-CoViD-19" = list(
      "Free beds" = "other_free",
      "Beds occupied" = "other_occupied",
      "Total number of beds" = "other_total"
    ),
    "Stand-by" = list(
      "Free beds" = "general_free"
    ),
    "Overall" = list(
      "Free beds" = "overall_free",
      "Beds occupied" = "overall_occupied",
      "Total number of beds" = "overall_total"
    ),
    "Supplementary CoViD-19 info" = list(
      "Suspected cases" = "covid_suspect",
      "ECMO" = "covid_ecmo",
      "IOT" = "covid_iot",
      "NIV" = "covid_niv",
      "Negativized" = "covid_negativized"
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
      h3(HTML("Report regionale (Veneto)")),
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
        title = "Dettaglio regionale della situazione corrente (punti) e stima andamento a 15 giorni (linee) dei posti letto nelle terapie intensive venete.",
        footer = "Linee orizzontali tratteggiate a 400 (rosso) e 500 (nero) posti letto."
      )
    ),
    fluidRow(
      h3(HTML("Report provinciale (Veneto)")),
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
        plotly::plotlyOutput(ns("gg_icuve_sitrep_prov")),
        title = "Dettaglio provinciale della situazione corrente (punti) e stima andamento a 15 giorni (linee) dei posti letto nelle terapie intensive venete.",
        footer = "Linee orizzontali tratteggiate a 400 (rosso) e 500 (nero) posti letto."
      )
    ),
    fluidRow(
      h3(HTML("Report per centro (Veneto)")),
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

  glive_ts <- withr::with_db_connection(
    con = list(con = covid19.icuve::dblive_connect()),
    code = covid19.icuve::dblive_read(con, verbose = FALSE)
  )

  callModule(id = id, function(input, output, session) {
    ns <- session$ns




    # siterep ----------------------------------------------------------
    output$gg_icuve_sitrep <- renderPlotly({
      which_info_reg <- req(input$whichInfoReg)

      plotly::ggplotly(
        gg_siterep(icuve_sitrep, which_info_reg)
      ) %>%
        plotly::config(modeBarButtonsToRemove = c(
          "zoomIn2d", "zoomOut2d", "pan2d", "select2d", "lasso2d")) %>%
        plotly::config(displaylogo = FALSE)
    })



    # provincial -------------------------------------------------------
    output$gg_icuve_sitrep_prov <- renderPlotly({
      which_prov <- req(input$whichProvince)
      which_info_prov <- req(input$whichInfoProv)

      plotly::ggplotly(
        gg_live(glive_ts, which_prov, which_info_prov, "province")
      ) %>%
        plotly::config(modeBarButtonsToRemove = c(
          "zoomIn2d", "zoomOut2d", "pan2d", "select2d", "lasso2d")) %>%
        plotly::config(displaylogo = FALSE)
    })




    # centre -----------------------------------------------------------
    output$gg_icuve_sitrep_centre <- renderPlotly({
      which_cntr <- req(input$whichCentre)
      which_info_cntr <- req(input$whichInfoCntr)

      plotly::ggplotly(
        gg_live(glive_ts, which_cntr, which_info_cntr, "centre")
      ) %>%
        plotly::config(modeBarButtonsToRemove = c(
          "zoomIn2d", "zoomOut2d", "pan2d", "select2d", "lasso2d")) %>%
        plotly::config(displaylogo = FALSE)
    })

  })
}

## To be copied in the UI
# mod_icuve_sitrep_ui("sitrep")

## To be copied in the server
# mod_icuve_sitrep_server("sitrep")

