#' discrepancies_icuve UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_discrepancies_icuve_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        box(
          width = 12,
          plotly::plotlyOutput(ns("gg_compare_icu"), height = "600px") %>%
            shinycssloaders::withSpinner(),
          title = "Actual ICU covid-19's beds occupied and official data provided.",
          footer = "Actual data from G-sheet, official data from DPC-GitHub."
        ),
        box(
          width = 12,
          plotly::plotlyOutput(ns("gg_delta_icu"), height = "600px") %>%
            shinycssloaders::withSpinner(),
          title = "Difference from actual ICU covid-19's beds occupied and official data provided.",
          footer = "Actual data from G-sheet, official data from DPC-GitHub."
        ),
        box(
          width = 12,
          plotly::plotlyOutput(ns("gg_prop_icu"), height = "600px") %>%
            shinycssloaders::withSpinner(),
          title = "Relative (to actual) difference from actual ICU covid-19's beds occupied and official data provided.",
          footer = "Actual data from G-sheet, official data from DPC-GitHub."
        )
      )
    )
  )
}

#' discrepancies_icuve Server Functions
#'
#' @noRd
mod_discrepancies_icuve_server <- function(id) {
  stopifnot(`package {covid19.icuve} required for this function` =
              requireNamespace("covid19.icuve"))

  icuve_sitrep <- covid19.icuve::fetch_gsheet(usr_email = "dario.gregori@unipd.it") |>
    dplyr::select(.data$date, .data$covid_occupied)

  veneto <- dpc_covid19_ita_regioni %>%
    dplyr::filter(.data$denominazione_regione == "Veneto") %>%
    dplyr::select(.data$data, .data$terapia_intensiva) %>%
    dplyr::mutate(data = lubridate::as_date(.data$data))


  delta_icu <- veneto |>
    dplyr::inner_join(icuve_sitrep, by = c("data" = "date")) |>
    dplyr::mutate(
      delta_icu = .data$covid_occupied - .data$terapia_intensiva,
      prop_icu = .data$delta_icu / .data$covid_occupied
    )

  gg_compare <- delta_icu |>
    tidyr::pivot_longer(
      c(covid_occupied, terapia_intensiva),
      names_to = "Source"
    ) |>
    dplyr::mutate(
      Source = .data$Source |>
        stringr::str_replace_all(c(
          covid_occupied = "G-sheet",
          terapia_intensiva = "GitHub's DPC"
        ))
    ) |>
    ggplot(aes(data, value, colour = Source)) +
    geom_point() +
    labs(
      y = "Beds (delta)",
      title = "Actual ICU covid-19's beds occupied and official data provided.",
      subtitle = "Actual data from G-sheet, official data from DPC-GitHub."
    )

  gg_delta <- ggplot(delta_icu, aes(data, delta_icu)) +
    geom_point(aes(color = delta_icu > 0)) +
    labs(
      y = "Beds (delta)",
      colour = "More than official",
      title = "Difference from actual ICU covid-19's beds occupied and official data provided.",
      subtitle = "Actual data from G-sheet, official data from DPC-GitHub."
    )

  gg_prop <- ggplot(delta_icu, aes(data, prop_icu)) +
    geom_point(aes(color = prop_icu > 0)) +
    labs(
      y = "Proportion (delta / actual)",
      colour = "More than official",
      title = "Relative (to actual) difference from actual ICU covid-19's beds occupied and official data provided.",
      subtitle = "Actual data from G-sheet, official data from DPC-GitHub."
    )

  moduleServer( id, function(input, output, session) {
    ns <- session$ns

    output$gg_compare_icu <- renderPlotly({
      plotly::ggplotly(gg_compare) %>%
        plotly::config(modeBarButtonsToRemove = c(
          "zoomIn2d", "zoomOut2d", "pan2d", "select2d", "lasso2d")) %>%
        plotly::config(displaylogo = FALSE)
    })

    output$gg_delta_icu <- renderPlotly({
      plotly::ggplotly(gg_delta) %>%
        plotly::config(modeBarButtonsToRemove = c(
          "zoomIn2d", "zoomOut2d", "pan2d", "select2d", "lasso2d")) %>%
        plotly::config(displaylogo = FALSE)
    })

    output$gg_prop_icu <- renderPlotly({
      plotly::ggplotly(gg_prop) %>%
        plotly::config(modeBarButtonsToRemove = c(
          "zoomIn2d", "zoomOut2d", "pan2d", "select2d", "lasso2d")) %>%
        plotly::config(displaylogo = FALSE)
    })

  })
}

## To be copied in the UI
# mod_discrepancies_icuve_ui("discrepancies_icuve_ui_1")

## To be copied in the server
# mod_discrepancies_icuve_server("discrepancies_icuve_ui_1")
