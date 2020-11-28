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

  fluidPage(
    fluidRow(
      box(
        width = 12,
        plotly::plotlyOutput(ns("gg_icuve_sitrep")),
        title = "Dettaglio della situazione corrente (punti) e stima andamento a 15 giorni (linee curve) dei posti letto nelle terapie intensive venete.",
        footer = "Modello di proiezione loess (span = 0.75, degree = 2); bande di confidenza al 95%. Linee orizzontali tratteggiate a 400 (rosso) e 500 (nero) posti letto."
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

  icuve_sitrep_long <- icuve_sitrep %>%
    tidyr::pivot_longer(-date,
                        names_to = "type",
                        values_to = "N beds") %>%
    dplyr::filter(!.data$type %in% c("covid_new", "covid_discharged")) %>%
    dplyr::mutate(
      type = stringr::str_replace_all(.data$type,
                                      c(covid_dead = "CoViD-19 deaths",
                                        covid_occupied = "CoViD-19 beds occupied",
                                        covid_variation = "CoViD-19 beds variation",
                                        other_occupied = "Non-CoViD-19 beds occupied",
                                        overall_free = "Overall free beds",
                                        overall_occupied = "Overall beds occupied",
                                        overall_total = "Overall number of beds")
      )
    )


  pred_db <- icuve_sitrep_long %>%
    dplyr::group_by(.data$type) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      model = .data$data %>%
        purrr::map(~stats::loess(as.formula("`N beds` ~ date"),
                                 data = .x %>%
                                   dplyr::filter(.data$date >= as.Date("2020-09-01")) %>%
                                   dplyr::mutate(date = as.numeric(.data$date)),
                                 control = stats::loess.control(surface = "direct")
        )),
      res  = purrr::map2(.data$data, .data$model,
                         ~tibble::tibble(
                           date = as.Date("2020-09-01"):(max(.x$date) + 15L),
                           `N beds` = stats::predict(.y,
                                                     data.frame(date = as.numeric(.data$date))
                           ),
                           se = stats::predict(.y,
                                               data.frame(date = as.numeric(.data$date)),
                                               se = TRUE
                           )[["se.fit"]]
                         )
      )
    ) %>%
    dplyr::select(.data$type, .data$res) %>%
    tidyr::unnest(cols = c("res")) %>%
    dplyr::mutate(
      `N beds` = .data$`N beds`,
      date = as.Date(.data$date, origin = "1970-01-01")
    ) %>%
    dplyr::ungroup()





  gg <- icuve_sitrep_long %>%
    dplyr::filter(.data$date >= as.Date("2020-09-01")) %>%
    ggplot(aes(x = .data$date,
               y = .data$`N beds`,
               colour = .data$type,
               fill = .data$type)) +
    geom_point() +
    geom_ribbon(data = pred_db,
                aes(ymin = .data$`N beds` - 1.96*.data$se,
                    ymax = .data$`N beds` + 1.96*.data$se),
                alpha = 0.33) +
    geom_hline(yintercept = 400, linetype = "dashed", colour = "red") +
    geom_hline(yintercept = 500, linetype = "dashed", colour = "black") +
    scale_x_date(date_breaks = "3 days",
                 date_labels = "%d %b") +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.5),
      panel.spacing.y = unit(2, "lines")
    ) +
    ylab("Numero posti letto") +
    xlab("")



  callModule(id = id, function(input, output, session) {
    ns <- session$ns
    output$gg_icuve_sitrep <- renderPlotly(
      plotly::ggplotly(gg) %>%
        plotly::config(modeBarButtonsToRemove = c(
          "zoomIn2d", "zoomOut2d", "pan2d", "select2d", "lasso2d")) %>%
        plotly::config(displaylogo = FALSE)
    )
  })
}

## To be copied in the UI
# mod_icuve_sitrep_ui("sitrep")

## To be copied in the server
# mod_icuve_sitrep_server("sitrep")

