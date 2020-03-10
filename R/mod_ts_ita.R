#' ts_ita UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList titlePanel fluidRow h1 h2 h3
#' @importFrom plotly plotlyOutput
mod_ts_ita_ui <- function(id){
  ns <- NS(id)
  fluidRow(plotlyOutput(ns("ts_plot")))
}

#' ts_ita Server Function
#'
#' @import ggplot2
#' @importFrom plotly renderPlotly ggplotly
#' @noRd
mod_ts_ita_server <- function(id, type = c("cum", "inc")) {
  type <- match.arg(type)

  var_of_interest <- names(dpc_covid19_ita_andamento_nazionale) %>%
    setdiff(c("stato", "tamponi", "nuovi_attualmente_positivi"))

  ts_ita_to_plot <- dpc_covid19_ita_andamento_nazionale[var_of_interest] %>%
    dplyr::mutate(data = as.Date(data)) %>%
    tidyr::pivot_longer(-.data$data,
      names_to = "Measure",
      values_to = "N"
    ) %>%
    dplyr::mutate(
      Measure = factor(.data$Measure,
        levels = var_of_interest,
        labels = var_of_interest %>%
          stringr::str_replace_all("_", " ") %>%
          stringr::str_to_title()
      )
    )

  y_lab <- "Data"

  if (type == "inc") {
    ts_ita_to_plot <- ts_ita_to_plot %>%
      dplyr::group_by(Measure) %>%
      dplyr::arrange(data) %>%
      dplyr::mutate(N = N - dplyr::lag(N)) %>%
      dplyr::ungroup()

    y_lab <- paste(y_lab, "(differences)")
  }

  gg <- ts_ita_to_plot %>%
    ggplot(aes(x = data, y = N, colour = Measure)) +
    geom_line() + geom_point() +
    xlab("Data") + ylab(y_lab) +
    scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
    )

  callModule(id = id, function(input, output, session) {
    ns <- session$ns
    output$ts_plot <- renderPlotly(ggplotly(gg))
  })
}

## To be copied in the UI
# mod_ts_ita_ui("time_series_ui_1")

## To be copied in the server
# callModule(mod_ts_ita_server, "time_series_ui_1")

