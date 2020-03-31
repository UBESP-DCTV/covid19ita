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
eng_mod_ts_ita_ui <- function(id, title, width = 12){
  ns <- NS(id)
  fluidPage(
    fluidRow(shiny::checkboxInput(ns("y_log"), "Logarithmic scale")),
  fluidRow(
    box(title = title, width = width,
      plotlyOutput(ns("ts_plot"))
    )
  )
 )
}

#' ts_ita Server Function
#'
#' @import ggplot2
#' @importFrom plotly renderPlotly ggplotly config
#' @noRd
eng_mod_ts_ita_server <- function(id, type = c("cum", "inc")) {
  type <- match.arg(type)

  dpc_data <- dpc_covid19_ita_andamento_nazionale %>%
    dplyr::mutate(data = as.Date(.data$data))

  var_of_interest <- c("data", measures("national"))
  exclude_from_pivoting <- "data"

  ts_data_to_plot <- dpc_data[var_of_interest] %>%
    tidyr::pivot_longer( -{{exclude_from_pivoting}},
      names_to = "Measure",
      values_to = "N"
    ) %>%
    dplyr::mutate(
      Measure = factor(.data$Measure,
        levels = measures("national"),
        labels = measures("national") %>%
          measure_to_labels(lang = "eng")
    )
    )

  y_lab <- "N"

  if (type == "inc") {
    groups <- c("Measure")

    ts_data_to_plot <- ts_data_to_plot %>%
      dplyr::group_by_at(groups) %>%
      dplyr::arrange(.data$data) %>%
      dplyr::mutate(N = .data$N - dplyr::lag(.data$N)) %>%
      dplyr::ungroup()

    y_lab <- paste(y_lab, "(daily change)")
  }


  gg <- ts_data_to_plot %>%
    ggplot(
      aes(x = .data$data, y = .data$N, colour = .data$Measure)
    ) +
    geom_line() + geom_point() +
    xlab("Date") + ylab(y_lab) +
    scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
    scale_colour_discrete(name = "Misura") +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.5)
    )

  callModule(id = id, function(input, output, session) {
    ns <- session$ns
    output$ts_plot <- renderPlotly({
      if (input$y_log) {
        gg <- gg + scale_y_continuous(
          trans = 'log2',
          breaks = scales::trans_breaks("log2", function(x) 2^x),
          labels = scales::trans_format("log2", scales::math_format(2^.data[[".x"]]))
        ) +
          ylab(paste0(y_lab," - log2"))
      }

      ggplotly(gg) %>%
        config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "pan2d", "select2d", "lasso2d")) %>%
        config(displaylogo = FALSE)
    })
  })
}

## To be copied in the UI
# mod_ts_ita_ui("time_series_ui_1")

## To be copied in the server
# mod_ts_ita_server("time_series_ui_1", type = )

