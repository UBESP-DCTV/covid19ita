#' icuve_ts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_icuve_ts_ui <- function(id){
  ns <- NS(id)
  plotlyOutput(ns("gg_icuve_ts"))
}

#' icuve_ts Server Function
#'
#' @import ggplot2
#' @import covid19.icuve
#' @noRd
mod_icuve_ts_server <- function(id) {

  icuve_ts <- covid19.icuve::fetch_gsheet()

  icuve_ts_long <- icuve_ts %>%
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


  pred_db <- icuve_ts_long %>%
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
          date = as.Date("2020-09-01"):(max(.x$date) + 30L),
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





  gg <- icuve_ts_long %>%
    dplyr::filter(.data$date >= as.Date("2020-09-01")) %>%
    ggplot(aes(x = .data$date,
               y = .data$`N beds`,
               colour = .data$type,
               fill = .data$type)) +
    geom_point() +
    geom_ribbon(data = pred_db,
                aes(ymin = .data$`N beds` - .data$se,
                    ymax = .data$`N beds` + .data$se),
                alpha = 0.33) +
    geom_hline(yintercept = 400, linetype = "dashed", colour = "red") +
    geom_hline(yintercept = 500, linetype = "dashed", colour = "black") +
    scale_x_date(date_breaks = "1 weeks",
                 date_labels = "%d %b") +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.5),
      panel.spacing.y = unit(2, "lines")
    ) +
    ggtitle("Andamento posti letto Terapie intensive Veneto.

      Fit proiezione loess (span = 0.75, degree = 2)
      Tratteggio a 400 (rosso) e 500 (nero) posti letto") +
    ylab("Numero posti letto") +
    xlab("")



  callModule(id = id, function(input, output, session) {
  ns <- session$ns
  output$gg_icuve_ts <- renderPlotly(
    plotly::ggplotly(gg) %>%
      plotly::config(modeBarButtonsToRemove = c(
        "zoomIn2d", "zoomOut2d", "pan2d", "select2d", "lasso2d")) %>%
      plotly::config(displaylogo = FALSE)
  )
  })
}

## To be copied in the UI
# mod_icuve_ts_ui("icuve_ts_cl")

## To be copied in the server
# mod_icuve_ts_server("icuve_ts_cl")

