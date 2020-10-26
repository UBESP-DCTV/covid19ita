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

  icuve_ts <- covid19.icuve::icuve_ts

  # 1) Prepare the data ------------------------------------------------
  df <- icuve_ts %>%
    dplyr::mutate(
      # Proportion of COVID beds
      prop_covid_occupied = .data$covid_occupied/.data$overall_total
    ) %>%
    # Take the 1st of September as starting date for the models
    dplyr::filter(.data$date >= lubridate::ymd("2020-09-01"))

  # 2) Prepare days ahead ----------------------------------------------
  days_ahead <- 30L
  seq_ahead <- lubridate::ymd(seq(
    max(df$date) + 1, max(df$date) + 1 + days_ahead, by = "1 day"
  ))

  df_days_ahead <- df %>%
    dplyr::bind_rows(
      df %>%
        dplyr::slice(1:length(seq_ahead)) %>%
        dplyr::mutate(date = seq_ahead) %>%
        dplyr::mutate_at(dplyr::vars(-.data$date), ~ NA_integer_)
    )

  # 3) Proportion of COVID beds ----------------------------------------
  fit_prop <- mgcv::gam(
    prop_covid_occupied ~ s(as.numeric(date)),
    data = df,
    family = mgcv::betar(link="logit")
  )

  pred_db <- df_days_ahead %>%
    dplyr::mutate(
      prop_pred = stats::predict(fit_prop, .),
      prop_se = stats::predict(fit_prop, ., se.fit = TRUE)[["se.fit"]]
    )

  ggprop <- ggplot2::ggplot(
    data = pred_db,
    mapping = ggplot2::aes(
      x = .data$date
    )
  ) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        y = stats::plogis(.data$prop_pred),
        color = "Atteso"
      ),
      size = 1.2
    ) +
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(
        ymin = stats::plogis(.data$prop_pred - 1.96 * .data$prop_se),
        ymax = stats::plogis(.data$prop_pred + 1.96 * .data$prop_se)
      ), alpha = 0.33, fill = "firebrick2", color = NA
    ) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        y = .data$prop_covid_occupied,
        color = "Osservato"
      ),
      size = 1.8
    ) +
    ggplot2::geom_hline(
      yintercept = 0.3, linetype = "dashed", colour = "black"
    ) +
    ggplot2::scale_color_manual(
      name = "",
      values = c("Atteso" = "firebrick2", "Osservato" = "dodgerblue1")
    ) +
    ggplot2::scale_x_date(
      date_breaks = "1 week", date_labels = "%d %b"
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(from = 0, to = 0.7, by = 0.1),
      labels = scales::percent
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 60, hjust = 1, vjust = 0.5
      ),
      panel.spacing.y = ggplot2::unit(2, "lines")
    ) +
    ggplot2::xlab("") +
    ggplot2::ylab("% posti letto occupati da pazienti COVID")






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

