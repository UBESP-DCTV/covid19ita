#' icuve_ts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS fluidRow fluidPage
#' @importFrom shinydashboard box
mod_icuve_ts_ui <- function(id){
  ns <- NS(id)

  fluidPage(
    fluidRow(
      box(
        width = 12,
        plotly::plotlyOutput(ns("fig1")),
        title = "Figure 1. Andamento stimato (linea rossa in grassetto,
        l'area rossa indica gli intervalli di confidenza al 95%) delle
        proporzioni di posti letto totali in terapia intensiva occupati
        da pazienti Covid. Andamento osservato (punti blu) fino alla
        data odierna."
      )
    ),
    fluidRow(
      box(
        width = 12,
        plotly::plotlyOutput(ns("fig2")),
        title = "Figure 2. Andamento stimato (linea rossa in grassetto,
        l'area rossa indica gli intervalli di confidenza al 95%) del
        numero di posti letto totali in terapia intensiva occupati
        da pazienti Covid. Andamento osservato (punti blu) fino alla
        data odierna."
      )
    ),
    fluidRow(
      box(
        width = 12,
        plotly::plotlyOutput(ns("fig3")),
        title = "Figure 3. Andamento stimato (linea rossa in grassetto,
        l'area rossa indica gli intervalli di confidenza al 95%) della
        differenza tra il numero di posti letto in terapia intensiva
        in data odierna e 3 giorni precedenti. Andamento osservato
        (punti blu) fino alla data odierna."
      )
    )
  )
}

#' icuve_ts Server Function
#'
#' @import ggplot2
#' @import covid19.icuve
#' @import mgcv
#' @noRd
mod_icuve_ts_server <- function(id) {

  icuve_ts <- covid19.icuve::fetch_gsheet()

  # 1) Prepare the data ------------------------------------------------
  df <- icuve_ts %>%
    dplyr::mutate(
      # Proportion of COVID beds
      prop_covid_occupied = .data$covid_occupied/.data$overall_total,
      # Covid variation as 3 days before
      covid_occ_lag = dplyr::lag(
        x = .data$covid_occupied, n = 3L, default = 0
      ),
      covid_variation = .data$covid_occupied - .data$covid_occ_lag
    ) %>%
    # Take the 1st of September as starting date for the models
    dplyr::filter(.data$date >= lubridate::ymd("2020-09-01"))

  # 2) Prepare days ahead ----------------------------------------------
  days_ahead <- 20L
  seq_ahead <- lubridate::ymd(seq(
    max(df$date) + 1, max(df$date) + days_ahead, by = "1 day"
  ))

  df_days_ahead <- df %>%
    dplyr::bind_rows(
      df %>%
        dplyr::slice(seq_along(seq_ahead)) %>%
        dplyr::mutate(date = seq_ahead) %>%
        dplyr::mutate_at(dplyr::vars(-.data$date), ~ NA_integer_)
    )

  # 3) Proportion of COVID beds ----------------------------------------
  fit_prop <- mgcv::gam(
    prop_covid_occupied ~ s(as.numeric(date)),
    data = df,
    family = stats::quasibinomial(link = "logit")
  )

  pred_db_prop <- df_days_ahead %>%
    dplyr::mutate(
      prop_pred = stats::predict(fit_prop, .),
      prop_se = stats::predict(fit_prop, ., se.fit = TRUE)[["se.fit"]]
    )

  ggprop <- ggplot(
    data = pred_db_prop, mapping = aes(x = .data$date)
  ) +
    geom_line(
      mapping = aes(y = stats::plogis(.data$prop_pred), color = "Atteso"),
      size = 1.2
    ) +
    geom_ribbon(
      mapping = aes(
        ymin = stats::plogis(.data$prop_pred - 1.96 * .data$prop_se),
        ymax = stats::plogis(.data$prop_pred + 1.96 * .data$prop_se)
      ), alpha = 0.33, fill = "firebrick2", color = NA
    ) +
    geom_point(
      mapping = aes(y = .data$prop_covid_occupied, color = "Osservato"),
      size = 1.8
    ) +
    geom_hline(yintercept = 0.3, linetype = "dashed", colour = "black") +
    scale_color_manual(
      name = "",
      values = c("Atteso" = "firebrick2", "Osservato" = "dodgerblue1")
    ) +
    scale_x_date(date_breaks = "1 week", date_labels = "%d %b") +
    scale_y_continuous(
      breaks = seq(from = 0, to = 0.7, by = 0.1)
    ) +
    theme(
      axis.text.x = element_text(
        angle = 60, hjust = 1, vjust = 0.5
      ),
      panel.spacing.y = unit(2, "lines")
    ) +
    xlab("") +
    ylab("Proporzione")

  # 4) COVID beds occupied ---------------------------------------------
  fit_beds <- mgcv::gam(
    covid_occupied ~ s(as.numeric(date), bs = "cr"),
    data = df,
    family = stats::poisson(link = "log")
  )

  pred_db_beds <- df_days_ahead %>%
    dplyr::mutate(
      beds_pred = stats::predict(fit_beds, .),
      beds_se = stats::predict(fit_beds, ., se.fit = TRUE)[["se.fit"]]
    )

  ggbeds <- ggplot(
    data = pred_db_beds, mapping = aes(x = .data$date)
  ) +
    geom_line(
      mapping = aes(y = exp(.data$beds_pred), color = "Atteso"),
      size = 1.2
    ) +
    geom_ribbon(
      mapping = aes(
        ymin = exp(.data$beds_pred - 1.96 * .data$beds_se),
        ymax = exp(.data$beds_pred + 1.96 * .data$beds_se)
      ), alpha = 0.33, fill = "firebrick2", color = NA
    ) +
    geom_point(
      mapping = aes(y = .data$covid_occupied, color = "Osservato"),
      size = 1.8
    ) +
    scale_color_manual(
      name = "",
      values = c("Atteso" = "firebrick2", "Osservato" = "dodgerblue1")
    ) +
    scale_x_date(
      limits = c(min(df$date), max(df$date) + 10),
      date_breaks = "1 week", date_labels = "%d %b"
    ) +
    theme(
      axis.text.x = element_text(
        angle = 60, hjust = 1, vjust = 0.5
      ),
      panel.spacing.y = unit(2, "lines")
    ) +
    xlab("") +
    ylab("Numero posti letto")

  # 5) Delta days ------------------------------------------------------
  fit_delta_days <- stats::loess(
    covid_variation ~ as.numeric(date),
    data = df,
    control = stats::loess.control(surface = "direct")
  )

  pred_db_delta_days <- df_days_ahead %>%
    dplyr::mutate(
      delta_pred = stats::predict(fit_delta_days, .),
      delta_se = stats::predict(fit_delta_days, ., se = TRUE)[["se.fit"]]
    )

  ggdelta_days <- ggplot(
    data = pred_db_delta_days,
    mapping = aes(x = .data$date)
  ) +
    geom_line(
      mapping = aes(y = .data$delta_pred, color = "Atteso"),
      size = 1.2
    ) +
    geom_ribbon(
      mapping = aes(
        ymin = .data$delta_pred - 1.96 * .data$delta_se,
        ymax = .data$delta_pred + 1.96 * .data$delta_se
      ), alpha = 0.33, fill = "firebrick2", color = NA
    ) +
    geom_point(
      mapping = aes(y = .data$covid_variation, color = "Osservato"),
      size = 1.8
    ) +
    scale_color_manual(
      name = "",
      values = c("Atteso" = "firebrick2", "Osservato" = "dodgerblue1")
    ) +
    scale_x_date(
      limits = c(min(df$date), max(df$date) + 5),
      date_breaks = "1 week",
      date_labels = "%d %b"
    ) +
    theme(
      axis.text.x = element_text(
        angle = 60, hjust = 1, vjust = 0.5
      ),
      panel.spacing.y = unit(2, "lines")
    ) +
    ylab("Differenza rispetto ai 3 giorni precedenti") +
    xlab("")


    callModule(id = id, function(input, output, session) {
      ns <- session$ns

      output$fig1 <- plotly::renderPlotly({
        plotly::ggplotly(ggprop)
      })

      output$fig2 <- plotly::renderPlotly({
        plotly::ggplotly(ggbeds)
      })

      output$fig3 <- plotly::renderPlotly({
        plotly::ggplotly(ggdelta_days)
      })

    })
}

## To be copied in the UI
# mod_icuve_ts_ui("icuve_ts_cl")

## To be copied in the server
# mod_icuve_ts_server("icuve_ts_cl")

