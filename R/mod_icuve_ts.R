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
        da pazienti Covid aggiustato per proporzione di tamponi positivi.
        Andamento osservato (punti blu) fino alla data odierna."
      )
    ),
    fluidRow(
      box(
        width = 12,
        plotly::plotlyOutput(
          ns("fig3")
        ),
        title = "Figure 3. Relazione stimata (linea rossa in grassetto,
        l'area rossa indica gli intervalli di confidenza al 95%) tra
        numero di posti letto totali in terapia intensiva occupati
        da pazienti Covid e proporzione di tamponi positivi."
      )
    ),
    fluidRow(
      box(
        width = 12,
        plotly::plotlyOutput(ns("fig4")),
        title = "Figure 4. Andamento stimato (linea rossa in grassetto,
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

  icuve_ts <- covid19.icuve::fetch_gsheet(
    "ts", usr_email = "luca.vedovelli@unipd.it"
  )

  # 1) Prepare the data ------------------------------------------------
  # Time series Veneto -------------------------------------------------
  veneto_ts <- dpc_covid19_ita_regioni %>%
    # Select the Veneto data
    dplyr::filter(.data$denominazione_regione == "Veneto") %>%
    # Rename the column date the make it consistent with the other db
    dplyr::rename(date = data) %>%
    # Take only the date, the positive and the swabs
    dplyr::select(.data$date, .data$totale_casi, .data$tamponi) %>%
    # dplyr::select(date, prop_pos) %>%
    dplyr::mutate(
      date = lubridate::as_date(.data$date),
      lag_casi = dplyr::lag(.data$totale_casi, n = 1L, default = NA),
      lag_swab = dplyr::lag(.data$tamponi, n = 1L, default = NA),
      casi = .data$totale_casi - .data$lag_casi,
      swab = .data$tamponi - .data$lag_swab,
      prop_pos = .data$casi/.data$swab
    ) %>%
    # Take the 1st of September as starting date for the models
    dplyr::filter(.data$date >= lubridate::ymd("2020-09-01")) %>%
    dplyr::select(.data$date, .data$prop_pos)

  # Time series data Veneto ICU ----------------------------------------
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
    dplyr::filter(.data$date >= lubridate::ymd("2020-09-01")) %>%
    # Join the datasets
    dplyr::left_join(veneto_ts, by = "date")

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
        dplyr::mutate_at(
          dplyr::vars(-.data$date),
          ~ NA_integer_
        )
    ) %>%
    # If the prop of positive swabs is missing take the last available
    dplyr::mutate(
      prop_pos = dplyr::if_else(
        is.na(.data$prop_pos),
        dplyr::last(stats::na.omit(.data$prop_pos)),
        .data$prop_pos
      )
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
    coord_cartesian(ylim = c(0, 0.7)) +
    theme(
      axis.text.x = element_text(
        angle = 60, hjust = 1, vjust = 0.5
      ),
      panel.spacing.y = unit(2, "lines")
    ) +
    xlab("") +
    ylab("Proporzione")

  # 4) COVID beds occupied adjusted by proportion of positive ----------
  beds_df <- df %>%
    dplyr::mutate(
      prop_pos = dplyr::if_else(
        is.na(.data$prop_pos),
        dplyr::last(stats::na.omit(.data$prop_pos)),
        .data$prop_pos
      )
    )

  fit_beds <- mgcv::gam(
    stats::as.formula(
      "covid_occupied ~ s(as.numeric(date)) + s(prop_pos)"
    ),
    data = beds_df,
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
    coord_cartesian(
      xlim = c(min(df$date), max(df$date) + 7),
      ylim = c(0, 250)
    ) +
    scale_x_date(date_breaks = "1 week", date_labels = "%d %b") +
    theme(
      axis.text.x = element_text(
        angle = 60, hjust = 1, vjust = 0.5
      )
    ) +
    xlab("") +
    ylab("Numero posti letto")

  # 5) COVID beds and by proportion of positive ------------------------
  swab_df <- df %>%
    dplyr::select(.data$covid_occupied, .data$prop_pos) %>%
    stats::na.omit()

  fit_swab <- mgcv::gam(
    stats::as.formula("covid_occupied ~ s(prop_pos)"),
    data = swab_df,
    family = stats::poisson(link = "log")
  )

  pred_swab <- stats::predict(fit_swab, swab_df, se.fit = TRUE)
  pred_swab_df <- tibble::tibble(
    prop_pos = swab_df$prop_pos,
    y_hat = as.double(exp(pred_swab$fit)),
    y_lower = as.double(exp(pred_swab$fit - 1.96 * pred_swab$se.fit)),
    y_upper = as.double(exp(pred_swab$fit + 1.96 * pred_swab$se.fit)),
  )

  ggswab <- ggplot(
    data = swab_df,
    mapping = aes(x = .data$prop_pos)
  ) +
    geom_line(
      data = pred_swab_df,
      mapping = aes(y = y_hat, color = "Atteso"),
      size = 1.2
    ) +
    geom_ribbon(
      data = pred_swab_df,
      mapping = aes(ymin = y_lower, ymax = y_upper),
      alpha = 0.33, fill = "firebrick2", colour = NA
    ) +
    geom_point(
      mapping = aes(y = .data$covid_occupied, colour = "Osservato"),
      size = 1.8
    ) +
    coord_cartesian(
      xlim = c(
        stats::quantile(swab_df$prop_pos, 0.1, na.rm = TRUE),
        stats::quantile(swab_df$prop_pos, 0.9, na.rm = TRUE)
      )
    ) +
    scale_color_manual(
      name = "",
      values = c("Atteso" = "firebrick2", "Osservato" = "dodgerblue1")
    ) +
    xlab("Proporzione tamponi positivi") +
    ylab("Numero posti letto")

  # 6) Delta days ------------------------------------------------------
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
    coord_cartesian(xlim = c(min(df$date), max(df$date) + 5)) +
    scale_x_date(
      # limits = c(min(df$date), max(df$date) + 5),
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
        plotly::ggplotly(ggswab)
      })

      output$fig4 <- plotly::renderPlotly({
        plotly::ggplotly(ggdelta_days)
      })

    })
}

## To be copied in the UI
# mod_icuve_ts_ui("icuve_ts_cl")

## To be copied in the server
# mod_icuve_ts_server("icuve_ts_cl")

