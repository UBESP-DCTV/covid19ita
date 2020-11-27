eval_aux_objs <- function(
  data, n_ahead, d = NULL, tstart, tstop = tstart + d
) {

  assertive::assert_is_data.frame(data)
  assertive::assert_is_integer(n_ahead)
  if (!is.null(d)) assertive::assert_is_integer(d)
  assertive::assert_is_date(tstart)
  assertive::assert_is_date(tstop)

  # Select the time series for model fitting
  ts_fit <- data %>%
    dplyr::filter(.data$data >= tstart & .data$data <= tstop)

  # Retrieve the time-series
  my_ts <- stats::ts(
    ts_fit[["terapia_intensiva"]],
    start = c(2020, as.numeric(format(min(ts_fit[["data"]]), "%j"))),
    end = c(2020, as.numeric(format(max(ts_fit[["data"]]), "%j")))
  )

  icu_obs <- data %>%
    dplyr::filter(.data$data <= max(ts_fit$data) + n_ahead) %>%
    dplyr::pull(.data$terapia_intensiva)

  list(
    ts_fit = ts_fit, my_ts = my_ts, obs_df = data, icu_obs = icu_obs[-c(1, 2)]
  )
}

ts_plot <- function(fit, pred, aux_objs, n_ahead, tstart, tstop, method
) {

  fitted_df <- tibble::tibble(
    data = seq(from = tstart, to = tstop + n_ahead, by = 1),
    est = round(c(fit, as.double(pred$mean)))
  ) %>%
    dplyr::mutate(
      lower = c(.data$est[seq_along(fit)], as.double(pred$lower[, 2])),
      upper = c(.data$est[seq_along(fit)], as.double(pred$upper[, 2]))
    ) %>%
    # If upper or lower are less than 0 put 0
    dplyr::mutate(
      lower = dplyr::if_else(.data$lower < 0, 0, .data$lower),
      upper = dplyr::if_else(.data$upper < 0, 0, .data$upper)
    )


  # TS plot
  ggres <- ggplot(
    data = fitted_df,
    mapping = aes(x = .data$data)
  ) +
    geom_line(
      mapping = aes(y = .data$est, colour = "Atteso"),
      size = 1.5
    ) +
    geom_line(
      data = aux_objs[["obs_df"]],
      mapping = aes(y = .data$terapia_intensiva, color = "Osservato"),
      size = 0.8
    ) +
    geom_ribbon(
      mapping = aes(ymin = .data$lower, ymax = .data$upper),
      alpha = 0.2, fill = "firebrick2", colour = NA
    ) +
    scale_color_manual(
      name = "",
      values = c("Atteso" = "firebrick2", "Osservato" = "dodgerblue1")
    ) +
    ylab("Numero ricoveri terapia intensiva") +
    xlab("") +
    scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
    theme(
      axis.text.x = element_text(
        angle = 60, hjust = 1, vjust = 0.5
      )
    )

  if (!is.null(method)) {
    ggres <- ggres +
      annotate("text",
               x = median(aux_objs[["obs_df"]]$data, na.rm = TRUE),
               y = max(aux_objs[["obs_df"]]$terapia_intensiva, na.rm = TRUE),
               label = paste0("Methods: ", method),
               vjust = "inward", hjust = "inward"
      )
  }

  ggres
}

tbl_error <- function(fit, pred, aux_objs, n_ahead) {
  # Construct data for error
  expected <- round(c(fit, pred$mean))

  # Squared error for count data
  sq_err <- tscount::scoring(expected, aux_objs[["icu_obs"]])[[7]]

  # Final result into a tibble
  tibble::tibble(
    data = max(aux_objs[["ts_fit"]]$data) + n_ahead,
    error = sq_err
  )

}

fit_partial_ts_model <- function(
  aux_objs, n_ahead, method = c("hw", "ets", "arima", "ets_auto")
) {
  method <- match.arg(method)

  mod <- switch(method,
    hw = stats::HoltWinters(aux_objs[["my_ts"]], gamma = FALSE),
    ets = forecast::ets(aux_objs[["my_ts"]], damped = TRUE),
    forecast::auto.arima(aux_objs[["my_ts"]]),
    ets_auto = forecast::ets(aux_objs[["my_ts"]])
  )

  fit <- as.double(
    if (method == "hw") mod$fitted[, 1] else mod$fitted
  )
  pred <- forecast::forecast(mod, h = n_ahead)

  list(mod = mod, fit = fit, pred = pred)
}

partial_ts_plot <- function(
  data, n_ahead, d = NULL, tstart, tstop = tstart + d,
  method = c("hw", "ets", "arima", "ets_auto")
) {
  method <- match.arg(method)

  aux_objs <- eval_aux_objs(data, n_ahead, d = d, tstart, tstop)

  mod <- fit_partial_ts_model(aux_objs, n_ahead, method)

  if (method == "hw") {
    tstart <- tstart + 2 # da eseguire dopo call to `eval_aux_objs()`
  }

  ts_plot(
    mod[["fit"]], mod[["pred"]], aux_objs, n_ahead, tstart, tstop,
    mod[["mod"]][["method"]]
  )

}

partial_ts_error <- function(
  data, n_ahead, d, tstart,
  method = c("hw", "ets", "arima", "ets_auto")
) {
  method <- match.arg(method)

  aux_objs <- eval_aux_objs(data, n_ahead, d, tstart)
  mod <- fit_partial_ts_model(aux_objs, n_ahead, method)
  tbl_error(mod[["fit"]], mod[["pred"]], aux_objs, n_ahead)
}


ts_plot_error <- function(df_error) {
  ggplot(
    data = df_error,
    mapping = aes(x = .data$data, y = .data$error)
  ) +
    geom_point(size = 1.1) +
    geom_smooth(se = FALSE) +
    ylab("Squared error") +
    xlab("") +
    scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.5)
    )
}

partial_forecast <- function(
  data, n_ahead, method = c("hw", "ets", "arima", "ets_auto")
) {
  method <- match.arg(method)

  aux_objs <- eval_aux_objs(
    data, n_ahead, tstart = min(data$data), tstop = max(data$data)
  )

  mod <- fit_partial_ts_model(aux_objs, n_ahead, method)

  fit <- as.double(
    if (method == "hw") mod$mod$fitted[, 1] else mod$mod$fitted
  )
  pred <- forecast::forecast(mod$mod, h = n_ahead)

  y_hat <- round(pred$mean)
  lower <- round(pred$lower[, 2])
  upper <- round(pred$upper[, 2])

  tibble::tibble(
    Data = seq(
      from = max(data$data) + 1, to = max(data$data) + n_ahead, by = 1
    ),
    `Ricoveri attesi [95% CI]` = glue::glue(
      "{y_hat} [{lower} - {upper}]"
    )
  )

}


