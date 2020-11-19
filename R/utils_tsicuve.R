holter_plot <- function(data, n_ahead, d, tstart) {

  assertive::assert_is_data.frame(data)
  assertive::assert_is_integer(n_ahead)
  assertive::assert_is_integer(d)
  assertive::assert_is_date(tstart)

  # Maximum date for TS model fitting
  tstop <- tstart + d

  # Select the time series for model fitting
  ts_fit <- data %>%
    dplyr::filter(.data$data >= tstart & .data$data <= tstop)

  # Retrieve the time-series
  my_ts <- stats::ts(
    ts_fit[["terapia_intensiva"]],
    start = c(2020, as.numeric(format(min(ts_fit[["data"]]), "%j"))),
    end = c(2020, as.numeric(format(max(ts_fit[["data"]]), "%j")))
  )

  # Fit the model
  hw_object <- stats::HoltWinters(my_ts, gamma = F)

  pred <- forecast::forecast(hw_object, n_ahead)

  # Prepare the dataframes for the TS plot
  na_ahead <- length(data[["data"]]) - d - n_ahead

  obs_df <- data %>%
    dplyr::rename(est = .data$terapia_intensiva) %>%
    dplyr::mutate(lower = NA_real_, upper = NA_real_) %>%
    dplyr::mutate(type = "obs")

  fitted_df <- tibble::tibble(
    data = data[["data"]],
    est = c(
      NA_real_, NA_real_,
      as.double(hw_object$fitted[, 1]),
      as.double(pred$mean),
      rep(NA_real_, na_ahead - 1)
    ),
    lower = c(
      NA_real_, NA_real_,
      rep(NA_real_, length(as.double(hw_object$fitted[, 1]))),
      pred$lower[, 2],
      rep(NA_real_, na_ahead - 1)
    ),
    upper = c(
      NA_real_, NA_real_,
      rep(NA_real_, length(as.double(hw_object$fitted[, 1]))),
      pred$upper[, 2],
      rep(NA_real_, na_ahead - 1)
    ),
    type = "expected"
  )

  plot_df <- dplyr::bind_rows(obs_df, fitted_df) %>%
    dplyr::mutate(
      type = factor(
        .data$type,
        levels = c("obs", "expected")
      )
    ) %>%
    # If upper or lower are less than 0 put 0
    dplyr::mutate(
      lower = dplyr::if_else(.data$lower < 0, 0, .data$lower),
      upper = dplyr::if_else(.data$upper < 0, 0, .data$upper)
    )

  # TS plot
  ggplot(
    data = plot_df,
    mapping = aes(
      x = .data$data, y = .data$est, colour = .data$type,
      fill = .data$type
    )
  ) +
    geom_line(size = 1.1) +
    geom_ribbon(
      mapping = aes(ymin = lower, ymax = upper), alpha = 0.2,
      colour = NA
    ) +
    scale_color_manual(
      name = "",
      values = c("dodgerblue1", "firebrick2"),
      labels = c("Osservato", "Atteso")
    ) +
    scale_fill_manual(
      name = "",
      values = c("dodgerblue1", "firebrick2"),
      labels = c("Osservato", "Atteso")
    ) +
    geom_vline(xintercept = tstop, linetype = "dashed") +
    ylab("Numero posti letto TI") +
    xlab("") +
    scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
    theme(
      axis.text.x = element_text(
        angle = 60, hjust = 1, vjust = 0.5
      )
    )

}

holter_error <- function(data, n_ahead, d, tstart) {

  assertive::assert_is_data.frame(data)
  assertive::assert_is_integer(n_ahead)
  assertive::assert_is_integer(d)
  assertive::assert_is_date(tstart)

  # Maximum date for TS model fitting
  tstop <- tstart + d

  # Select the time series for model fitting
  ts_fit <- data %>%
    dplyr::filter(.data$data >= tstart & .data$data <= tstop)

  # Retrieve the time-series
  my_ts <- stats::ts(
    ts_fit[["terapia_intensiva"]],
    start = c(2020, as.numeric(format(min(ts_fit[["data"]]), "%j"))),
    end = c(2020, as.numeric(format(max(ts_fit[["data"]]), "%j")))
  )

  # Fit the model
  hw_object <- stats::HoltWinters(my_ts, gamma = F)

  pred <- forecast::forecast(hw_object, n_ahead)

  # Construct data for error
  expected <- round(c(hw_object$fitted[, 1], pred$mean))

  tt <- data %>%
    dplyr::filter(.data$data <= max(ts_fit$data) + n_ahead)
  obs <- tt$terapia_intensiva[-c(1, 2)]

  # Squared error for count data
  sq_err <- tscount::scoring(expected, obs)[[7]]

  # Final result into a tibble
  tibble::tibble(
    data = max(ts_fit$data) + n_ahead,
    error = sq_err
  )

}
