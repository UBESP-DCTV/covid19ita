holter_plot <- function(data, n_ahead, tstart, tstop) {

  assertive::assert_is_data.frame(data)
  assertive::assert_is_integer(n_ahead)
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

  # Fit the model
  hw_object <- stats::HoltWinters(my_ts, gamma = F)

  pred <- forecast::forecast(hw_object, n_ahead)

  # Prepare the dataframes for the TS plot
  na_ahead <- length(data$data) -
    lubridate::interval(tstart, tstop)/lubridate::ddays(1) -
    n_ahead

  obs_df <- data %>%
    dplyr::rename(est = .data$terapia_intensiva) %>%
    dplyr::mutate(lower = NA_real_, upper = NA_real_) %>%
    dplyr::mutate(type = "obs")

  fitted_df <- tibble::tibble(
    data = seq(from = tstart + 2, to = tstop + n_ahead, by = 1),
    est = round(c(
      as.double(hw_object$fitted[, 1]),
      as.double(pred$mean)
      ))
    ) %>%
    dplyr::mutate(
      lower = c(
        rep(NA_real_, length(as.double(hw_object$fitted[, 1]))),
        pred$lower[, 2]
      ),
      upper = c(
        rep(NA_real_, length(as.double(hw_object$fitted[, 1]))),
        pred$upper[, 2]
      )
    ) %>%
    dplyr::mutate(
      lower = dplyr::if_else(
        is.na(.data$lower), .data$est, .data$lower
      ),
      upper = dplyr::if_else(
        is.na(.data$upper), .data$est, .data$upper
      ),
    ) %>%
    # If upper or lower are less than 0 put 0
    dplyr::mutate(
      lower = dplyr::if_else(.data$lower < 0, 0, .data$lower),
      upper = dplyr::if_else(.data$upper < 0, 0, .data$upper)
    )


  # TS plot
  ggplot(
    data = fitted_df,
    mapping = aes(x = .data$data)
  ) +
    geom_line(
      mapping = aes(y = .data$est, colour = "Atteso"),
      size = 1.5
    ) +
    geom_line(
      data = obs_df,
      mapping = aes(y = .data$est, color = "Osservato"),
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
