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

  obs_df <- dplyr::rename(data, est = .data$terapia_intensiva)

  obs <- data %>%
    dplyr::filter(.data$data <= max(ts_fit$data) + n_ahead) %>%
    dplyr::pull(.data$terapia_intensiva)

  list(
    ts_fit = ts_fit, my_ts = my_ts, obs_df = obs_df, obs = obs[-c(1, 2)]
  )
}

ts_plot <- function(hw_fitted, pred, aux_objs, n_ahead, tstart, tstop) {
  fitted_df <- tibble::tibble(
    data = seq(from = tstart, to = tstop + n_ahead, by = 1),
    est = round(c(hw_fitted, as.double(pred$mean)))
  ) %>%
    dplyr::mutate(
      lower = c(
        rep(NA_real_, length(hw_fitted)),
        as.double(pred$lower[, 2])
      ),
      upper = c(
        rep(NA_real_, length(hw_fitted)),
        as.double(pred$upper[, 2])
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
      data = aux_objs[["obs_df"]],
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

tbl_error <- function(hw_fitted, pred, aux_objs, n_ahead) {
  # Construct data for error
  expected <- round(c(hw_fitted, pred$mean))

  # Squared error for count data
  sq_err <- tscount::scoring(expected, aux_objs[["obs"]])[[7]]

  # Final result into a tibble
  tibble::tibble(
    data = max(aux_objs[["ts_fit"]]$data) + n_ahead,
    error = sq_err
  )

}

fit_partial_ts_model <- function(
  aux_objs, n_ahead, method = c("hw", "ets", "arima")
) {
  method <- match.arg(method)

  mod <- switch(method,
    hw = stats::HoltWinters(aux_objs[["my_ts"]], gamma = FALSE),
    ets = forecast::ets(aux_objs[["my_ts"]], damped = TRUE),
    forecast::auto.arima(aux_objs[["my_ts"]])
  )

  fit <- as.double(
    if (method == "hw") mod$fitted[, 1] else mod$fitted
  )
  pred <- forecast::forecast(mod, h = n_ahead)

  list(mod = mod, fit = fit, pred = pred)
}

partial_ts_plot <- function(
  data, n_ahead, d = NULL, tstart, tstop,
  method = c("hw", "ets", "arima")
) {
  method <- match.arg(method)

  aux_objs <- eval_aux_objs(data, n_ahead, d = d, tstart, tstop)
  mod <- fit_partial_ts_model(aux_objs, n_ahead, method)

  if (method == "hw") {
    tstart <- tstart + 2 # da eseguire dopo call to `eval_aux_objs()`
  }

  ts_plot(
    hw_fitted = mod[["fit"]], pred = mod[["pred"]],
    aux_objs, n_ahead, tstart, tstop
  )

}

holter_error <- function(data, n_ahead, d, tstart) {

  aux_objs <- eval_aux_objs(data, n_ahead, d, tstart)

  # Fit the model
  hw_object <- stats::HoltWinters(aux_objs[["my_ts"]], gamma = FALSE)
  hw_fitted <- as.double(hw_object$fitted[, 1])
  pred <- forecast::forecast(hw_object, n_ahead)

  tbl_error(hw_fitted, pred, aux_objs, n_ahead)
}

damped_plot <- function(data, n_ahead, tstart, tstop) {

  aux_objs <- eval_aux_objs(data, n_ahead, d = NULL, tstart, tstop)

  # Fit the model
  mod <- fit_partial_ts_model(aux_objs, n_ahead, "ets")

  ts_plot(
    mod[["mod"]], mod[["pred"]],
    aux_objs, n_ahead, tstart, tstop
  )
}

damped_error <- function(data, n_ahead, d, tstart) {

  aux_objs <- eval_aux_objs(data, n_ahead, d, tstart)

  # Fit the model
  hw_object <- forecast::ets(aux_objs[["my_ts"]], damped = TRUE)
  hw_fitted <- as.double(hw_object$fitted)
  pred <- forecast::forecast(hw_object,  h = n_ahead)

  tbl_error(hw_fitted, pred, aux_objs, n_ahead)
}

arima_plot <- function(data, n_ahead, tstart, tstop) {

  aux_objs <- eval_aux_objs(data, n_ahead, d = NULL, tstart, tstop)

  # Fit the model
  mod <- fit_partial_ts_model(aux_objs, n_ahead, "arima")

  ts_plot(
    mod[["mod"]], mod[["pred"]],
    aux_objs, n_ahead, tstart, tstop
  )
}

arima_error <- function(data, n_ahead, d, tstart) {

  aux_objs <- eval_aux_objs(data, n_ahead, d, tstart)

  # Fit the model
  hw_object <- forecast::auto.arima(aux_objs[["my_ts"]])
  hw_fitted <- as.double(hw_object$fitted)
  pred <- forecast::forecast(hw_object,  h = n_ahead)

  tbl_error(hw_fitted, pred, aux_objs, n_ahead)
}

