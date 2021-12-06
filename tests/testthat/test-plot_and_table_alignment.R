test_that("forcasting plots are aligned with tables", {
  # setup
  region <- dpc_covid19_ita_regioni |>
    dplyr::filter(.data$denominazione_regione == "Veneto") |>
    dplyr::select(
      .data$data,
      .data$terapia_intensiva,
      .data$ricoverati_con_sintomi
    ) |>
    dplyr::mutate(data = lubridate::as_date(.data$data)) |>
    dplyr::arrange(.data$data)

  n_ahead <- 7L
  tstart <- min(region[["data"]], na.rm = TRUE)
  tstop <- max(region[["data"]], na.rm = TRUE)


  # Execution
  table <- region |>
    partial_forecast(n_ahead, "ets_auto", tstop = tstop) |>
    tidyr::separate(
      `Ricoveri attesi [95% CI]`,
      c("est", "lower", "upper"),
      convert = TRUE,
      extra = "drop"
    ) |>
    dplyr::rename(data = Data)

  gg <- partial_ts_plot(
    region,
    n_ahead = n_ahead,
    tstart = tstart,
    tstop = tstop,
    method = "ets_auto"
  )[["data"]] |>
   dplyr::filter(
     .data[["data"]] %in% table[["data"]]
   ) |>
    dplyr::mutate(across(where(is.numeric), ~as.integer(round(.x))))



  # tests
  expect_equal(table, gg)

})
