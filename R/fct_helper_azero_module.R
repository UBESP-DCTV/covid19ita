azero_data_path <- function() {
  Sys.getenv("AZ_PATH")
}


read_azero <- function(
  db = c("positivi", "ricoverati", "critica"),
  from = c("azure", "local")
) {
  db <- match.arg(db)
  from <- match.arg(from)

  switch(from,
    azure = read_azero_azure(db),
    local = read_azero_local(db)
  )
}


read_azero_local <- function(
  db = c("positivi", "ricoverati", "critica")
) {
  db <- match.arg(db)

  file.path(azero_data_path(), paste0(db, ".rds")) %>%
    readr::read_rds()
}

read_azero_azure <- function(
  db = c("positivi", "ricoverati", "critica")
) {
  db <- match.arg(db)

  data.azero::fetch_from_azure(db)
}


summarize_median_by <- function(df, .strata = NULL) {
  stopifnot(c("data", "permanenza_uo") %in% names(df))

  df %>%
    dplyr::group_by(dplyr::across(
      dplyr::all_of(c("data", .strata))
    )) %>%
    dplyr::mutate(
      permanenza_uo = as.double(.data[["permanenza_uo"]],
                                units = "days")
    ) %>%
    dplyr::summarise(
      valori = stats::median(.data[["permanenza_uo"]], na.rm = TRUE),
      provincia_domicilio = .data[["provincia_domicilio"]] %>%
        unique() %>%
        `[[`(1L)
    ) %>%
    dplyr::arrange(.data[["data"]]) %>%
    dplyr::ungroup()
}


extract_cum_by <- function(x, .strata = NULL) {
  x %>%
    dplyr::distinct(.data[["id_paziente"]], .keep_all = TRUE) %>%
    dplyr::group_by(dplyr::across(
      dplyr::all_of(c("data_primo_ingresso_ti", .strata))
    )) %>%
    dplyr::summarise(
      n = dplyr::n(),
      provincia_domicilio = .data[["provincia_domicilio"]] %>%
        unique() %>%
        `[[`(1L)
    ) %>%
    dplyr::arrange(.data[["data_primo_ingresso_ti"]]) %>%
    dplyr::ungroup()
}


fill_missing_dates <- function(x, date_var, .strata = NULL) {

  date_range <- range(x[[date_var]])

  full_dates <- tibble::tibble(
    {{date_var}} := seq(
      from = date_range[[1]],
      to = date_range[[2]],
      by = "1 day"
    )
  )

  x %>%
    tidyr::nest(data = -dplyr::any_of(.strata)) %>%
    dplyr::mutate(
      data = .data[["data"]] %>%
        purrr::map(~dplyr::full_join(.x, full_dates))
    ) %>%
    tidyr::unnest(cols = c(.data[["data"]])) %>%
    dplyr::arrange(.data[[date_var]])

}



add_mov_avr_k <- function(x, k, date_var, y_var, .strata = NULL) {
  checkmate::assertNames(
    c(date_var, y_var, .strata),
    subset.of = names(x)
  )

  x %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(.strata))) %>%
    dplyr::arrange(.data[[date_var]]) %>%
    dplyr::mutate(
      mov_avr = .data[[y_var]] %>%
        zoo::rollmean(k = k, fill = NA, align = "right")
    ) %>%
    dplyr::ungroup()

}

filter_checked <- function(db, var_to_filter = NULL) {
  db %>%
    dplyr::filter(
      dplyr::if_all(dplyr::all_of(var_to_filter),
                    ~ !is.na(.x) & .x)
    )
}


filter_or_checked <- function(db, var_to_or_filter = NULL) {
  db %>%
    dplyr::filter(
      dplyr::if_any(dplyr::all_of(var_to_or_filter),
                    ~ !is.na(.x) & .x)
    )
}



