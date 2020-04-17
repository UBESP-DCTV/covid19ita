#' Extract region adding population
#'
#' From the official regional [dpc_covid19_ita_regioni] data, pull out
#' records from a sigle region, and add a column reporting that
#' region population
#'
#' @param region (chr) name of the region to extract
#'
#' @return a [tibble][tibble::tibble-package]
#'
#'
#' @examples
#' covid19ita:::pull_region_w_pop("veneto")
pull_region_w_pop <- function(region) {
  dpc_covid19_ita_regioni %>%
    dplyr::filter(
      .data$denominazione_regione == region
    ) %>%
    dplyr::mutate(
      day = lubridate::ymd_hms(.data$data),
      days = seq_along(.data$data),
      pop = dplyr::filter(
        region_population,
        .data$denominazione_regione == region
      )[["residenti"]]
    )
}
