#' Evaluate mortality variation
#'
#' @param type (chr) "sex" or "age" stratification respectively
#'
#' @return a tibble including name, stratas, 2019 and 2020 overall death
#'   by strata, and the relative variation from 2019 to 2020
#' @export
#'
#' @examples
#' mort_data_reg("sex")
#' mort_data_reg("age")
mort_data_reg <- function(type = c("sex", "age")) {
  type <- match.arg(type)

  ref_data <- switch(type,
    "sex" = decessi_genere,
    "age" = decessi_eta
  )


  data_region <- ref_data %>%
    dplyr::select(-dplyr::starts_with("var")) %>%
    dplyr::group_by(.data$nome_reg) %>%
    dplyr::summarise_if(purrr::is_numeric, sum, na.rm = TRUE) %>%
    dplyr::mutate(
      nome_reg = stringr::str_replace_all(nome_reg, c(
        "Trentino-Alto Adige/Südtirol" = "Trentino A.A.",
        "Valle d'Aosta/Vallée d'Aoste" = "Valle d'Aosta",
        "Friuli-Venezia Giulia" = "Friuli Venezia Giulia"
      ))
    )

  data_italy <- data_region %>%
    dplyr::summarise_if(purrr::is_numeric, sum) %>%
    dplyr::mutate(nome_reg = "Italia")


  dplyr::bind_rows(data_region, data_italy) %>%
    tidyr::pivot_longer(-"nome_reg",
      names_to = c("type", "strata", "year"),
      names_sep = "_"
    ) %>%
    tidyr::pivot_wider(
      names_from = .data$year,
      values_from = .data$value
    ) %>%
    dplyr::select(-.data$type) %>%
    dplyr::mutate(
      variation = (100 *
        (
          .data[["2020"]] - .data[["2019"]]
        ) / .data[["2019"]]
      ) %>% round(2)
    )

}
