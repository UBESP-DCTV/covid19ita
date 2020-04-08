#' Evaluate mortality variation
#'
#' @param type (chr) "sex" or "age" stratification respectively
#'
#' @return a tibble including name, stratas, 2019 and 2020 overall death
#'   by strata, and the relative variation from 2019 to 2020
#' @export
#'
#' @examples
#' mort_data_veneto("sex")
#' mort_data_veneto("age")
mort_data_veneto <- function(type = c("sex", "age")) {
  type <- match.arg(type)

  ref_data <- switch(type,
                     "sex" = decessi_genere,
                     "age" = decessi_eta
  )

  data_prov <- ref_data %>%
    dplyr::rename(provincia = .data$nome_prov) %>%
    dplyr::select(-dplyr::starts_with("var")) %>%
    dplyr::filter(.data$nome_reg == "Veneto") %>%
    dplyr::group_by(.data[["provincia"]]) %>%
    dplyr::summarise_if(is.numeric, sum, na.rm = TRUE)

  data_veneto <- data_prov %>%
    dplyr::summarise_if(is.numeric, sum) %>%
    dplyr::mutate(provincia = "Veneto")


  dplyr::bind_rows(data_prov, data_veneto) %>%
    tidyr::pivot_longer(-"provincia",
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
