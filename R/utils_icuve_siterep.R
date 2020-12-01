sitrep2long <- function(db, vars) {
  db %>%
    tidyr::pivot_longer(-date,
                        names_to = "type",
                        values_to = "N beds") %>%
    dplyr::filter(.data$type %in% vars) %>%
    dplyr::mutate(
      type = .data$type %>%
        stringr::str_replace_all(c(
          covid_dead = "CoViD-19 deaths",
          covid_new = "CoViD-19 new",
          covid_discharged = "CoViD-19 discharged",
          covid_occupied = "CoViD-19 beds occupied",
          covid_variation = "CoViD-19 beds variation",
          other_occupied = "Non-CoViD-19 beds occupied",
          overall_free = "Overall free beds",
          overall_occupied = "Overall beds occupied",
          overall_total = "Overall number of beds"
        ))
    )
}




pred_siterep <- function(db_long) {
  db_long %>%
    dplyr::group_by(.data$type) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      model = .data$data %>%
        purrr::map(~stats::loess(
          as.formula("`N beds` ~ date"),
          data = .x %>%
            dplyr::filter(.data$date >= as.Date("2020-09-01")) %>%
            dplyr::mutate(date = as.numeric(.data$date)),
          control = stats::loess.control(surface = "direct")
        )),
      res  = purrr::map2(.data$data, .data$model,
        ~tibble::tibble(
          date = as.Date("2020-09-01"):(max(.x$date) + 15L),
          `N beds` = stats::predict(.y,
             data.frame(date = as.numeric(.data$date))
          ),
          se = stats::predict(.y,
            data.frame(date = as.numeric(.data$date)), se = TRUE
          )[["se.fit"]]
         )
      )
    ) %>%
    dplyr::select(.data$type, .data$res) %>%
    tidyr::unnest(cols = c("res")) %>%
    dplyr::mutate(
      `N beds` = .data$`N beds`,
      date = as.Date(.data$date, origin = "1970-01-01")
    ) %>%
    dplyr::ungroup()
}



gg_siterep <- function(db, db_pred) {
  db %>%
    dplyr::filter(.data$date >= as.Date("2020-09-01")) %>%
    ggplot(aes(x = .data$date,
               y = .data$`N beds`,
               colour = .data$type,
               fill = .data$type)) +
    geom_point() +
    geom_ribbon(data = db_pred,
                aes(ymin = .data$`N beds` - 1.96*.data$se,
                    ymax = .data$`N beds` + 1.96*.data$se),
                alpha = 0.33) +
    geom_hline(yintercept = 400, linetype = "dashed", colour = "red") +
    geom_hline(yintercept = 500, linetype = "dashed", colour = "black") +
    scale_x_date(date_breaks = "3 days",
                 date_labels = "%d %b") +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.5),
      panel.spacing.y = unit(2, "lines")
    ) +
    ylab("Numero posti letto") +
    xlab("")
}
