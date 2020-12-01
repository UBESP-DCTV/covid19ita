# static ===============================================================
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








pred_ets <- function(db_long, groups = "type", n_ahead = 15) {

  time_range <- as.Date(range(db_long$date, na.rm = FALSE))

  db_long %>%
    ggplot2::remove_missing() %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups)), .data$date) %>%
    dplyr::mutate(date = as.Date(.data$date)) %>%
    dplyr::summarise(`N beds` = mean(.data$`N beds`)) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      model = .data$data %>%
        purrr::map(~{
          ts <- stats::ts(
            .x$`N beds`,
            start = c(2020, as.numeric(format(time_range[[1]], "%j"))),
            end = c(2020, as.numeric(format(time_range[[2]], "%j")))
          )
          forecast::ets(ts, lambda = forecast::BoxCox.lambda(ts))
        }),
      res  = purrr::map2(.data$data, .data$model, ~{

        pred = forecast::forecast(.y, h = n_ahead)

        tibble::tibble(
          date = seq(from = time_range[[1]],
                     to = time_range[[2]] + n_ahead,
                     by = 1
          ),
          `N beds` = c(.y$fitted, as.double(pred$mean)) %>% round(),
          lower = c(
            .data$`N beds`[seq_along(.y$fitted)],
            as.double(pred$lower[, 2])
          ),
          upper = c(
            .data$`N beds`[seq_along(.y$fitted)],
            as.double(pred$upper[, 2])
          ),
          method = .y$method
        ) %>%
          # If upper or lower are less than 0 put 0
          dplyr::mutate(
            `N beds` = dplyr::if_else(.data$`N beds` < 0, 0, .data$`N beds`),
            lower = dplyr::if_else(.data$lower < 0, 0, .data$lower),
            upper = dplyr::if_else(.data$upper < 0, 0, .data$upper),
            lower = zoo::na.locf(.data$lower),
            upper = zoo::na.locf(.data$upper)
          )
      })
    ) %>%
    dplyr::select(dplyr::all_of(groups), .data$res) %>%
    tidyr::unnest(cols = c("res")) %>%
    dplyr::mutate(
      `N beds` = .data$`N beds`,
      date = as.Date(.data$date, origin = "1970-01-01")
    ) %>%
    dplyr::ungroup()
}



gg_siterep <- function(db, which_info_reg, start_date = "2020-09-01") {

  db_long <- sitrep2long(db, which_info_reg) %>%
    dplyr::filter(.data$date >= as.Date(start_date))

  db_pred <- pred_ets(db_long)
  methods <- dplyr::filter(db_pred, date == max(db_long$date))

  db_long %>%
    ggplot(aes(x = .data$date,
               y = .data$`N beds`,
               colour = .data$type,
               fill = .data$type)) +
    geom_point(size = 0.5) +
    geom_ribbon(data = db_pred,
                aes(ymin = .data$lower, ymax = .data$upper),
                alpha = 0.33) +
    geom_text(data = dplyr::mutate(methods, date = .data$date + 5),
              aes(x = date, y = .data$`N beds`, label = .data$method),
              hjust = "inward",
              size = 3,
              alpha = 0.6
    ) +
    geom_hline(yintercept = 400, linetype = "dashed", colour = "red") +
    geom_hline(yintercept = 500, linetype = "dashed", colour = "black") +
    scale_x_date(date_breaks = "3 days", date_labels = "%d %b") +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.5),
      panel.spacing.y = unit(2, "lines")
    ) +
    ylab("Numero posti letto") +
    xlab("")
}



# live =================================================================

gg_live <- function(db, who, vars, group = c("province", "centre"),
                    start_date = "2020-09-01"
) {
  group <- match.arg(group)

  db_long <- db %>%
    dplyr::rename(date = .data$storing_time) %>%
    dplyr::select(
      .data$date, dplyr::all_of(c(vars, group))
    ) %>%
    dplyr::filter(dplyr::across(dplyr::all_of(group), ~ . %in% who)) %>%
    tidyr::pivot_longer(-c(.data$date, dplyr::all_of(group)),
                        names_to = "type",
                        values_to = "N beds"
    ) %>%
    dplyr::filter(.data$date >= as.Date(start_date)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(
      "date", "type", group
    )))) %>%
    dplyr::summarize(`N beds` = sum(.data$`N beds`, na.rm = TRUE))

  db_pred <- pred_ets(db_long, groups = c("type", group))
  methods <- db_pred %>%
    dplyr::mutate(date = as.Date(.data$date)) %>%
    dplyr::filter(date == as.Date(max(db_long$date))) %>%
    dplyr::distinct() %>%
    dplyr::filter(dplyr::across(dplyr::all_of(group), ~.%in% who))

  db_long  %>%
    ggplot(aes(
      x = .data$date,
      y = .data$`N beds`,
      colour = .data$type,
      fill = .data$type
    )) +
    geom_point(size = 0.5) +
    geom_ribbon(data = db_pred %>%
                  dplyr::mutate(date = as.POSIXct(.data$date)),
                aes(ymin = .data$lower, ymax = .data$upper),
                alpha = 0.33) +
    geom_text(data = dplyr::mutate(methods, date = as.POSIXct(.data$date + 5)),
        aes(x = date, y = .data$`N beds`, label = .data$method),
             hjust = "inward",
             size = 3,
             alpha = 0.6
    ) +
    scale_x_datetime(date_breaks = "3 days", date_labels = "%d %b") +
    facet_wrap(stats::as.formula(paste(group, "~", "."))) +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.5),
      panel.spacing.y = unit(2, "lines")
    ) +
    ylab("Numero posti letto") +
    xlab("")

}

