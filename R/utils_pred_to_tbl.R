predict_to_tbl <- function(pred, data) {


  ci_ray <- if ("df" %in% names(pred)) {
    qt(0.975, pred[["df"]]) * pred[["se.fit"]]
  } else {
    1.96 * pred[["se.fit"]]
  }

  tibble::tibble(
    day         = c(
      data[["day"]],
      max(data[["day"]]) + lubridate::days(1:3)
    ),
    totale_casi = pred[["fit"]],
    lower       = pred[["fit"]] - ci_ray,
    upper       = pred[["fit"]] + ci_ray,
    series      = 'Predetto'
  )
}


gg_novara <- function(db_pred, db_true) {
  ggplot(db_pred, aes(
    x = as.Date(.data$day),
    y = .data$totale_casi,
    colour = .data$series
  )) +
    geom_point(data = db_true) +
    geom_line() +
    geom_line(aes(y = lower), linetype = "dashed") +
    geom_line(aes(y = upper), linetype = "dashed") +
    labs(title = "", x = "Giorno", y = "Totale casi") +
    scale_x_date(date_breaks = "1 day", date_labels = "%b %d")
}
