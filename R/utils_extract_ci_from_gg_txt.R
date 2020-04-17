extract_ci_from_gg_txt <- function(gg) {
  gg_data <- ggplot2::ggplot_build(gg)[["data"]][[1L]]

  n <- nrow(gg_data)
  est <- round(gg_data$y[n], 2)
  lb <- round(gg_data$ymin[n], 2)
  ub <- round(gg_data$ymax[n], 2)

  list(
    est = est,
    label = glue::glue("95% CI ={est}, ({lb}, {ub})")
  )
}
