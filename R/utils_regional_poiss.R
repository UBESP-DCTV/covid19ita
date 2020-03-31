regional_poiss <- function(.db, response) {
  stopifnot(is.character(response))
  stopifnot(all(c(response, "days", "residenti") %in% names(.db)))

  ## package inheritage for every package but base and covid19ita
  ## (and the imported function, e.g. some from ggplot2)
  stats::as.formula(glue::glue("{response} ~ splines::ns(days, 3)")) %>%
    stats::glm(data = .db,
      family = stats::poisson,
      offset = log(.db[["residenti"]])
    )
}
