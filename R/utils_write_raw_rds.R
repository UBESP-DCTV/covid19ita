#' Write RDS for data-raw
#'
#' Given the names of some data, it save the corresponding objects in
#' the `here::here("data-raw/")` folder. Each object will be saved like
#' an independend `.rds` file with its own name as its filename.
#'
#'
#' @param data_name (chr) names of the pobject
#'
#' @return named logical vector (invisibly) with `TRUE`s for all the
#'   objects that are correctly saved, `FALSE`s for the other ones.
write_raw_rds <- function(data_name) {
  assertive::assert_all_are_non_missing_nor_empty_character(data_name)
  assertive::assert_all_are_existing(data_name)

  ui_todo("Saving dataset(s): {ui_value(data_name)}")

  dir_path <- here::here("data-raw")

  res <- purrr::map_lgl(data_name, ~ {
    written <- attempt::attempt({
      saveRDS(
        object = base::get(.x),
        file = file.path(dir_path, paste0(.x, ".rds")),
        compress = "xz"
      )
    })

    if (attempt::is_try_error(written)) {
      ui_oops("Error writing {ui_value(.x)}.")
      FALSE
    } else {
      ui_done("Dataset {ui_value(.x)} written in {ui_field(dir_path)}.")
      TRUE
    }
  })

  invisible(res)
}


#' @describeIn write_raw_rds read data-raw
read_data_raw <- function(data_name) {
  readr::read_rds(
    here::here("data-raw", paste0(data_name, ".rds"))
  )
}

