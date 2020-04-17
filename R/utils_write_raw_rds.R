#' Write RDS for data-raw
#'
#' Given the names of some data, it save the corresponding objects in
#' the `here::here("data-raw/")` folder. Each object will be saved like
#' an independend `.rds` file with its own name as its filename.
#'
#'
#' @param data_name (chr) names of the pobject
#' @param env teh environment in which look for the objects. By default
#'   it is set to `sys.frame(1)` which is the calling environment. That
#'   is because it is supposed that this funciton will be called inside
#'   the functions which will create/update the data.
#'
#' @return named logical vector (invisibly) with `TRUE`s for all the
#'   objects that are correctly saved, `FALSE`s for the other ones.
write_raw_rds <- function(data_name, env = sys.frame(1)) {
  assertive::assert_all_are_non_missing_nor_empty_character(data_name)
  assertive::assert_all_are_existing(data_name, envir = env)

  ui_todo("Saving dataset(s): {ui_value(data_name)}")

  dir_path <- here::here("data-raw")

  res <- purrr::map_lgl(data_name, ~ {
    written <- attempt::attempt({
      readr::write_rds(
        x = base::get(.x, envir = env, inherits = FALSE),
        path = file.path(dir_path, paste0(.x, ".rds")),
        compress = "xz",
        compression = 9L
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
