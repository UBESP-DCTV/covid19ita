devtools::load_all()

# we want to stop on warnings!
withr::local_options(list(warn = 2))

source(here::here("data-raw", "data-dpc_covid19_ita.R"), local = TRUE)
data_dpc()


source(here::here("data-raw", "data-INTERNAL.R"), local = TRUE)
source(here::here("data-raw", "data-EXPORTED.R"), local = TRUE)

rm(list = ls(all.names = TRUE))

withr::with_envvar(
  list("_R_CHECK_LENGTH_1_LOGIC2_" = "false"),
  devtools::check_man()
)

here::here("R") %>%
  list.files(pattern = ".R$", full.names = TRUE) %>%
  purrr::set_names(basename(.)) %>%
  purrr::map(tools::showNonASCIIfile) %>%
  `[`(purrr::map_lgl(., ~length(.x) > 0))


# {
#   options(golem.app.prod = FALSE)
#   golem::detach_all_attached()
#   golem::document_and_reload()
#   run_app(language = "ita")
# }
#


test_that("app launches", {
  f <- function(sleep, testdir = "apptest") {
    skip_on_cran()

    if (Sys.getenv("CALLR_CHILD_R_LIBS_USER") == "") {
      pkg_name <- pkgload::pkg_name()
      go_for_pkgload <- TRUE
    } else {
      pkg_name <- Sys.getenv("TESTTHAT_PKG")
      go_for_pkgload <- FALSE
      cat(pkg_name)
    }
    if (go_for_pkgload) {
      shinyproc <- processx::process$new(
        command = normalizePath(file.path(
          Sys.getenv("R_HOME"), "bin", "R"
        )),
        c(
          "-e",
          "pkgload::load_all(here::here());run_app()"))
    } else {
      shinyproc <- processx::process$new(
        echo_cmd = TRUE,
        command = normalizePath(file.path(
          Sys.getenv("R_HOME"), "bin", "R"
        )),
        c("-e",
          sprintf(
            "library(%s, lib = '%s');run_app()", pkg_name, .libPaths()
          )
        ),
        stdout = "|", stderr = "|"
      )
    }
    Sys.sleep(sleep)
    expect_true(shinyproc$is_alive())
    shinyproc$kill()
  }

  f(20)
})


# devtools::check()
