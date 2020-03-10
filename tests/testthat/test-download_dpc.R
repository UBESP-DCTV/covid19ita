test_that("download_dpc works", {
  temp_dir <- tempdir()
  expect_true(download_dpc("italia", temp_dir))
  expect_true(fs::file_exists(file.path(temp_dir, "italia.csv")))
})
