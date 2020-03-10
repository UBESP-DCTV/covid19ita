test_that("download_dpc works", {
  temp_dir <- tempdir()
  out_file <- file.path(temp_dir, "dpc-covid19-ita-andamento-nazionale.csv")

  expect_true(download_dpc("italia", temp_dir))
  expect_true(fs::file_exists(out_file))
  expect_equal(readLines(out_file, 1L), "data,stato,ricoverati_con_sintomi,terapia_intensiva,totale_ospedalizzati,isolamento_domiciliare,totale_attualmente_positivi,nuovi_attualmente_positivi,dimessi_guariti,deceduti,totale_casi,tamponi")
})

