test_that("download_dpc works", {
  temp_dir <- tempdir()
  out_file <- file.path(temp_dir, "dpc-covid19-ita-andamento-nazionale.csv")

  expect_true(download_dpc("italia", temp_dir))
  expect_true(fs::file_exists(out_file))
  expect_equal(
    readLines(out_file, 1L),
    paste0(
      "data,stato,ricoverati_con_sintomi,terapia_intensiva,",
      "totale_ospedalizzati,isolamento_domiciliare,",
      "totale_positivi,variazione_totale_positivi,nuovi_positivi,",
      "dimessi_guariti,deceduti,casi_da_sospetto_diagnostico,",
      "casi_da_screening,totale_casi,tamponi,casi_testati,note,",
      "ingressi_terapia_intensiva,note_test,note_casi,",
      "totale_positivi_test_molecolare,",
      "totale_positivi_test_antigenico_rapido,",
      "tamponi_test_molecolare,tamponi_test_antigenico_rapido"
    )
  )
})
