test_that("pull_region_w_pop works", {
  expect_is(
    pull_region_w_pop("Veneto"),
    "tbl_df"
  )

  expect_equal(
    pull_region_w_pop("Veneto")$pop[[1]],
    4907529
  )
})
