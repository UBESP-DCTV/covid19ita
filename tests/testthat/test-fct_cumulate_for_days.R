test_that("cumulate_for_days works", {
  expect_equal(
    cumulate_for_days(c(1, 2, 3), 2),
    c(1, 3, 5, 3)
  )

  expect_equal(
    cumulate_for_days(c(1, 2, 3), 3),
    c(1, 3, 6, 5, 3)
  )

  expect_equal(
    cumulate_for_days(c(NA, 2, 3), 3),
    c(0, 2, 5, 5, 3)
  )

})
