test_that("dashboard helpers", {
  expect_shinytag(dashboard_header())
  expect_shinytag(dashboard_home_sidebar())
  expect_shinytag(dashboard_home_body())
})
