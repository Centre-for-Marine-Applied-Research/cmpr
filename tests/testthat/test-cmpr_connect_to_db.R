test_that("database connection flags missing config", {
  expect_error(cmpr_connect_to_db())
})
