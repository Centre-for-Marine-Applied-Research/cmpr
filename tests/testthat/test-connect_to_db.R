test_that("database connection flags missing config", {
  expect_error(connect_to_db())
})
