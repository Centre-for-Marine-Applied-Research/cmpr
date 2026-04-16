# parse_time_from_excel tests
test_that("POSIXct datetime parses correctly", {
  test_datetime_val <- as.POSIXct("1899-12-31 17:35:00")
  output_time_val <- cmpr_parse_time_from_excel(test_datetime_val)
  expect_equal(output_time_val, "17:35:00")
})

test_that("non-POSIXct values produce error", {
  test_datetime_val <- "1899-12-31 17:35:00"
  expect_error(cmpr_parse_time_from_excel(test_datetime_val))
})
