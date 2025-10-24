test_that("error message is produced if invalid waterbodies are present in the
          metadata", {
  test_data_filepath <- system.file("testdata", package = "cmpdb")
  expect_error(import_depl_metadata(
    paste0(
      filepath,
      "water_quality_deployment_tracking_invalid_waterbodies.xlsx"
    ),
    regexp = "Invalid waterbody found in metadata sheet for INVALID WATERBODY
    at row [0-9]+"
  ))
})

test_that("error message is produced if invalid stations are present in the
          metadata", {
  test_data_filepath <- system.file("testdata", package = "cmpdb")
  expect_error(import_depl_metadata(
    paste0(
      filepath,
      "water_quality_deployment_tracking_invalid_waterbodies.xlsx"
    ),
    regexp = "Invalid station found in metadata sheet for INVALID STATION at
    row [0-9]+"
  ))
})

test_that("metadata sheet successfully reads in for valid filepath", {
  test_data_filepath <- system.file("testdata", package = "cmpdb")
  expect_error(import_depl_metadata(
    paste0(
      filepath,
      "water_quality_deployment_tracking.xlsx"
    ),
  ))
})
