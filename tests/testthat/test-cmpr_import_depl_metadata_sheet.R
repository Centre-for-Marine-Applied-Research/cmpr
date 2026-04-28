test_that("error message is produced if invalid waterbodies are present in the
          metadata", {
  test_data_filepath <- system.file("testdata", package = "cmpr")
  expect_error(
    cmpr_import_depl_metadata_sheet(paste0(
      test_data_filepath,
      "/water_quality_deployment_tracking_invalid_waterbodies.xlsx"
    )),
    regexp = "Invalid waterbody found in metadata sheet for INVALID WATERBODY at row [0-9]+"
  )
})

test_that("error message is produced if invalid stations are present in the
          metadata", {
  test_data_filepath <- system.file("testdata", package = "cmpr")
  expect_error(
    cmpr_import_depl_metadata_sheet(
      paste0(
        test_data_filepath,
        "/water_quality_deployment_tracking_invalid_stations.xlsx"
      )
    ),
    regexp = "Invalid station found in metadata sheet for INVALID STATION at row [0-9]+"
  )
})

test_that("metadata sheet successfully reads in for valid filepath", {
  test_data_filepath <- system.file("testdata", package = "cmpr")
  expect_no_error(cmpr_import_depl_metadata_sheet(
    paste0(
      test_data_filepath,
      "/water_quality_deployment_tracking.xlsx"
    )
  ))
})
