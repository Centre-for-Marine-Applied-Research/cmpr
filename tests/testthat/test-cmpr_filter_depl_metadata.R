test_that("error message is produced if invalid date is provided", {
  test_metadata_df <- cmpr_read_depl_metadata_sheet(paste0(
    system.file("testdata", package = "cmpr"),
    "/water_quality_deployment_tracking.xlsx"
  ))
  test_last_update_date <- "this literally cannot be converted to a date"
  expect_error(
    cmpr_filter_depl_metadata(test_metadata_df, test_last_update_date),
    regexp = "Warning in parsing last_update_date:"
  )
})

test_that("metadata sheet is not filtered if no date is provided", {
  test_metadata_df <- cmpr_read_depl_metadata_sheet(paste0(
    system.file("testdata", package = "cmpr"),
    "/water_quality_deployment_tracking.xlsx"
  ))
  test_last_update_date <- NULL
  test_metadata_df <- cmpr_filter_depl_metadata(
    test_metadata_df,
    test_last_update_date
  )
  expected_metadata_df <- cmpr_read_depl_metadata_sheet(paste0(
    system.file("testdata", package = "cmpr"),
    "/water_quality_deployment_tracking.xlsx"
  ))
  expect_equal(test_metadata_df, expected_metadata_df)
})

test_that("metadata sheet is correctly filtered based on provided date", {
  test_metadata_df <- cmpr_read_depl_metadata_sheet(paste0(
    system.file("testdata", package = "cmpr"),
    "/water_quality_deployment_tracking.xlsx"
  ))
  test_last_update_date <- "2025-10-10"
  test_metadata_df <- cmpr_filter_depl_metadata(
    test_metadata_df,
    test_last_update_date
  )
  expected_metadata_df <- cmpr_read_depl_metadata_sheet(paste0(
    system.file("testdata", package = "cmpr"),
    "/water_quality_deployment_tracking_filtered.xlsx"
  ))
  expect_equal(test_metadata_df, expected_metadata_df)
})
