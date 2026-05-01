cmpr_validate_metadata_sheet <- function(
  conn,
  metadata_sheet
) {
  # Pull in database station metadata
  db_metadata <- cmpr_get_station_metadata(conn)
  # Filter metadata sheet for what does not exist in the database for each column
  # TODO: Pull this out into a function, since it's so repetitive?
  invalid_stations <- metadata_sheet |>
    dplyr::filter(
      !(station %in% db_metadata$station)
    ) |>
    dplyr::pull(station) |>
    unique()

  invalid_waterbodies <- metadata_sheet |>
    dplyr::filter(
      !(waterbody %in% db_metadata$waterbody)
    ) |>
    dplyr::pull(waterbody) |>
    unique()

  invalid_counties <- metadata_sheet |>
    dplyr::filter(
      !(county %in% db_metadata$county)
    ) |>
    dplyr::pull(county) |>
    unique()

  # Use row counts to determine if any metadata sheet entries did not match the database
  have_invalid_stations <- length(invalid_stations) > 0
  have_invalid_waterbodies <- length(invalid_waterbodies) > 0
  have_invalid_counties <- length(invalid_counties) > 0
  # If there are rows in any of these, we need to throw an error
  if (
    any(c(
      have_invalid_stations,
      have_invalid_waterbodies,
      have_invalid_counties
    ))
  ) {
    # Construct error message based on which values did not have a match
    error_msg <- "Error: stations, waterbodies, or counties exist in the metadata sheet that do not match a database entry."
    if (have_invalid_stations) {
      error_msg <- glue::glue(
        "{error_msg}\nUnexpected stations are: {paste(unique(invalid_stations), collapse = ', ')}"
      )
    }
    if (have_invalid_waterbodies) {
      error_msg <- glue::glue(
        "{error_msg}\nUnexpected waterbodies are: {paste(unique(invalid_waterbodies), collapse = ', ')}"
      )
    }
    if (have_invalid_counties) {
      error_msg <- glue::glue(
        "{error_msg}\nUnexpected counties are: {paste(unique(invalid_counties), collapse = ', ')}"
      )
    }
    stop(
      glue::glue(
        "{error_msg}\nMetadata import process has been aborted. Please check for typos or enter the new locations into the database separately, prior to importing the metadata sheet."
      )
    )
  }
}
