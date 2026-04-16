#' Update Deployment Metadata
#'
#' @param conn database connection object
#' @param filepath location of the metadata tracking sheet Excel file
#' @param last_update_date date when the database was last updated from the
#'    metadata tracking sheet
#'
#' @returns
#' @export
#' @import lubridate
#'
cmpr_update_depl_metadata <- function(conn, filepath, last_update_date = NULL) {
  # Retrieve most recent metadata tracking sheet
  metadata_sheet <- import_depl_metadata(filepath)
  # Filter for entries since last update (if NULL, just keep all?)
  if (!is.null(last_update_date)) {
    # Check date format
    tryCatch(
      {
        last_update_date <- lubridate::as_date(last_update_date)
      }, # Stop execution in case of warnings in as_date
      # This is important to properly manage datatypes
      warning = function(w) {
        stop(paste0(
          "Warning in parsing last_update_date:\n",
          w$message,
          "\n"
        ))
      }
    )

    metadata_sheet <- metadata_sheet %>%
      filter(last_updated_date > last_update_date)
  }

  # Split up into relevant database tables: SSDepl and SSDefaultLog
  # SSDepl:
  # station_name,
  # depl_status,
  # depl_date,
  # depl_time_utc,
  # retrieval_date,
  # retrieval_time_utc,
  # depl_latitude,
  # depl_longitude,
  # string_config,
  # acoustic_release,
  # anchor_type,
  # anchor_weight_kg,
  # biofouling_prevention,
  # depl_attendant,
  # retrieval_attendant,
  # datum,
  # photos_taken,
  # depl_notes,
  # depth_crosscheck_flag

  # SSDefaultLog:
  # retrieval_latitude
  # retrieval_longitude
  # sounding_m
  # tide_correction_m
  # depl_tide_direction
  # vr2ar_lug_height_above_seafloor_m
  # primary_buoy_type
  # secondary_buoy_type
  # bottom_buoy_type
  ssdepl <- metadata_sheet %>%
    select(
      station, #station_name
      status, #depl_status
      deployment_date, #depl_date
      deployment_time_utc, #depl_time_utc
      retrieval_date, #retrieval_date
      retrieval_time_utc, #retrieval_time_utc
      deployment_latitude, #depl_latitude
      deployment_longitude, #depl_longitude
      string_configuration, #string_config
      acoustic_release, #acoustic_release
      anchor_type, #anchor_type
      anchor_weight_kg, #anchor_weight_kg
      biofouling_prevention, #biofouling_prevention
      deployment_attendant, #depl_attendant
      retrieval_attendant, #retrieval_attendant
      datum, #datum
      photos_taken, #photos_taken
      notes #depl_notes
    )
  ssdefaultlog <- metadata_sheet %>%
    select(
      station, #station_name
      deployment_date, #depl_date
      retrieval_latitude, #retrieval_latitude
      retrieval_longitude, #retrieval_longitude
      sounding_m, #sounding_m
      tide_correction_m, #tide_correction_m
      deployment_tide_direction, #depl_tide_direction
      vr2ar_lug_height_above_seafloor_m, #vr2ar_lug_height_above_seafloor_m
      primary_buoy_type, #primary_buoy_type
      secondary_buoy_type, #secondary_buoy_type
      bottom_buoy_type #bottom_buoy_type
    )

  # TODO: Handling new rows versus row updates?
  # (possibly happens in stored procedure)

  # Submit to database (should use stored procedure)
}
