#' Get list of CMP stations
#'
#' @param conn database connection object
#'
#' @returns data frame of all of the deployments from a station
#' @export
#'
#' @importFrom DBI dbSendQuery
#' @importFrom dplyr mutate
#'
cmpr_get_station_depl <- function(conn, station_name) {
  # Retrieve deployment information for the given station
  res <- DBI::dbSendQuery(
    conn,
    paste0(
      "SELECT station_name, 
        depl_date, 
        depl_time_utc, 
        depl_status, 
        retrieval_date, 
        retrieval_time_utc, 
        depl_latitude, 
        depl_longitude, 
        string_config, 
        acoustic_release, 
        anchor_type, 
        anchor_weight_kg, 
        biofouling_prevention, 
        depl_attendant, 
        retrieval_attendant, 
        datum, 
        photos_taken, 
        depl_notes, 
        depth_crosscheck_flag
      FROM sensorstring.ss_depl
      LEFT JOIN sensorstring.ss_station
      ON ss_depl.station_id = ss_station.station_id
      WHERE station_name = '",
      station_name,
      "';"
    )
  )
  depl_table <- dbFetch(res)
  dbClearResult(res)

  # Clean up types and columns
  # depl_table <- depl_table |>
  #   mutate(across(contains("name"), ~ as.character(.x))) |>
  #   mutate(station_classification = as.character(station_classification))
  depl_table
}
