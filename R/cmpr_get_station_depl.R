#' Get list of deployments at a given station
#'
#' @param station_name name of station
#' @inheritParams cmpr_get_depl_data
#'
#' @returns data frame of all of the deployments from a station
#' @export
#'
#' @importFrom DBI dbClearResult dbFetch dbSendQuery

cmpr_get_station_depl <- function(conn, station_name) {
  selected_station_list_string <-
    paste(glue::glue("'{station_name}'"), collapse = ", ")
  query <- glue::glue(
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
      WHERE station_name IN ({selected_station_list_string});"
  )
  # Retrieve deployment information for the given station
  depl_table <- DBI::dbGetQuery(
    conn,
    query
  )

  # Clean up types and columns
  depl_table <- depl_table |>
    cmpr_clean_enum_types() |>
    cmpr_convert_to_ss_cols()

  return(depl_table)
}
