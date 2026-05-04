#' Get list of CMP stations
#'
#' @inheritParams cmpr_get_depl_data
#'
#' @returns data frame of all stations with their deloyment dates and locations.
#' @export
#'
#' @importFrom DBI dbGetQuery
#'
cmpr_get_station_metadata <- function(conn) {
  # Retrieve station data from database, including waterbody, county, and province
  station_table <- DBI::dbGetQuery(
    conn,
    "SELECT
      station_name,
      waterbody_name,
      county_name,
      province_name,
      lease_num,
      station_latitude,
      station_longitude,
      station_classification,
      station_notes
    FROM sensorstring.ss_station
    RIGHT JOIN waterbody
    ON ss_station.waterbody_id = waterbody.waterbody_id
    RIGHT JOIN county
    ON ss_station.county_code = county.county_code
    RIGHT JOIN province
    ON county.province_code = province.province_code;"
  )

  # Clean up types and columns
  station_table <- station_table |>
    cmpr_clean_enum_types() |>
    cmpr_convert_to_ss_cols()

  return(station_table)
}
