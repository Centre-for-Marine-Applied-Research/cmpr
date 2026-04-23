#' Get list of CMP stations
#'
#' @param conn database connection object
#'
#' @returns data frame of all stations and their attributes
#' @export
#'
#' @importFrom DBI dbSendQuery
#' @importFrom dplyr mutate
#'
cmpr_get_station_metadata <- function(conn) {
  # Retrieve station data from database, including waterbody, county, and province
  res <- DBI::dbSendQuery(
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
  station_table <- dbFetch(res)
  dbClearResult(res)

  # Clean up types and columns
  # Clean up custom enum types
  return(cmpr_clean_enum_types(station_table))
}
