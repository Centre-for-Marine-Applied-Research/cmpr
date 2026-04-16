#' Get list of CMP stations
#'
#' @param conn database connection object
#'
#' @returns data frame of all stations and their attributes
#' @export
#'
cmpr_get_station_data <- function(conn) {
  res <- dbSendQuery(conn, "SELECT * FROM sensorstring.ss_station;")
  station_table <- dbFetch(res)
  dbClearResult(res)
  station_table
}
