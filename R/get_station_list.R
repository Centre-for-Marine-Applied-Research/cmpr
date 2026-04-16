#' Get list of CMP stations
#'
#' @param conn database connection object
#'
#' @returns data frame of all stations and their attributes
#' @export
#'
get_station_list <- function(conn) {
  res <- dbSendQuery(conn, "SELECT * FROM sensorstring.ss_station;")
  station_table <- dbFetch(res)
  dbClearResult(res)
  station_table
}
