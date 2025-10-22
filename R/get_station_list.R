get_station_list <- function(conn) {
  res <- dbSendQuery(conn, "SELECT * FROM sensorstring.ss_station;")
  station_table <- dbFetch(res)
  dbClearResult(res)
  station_table
}
