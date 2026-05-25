#' Insert Location Metadata into database
#'
#' @param conn database connection object
#' @param new_location_metadata data frame of new location metadata, matching database format,
#' @param mode indicates which kind of location data is coming into the database and thus which tables it
#'     should go into, options are 'waterbody' and 'station'
#' @param notes optional notes to include in the data import log for this location metadata insertion
#'
#' @returns tbd
#'
#' @export
cmpr_insert_location_metadata <- function(
  conn,
  new_location_metadata,
  mode,
  notes = ""
) {
  if (mode == "waterbody") {
    query <- ""
    query_notes <- "automated waterbody metadata update"
  } else if (mode == "station") {
    query <- ""
    query_notes <- "automated station metadata update"
  } else {
    (stop("Error: valid modes are 'waterbody' and 'station'"))
  }
  data_name <- "ns_wq_metadata"

  # Begin transaction
  DBI::dbBegin(conn)
  # INSERT data
  DBI::dbExecute(conn, query)
  # log data insertion
  cmpr_record_data_import(conn, data_name = data_name, notes = query_notes)
  # Persist results
  DBI::dbCommit(conn)
}
