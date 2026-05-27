#' Insert Location Metadata into database
#'
#' @param conn database connection object
#' @param new_location_metadata data frame of new location metadata, matching database format,
#' @param mode indicates which kind of location data is coming into the database and thus which tables it
#'     should go into, options are 'waterbody' and 'station'
#'
#' @returns tbd
#'
#' @importFrom glue glue_sql
#'
#' @export
cmpr_insert_location_metadata <- function(
  conn,
  new_location_metadata,
  mode
) {
  if (mode == "waterbody") {
    schema <- "public"
    table_name <- "waterbody"
    query_notes <- "automated waterbody metadata insert"
  } else if (mode == "station") {
    schema <- "sensorstring"
    table_name <- "ss_station"
    query_notes <- "automated station metadata insert"
  } else {
    (stop("Error: valid modes are 'waterbody' and 'station'"))
  }

  # Begin transaction
  DBI::dbBegin(conn)
  tryCatch(
    {
      # INSERT data
      DBI::dbAppendTable(
        conn,
        name = DBI::Id(schema = schema, table = table_name),
        value = new_location_metadata
      )
      # Log data insertion
      cmpr_record_data_import(
        conn,
        data_name = "ns_wq_metadata",
        notes = query_notes
      )
      # Commit changes
      DBI::dbCommit(conn)
    },
    error = function(e) {
      # Revert changes and communicate error if detected
      DBI::dbRollback(conn)
      message("ERROR: Automated metadata insertion failed.", e)
    }
  )
}
