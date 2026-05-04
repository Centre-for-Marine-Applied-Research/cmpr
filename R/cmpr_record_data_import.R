#' Write a new data import record and submit it to the CMPDB
#'
#' @param conn
#'
#' @importFrom lubridate now
cmpr_record_data_import <- function(conn, data_name, notes = "") {
  import_date <- lubridate::now()
  query <- glue::glue(
    "INSERT INTO data_import_log (data_name, import_date, notes) 
    VALUES ('{data_name}', '{import_date}', '{notes}');"
  )

  num_affected_rows <- DBI::dbExecute(conn, query)
  if (num_affected_rows != 1) {
    stop(
      "Error: number of rows other than 1 affected. This function should only affect a single row. Please contact the database administrator."
    )
  }
}
