#' Write a new data import record and submit it to the CMPDB
#'
#' @param conn database connection object
#' @param data_name name of the data being imported
#' @param notes text with any additional notes about the data import process
#'
#' @importFrom lubridate now
cmpr_record_data_import <- function(conn, data_name, notes = "") {
  import_date <- lubridate::now()
  query <- glue::glue_sql(
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
