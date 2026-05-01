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

  res <- DBI::dbSendQuery(conn, query)
}
