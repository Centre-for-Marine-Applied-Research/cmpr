#' Connect to CMAR CMP Database
#'
#' User must be connected to Perennia VPN or Perennia Secure wifi.
#'
#' @param connection_config name of configuration in config.yml to use for
#'   connection
#'
#' @return database connection object
#' @importFrom config get
#' @importFrom RPostgres Postgres
#' @importFrom DBI dbConnect
#' @export
#'
#' @examples
#' db_conn <- try(connect_to_db())

cmpr_connect_to_db <- function(connection_config = "default") {
  # Get configuration file with database connection details
  db_config <- config::get(config = connection_config)

  # Initialize database connection
  conn <- DBI::dbConnect(
    RPostgres::Postgres(),
    user = db_config$user,
    password = db_config$password,
    host = db_config$host,
    port = db_config$port,
    dbname = db_config$dbname
  )
}
# Close database connection
#DBI::dbDisconnect(conn)
