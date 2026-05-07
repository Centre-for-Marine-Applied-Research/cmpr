#' Get Waterbody Metadata
#'
#' @param conn database connection object
#'
#' @returns data frame with waterbody metadata
#'
#' @export
cmpr_get_waterbody_metadata <- function(conn) {
  query <- "SELECT waterbody_id, waterbody_name FROM public.waterbody;"
  waterbody_table <- DBI::dbGetQuery(conn, query)
  waterbody_table
}
