#' Get County Metadata
#'
#' @param conn database connection object
#'
#' @returns data frame with county metadata
#'
#' @export
cmpr_get_county_metadata <- function(conn) {
  query <- "SELECT * FROM public.county;"
  county_table <- DBI::dbGetQuery(conn, query)
  county_table |>
    cmpr_clean_enum_types()
}
