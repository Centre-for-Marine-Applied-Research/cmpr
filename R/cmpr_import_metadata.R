#' Import Metadata to DB
#'
#' @param conn database connection object
#' @param filepath location of the metadata tracking sheet Excel file
#'
#' @returns tbd
#' @export
#' @import lubridate
#' @importFrom DBI dbGetQuery
#'
cmpr_import_metadata <- function(conn, filepath) {
  # TODO: Retrieve location metadata
  # cmpr_read_location_metadata()

  # TODO: Submit location metadata updates to the database
  #cmpr_import_location_metadata
  # Retrieve metadata tracking sheet
  #filepath <- "R:/tracking_sheets/metadata_tracking/water_quality_deployment_tracking.xlsx"
  metadata_sheet <- cmpr_read_depl_metadata_sheet(filepath)

  # Retrieve most recently updated date from the database
  data_import_table <- DBI::dbGetQuery(
    conn,
    "SELECT * FROM public.data_import_log
    WHERE data_name = 'ns_wq_metadata'
    ORDER BY import_date DESC
    LIMIT 1;"
  )
  last_db_update_date <- data_import_table |> dplyr::pull(import_date)
  #last_db_update_date <- "2024-01-01"

  tryCatch(
    {
      last_db_update_date <- lubridate::as_date(last_db_update_date)
    },
    # Stop execution in case of warnings in date parsing - this needs to be fixed
    warning = function(w) {
      stop(paste0(
        "Warning in parsing last_db_update_date:\n",
        w$message,
        "\n"
      ))
    }
  )

  metadata_sheet <- metadata_sheet |>
    filter(last_updated_date > last_db_update_date)

  # Validate the metadata sheet to be imported based on database values
  # All location metadata should have been submitted to the database earlier in this function
  # So any errors should be flagged
  # Does this need a tryCatch?
  cmpr_validate_metadata_sheet(conn, metadata_sheet)

  # Submit metadata to the database
  #cmpr_update_db_depl_metadata(conn, metadata_sheet)

  # TODO: What information can be returned here that would be useful?
}
