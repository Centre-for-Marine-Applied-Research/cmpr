#' Filter Deployment Metadata by Date
#'
#' @param metadata_df data frame of sensor string deployment metadata. Must
#'   include column \code{last_updated_date}.
#'
#' @param last_update_date date when the database was last updated from the
#'   metadata tracking sheet, in yyyy-mm-dd format.
#'
#' @returns Returns rows of \code{metadata_df} that were updated after
#'   \code{last_update_date}.
#'
#' @export
#'
#' @importFrom dplyr filter mutate
#' @importFrom lubridate as_date

cmpr_filter_depl_metadata <- function(metadata_df, last_update_date = NULL) {
  # Filter for entries since last update if NULL date, keep all
  if (!is.null(last_update_date)) {
    # Check date format
    tryCatch(
      {
        last_update_date <- lubridate::as_date(last_update_date)
      }, # Stop execution in case of warnings in as_date
      # This is important to properly manage datatypes
      warning = function(w) {
        stop(paste0(
          "Warning in parsing last_update_date:\n",
          w$message,
          "\n"
        ))
      }
    )

    metadata_df <- metadata_df |>
      filter(last_updated_date > last_update_date)
  }
  metadata_df
}
