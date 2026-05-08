#' Import Location Metadata
#'
#' @param filepath location of the metadata tracking sheet Excel file, include
#'   file name and extension.
#'
#' @return data frame of the deployment metadata sheet
#'
#' @export
#'
#' @importFrom dplyr across contains filter select
#' @importFrom readxl read_excel
#'
cmpr_import_location_metadata <- function(conn, filepath) {
    col_types <- c(
        "text", # station
        "text", # waterbody
        "text", # county
        "numeric", # latitude
        "numeric", # longitude
        "text", # latitude_ddm
        "text", # longitude_ddm
        "text", # status
        "text" # notes
    )

    tryCatch(
        {
            location_metadata <- readxl::read_excel(
                filepath,
                sheet = "station_waterbody_county",
                col_types = col_types
            )
        }, # Stop execution in case of warnings in read_excel
        # This is important to properly manage datatypes
        warning = function(w) {
            stop(paste0(
                "Warning in reading deployment metadata tracking sheet:\n",
                w$message,
                "\n"
            ))
        }
    )
    # Remove any rows where:
    location_metadata <- location_metadata |>
        # all values are NA
        filter(rowSums(is.na(location_metadata)) != ncol(location_metadata))

    # Pull in station, waterbody and county values from the database to identify new entries
    waterbody_metadata <- cmpr_get_waterbody_metadata(conn)
    station_metadata <- cmpr_get_station_metadata(conn)
    county_metadata <- cmpr_get_county_metadata(conn)

    # Return new entries to be added to the database
}
