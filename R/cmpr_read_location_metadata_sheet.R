#' Read location Metadata from Metadata Tracking Sheet
#'
#' @param filepath location of the metadata tracking sheet Excel file, including
#'   file name and extension.
#'
#' @returns data frame of the locations from the metadata tracking sheet
#'
#' @export
#'
#' @importFrom readxl read_excel
cmpr_read_location_metadata_sheet <- function(filepath) {
  col_types <- c(
    "text", #station
    "text", #waterbody
    "text", #county
    "numeric", #latitude
    "numeric", #longitude
    "text", #latitude_ddm
    "text", #longitude_ddm
    "text", #status
    "text" #notes
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
  location_metadata |>
    dplyr::rowwise() |>
    dplyr::mutate(station = cmpr_prepend_lease_zeroes(station))
}
