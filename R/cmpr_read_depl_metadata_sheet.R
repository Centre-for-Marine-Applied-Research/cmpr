#' Read Deployment Metadata from Metadata Tracking Sheet
#'
#' @param filepath location of the metadata tracking sheet Excel file, including
#'   file name and extension.
#'
#' @return data frame of the deployment metadata sheet
#'
#' @export
#'
#' @importFrom dplyr across contains filter select
#' @importFrom readxl read_excel
#'
cmpr_read_depl_metadata_sheet <- function(filepath) {
  col_types <- c(
    "date", #last_updated_date
    "text", #station
    "text", #waterbody
    "text", #county
    "text", #lease
    "text", #status
    "date", #deployment_date
    "date", #deployment_time_utc
    "date", #retrieval_date
    "date", #retrieval_time_utc
    "numeric", #deployment_latitude
    "numeric", #deployment_longitude
    "numeric", #retrieval_latitude
    "numeric", #retrieval_longitude
    "text", #deployment_latitude_n_ddm
    "text", #deployment_longitude_w_ddm
    "text", #retrieval_latitude_n_ddm
    "text", #retrieval_longitude_w_ddm
    "text", #sensor_type
    "numeric", #sensor_serial_number
    "numeric", #sensor_depth_m
    "text", #string_configuration
    "numeric", #sounding_m
    "text", #acoustic_release
    "numeric", #tide_correction_m
    "numeric", #vr2ar_lug_height_above_seafloor_m
    "text", #deployment_tide_direction
    "text", #primary_buoy_type
    "text", #secondary_buoy_type
    "text", #bottom_buoy_type
    "text", #whalesafe link type
    "numeric", #whalesafe link number
    "text", #anchor_type
    "numeric", #anchor_weight_kg
    "text", #biofouling_prevention
    "text", #datum
    "text", #photos_taken
    "text", #deployment_attendant
    "text", #retrieval_attendant
    "text" #notes
  )

  tryCatch(
    {
      metadata_sheet <- readxl::read_excel(
        filepath,
        sheet = "tracker",
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
  metadata_sheet <- metadata_sheet |>
    # all values are NA
    filter(rowSums(is.na(metadata_sheet)) != ncol(metadata_sheet)) |>
    # waterbody is 0 (absence of XLOOKUP so no data)
    filter(waterbody != 0)

  # Reformat time columns and lease numbers
  metadata_sheet <- metadata_sheet |>
    dplyr::mutate(
      across(contains("time_utc"), cmpr_parse_time_from_excel)
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      station = cmpr_prepend_lease_zeroes(station)
    )

  # Add row index as column to provide relevant row numbers in error messages
  metadata_sheet$row_index = rownames(metadata_sheet)

  # Confirm valid waterbody values
  location_metadata <- cmpr_read_location_metadata_sheet(filepath)
  waterbody_list <- unique(location_metadata$waterbody)

  invalid_waterbody_entries <- metadata_sheet |>
    dplyr::filter_out(waterbody %in% waterbody_list)

  if (nrow(invalid_waterbody_entries) > 0) {
    stop(paste0(
      "Invalid waterbody found in metadata sheet for ",
      invalid_waterbody_entries$waterbody,
      " at row ",
      invalid_waterbody_entries$row_index,
      "\n"
    ))
  }

  # Confirm valid station values
  station_list <- unique(location_metadata$station)

  invalid_station_entries <- metadata_sheet |>
    dplyr::filter_out(station %in% station_list)

  if (nrow(invalid_station_entries) > 0) {
    stop(paste0(
      "Invalid station found in metadata sheet for ",
      invalid_station_entries$station,
      " at row ",
      invalid_station_entries$row_index,
      "\n"
    ))
  }
  metadata_sheet |> select(-row_index)
}
