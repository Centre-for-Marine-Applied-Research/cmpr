#' Import Deployment Metadata
#'
#' @param filepath location of the metadata tracking sheet Excel file
#'
#' @return data.frame metadata sheet
#' @export
#' @import dplyr
#' @importFrom readxl read_excel
#'
#' @examples
#' try(
#'   import_depl_metadata("./extdata/water_quality_deployment_tracking.xlsx")
#' )
import_depl_metadata <- function(filepath) {
  col_types <- c("date", #last_updated_date
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

  tryCatch({
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
  })
  # Remove any rows where waterbody is 0 (absence of XLOOKUP so no data)
  metadata_sheet <- metadata_sheet %>%
    filter(waterbody != 0)

  # Reformat time columns
  metadata_sheet <- metadata_sheet %>%
    mutate(across(contains("time_utc"), parse_time_from_excel))

  # Add row index as column to provide relevant row numbers in error messages
  metadata_sheet$row_index = rownames(metadata_sheet)

  # Confirm valid waterbody values
  station_waterbody_county_list <- read_excel(filepath, sheet = "station_waterbody_county")
  waterbody_list <- unique(station_waterbody_county_list$waterbody)

  invalid_waterbody_entries <- metadata_sheet %>%
    filter(!(metadata_sheet$waterbody %in% waterbody_list))

  if(nrow(invalid_waterbody_entries) > 0) {
    stop(paste0("Invalid waterbody found in metadata sheet for ",
                invalid_waterbody_entries$waterbody,
                " at row ",
                invalid_waterbody_entries$row_index,
                "\n"))
  }

  # Confirm valid station values
  station_list <- unique(station_waterbody_county_list$station)

  invalid_station_entries <- metadata_sheet %>%
    filter(!(metadata_sheet$station %in% station_list))

  if(nrow(invalid_station_entries) > 0) {
    stop(paste0("Invalid station found in metadata sheet for ",
                invalid_station_entries$station,
                " at row ",
                invalid_station_entries$row_index,
                "\n"))
  }
  metadata_sheet
}
