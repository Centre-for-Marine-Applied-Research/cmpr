#' Parse time from POSIXct
#'
#' @param x POSIXct value or vector of POSIXct values
#'
#' @return string representing a time in the format HH:MM:SS
#' @export
#' @importFrom stringr str_remove
#' @importFrom lubridate is.POSIXct
#'
#' @examples
#' datetime_from_excel <- as.POSIXct("2024-10-10 10:00:00")
#' cmpr_parse_time_from_excel(datetime_from_excel)
cmpr_parse_time_from_excel <- function(x) {
  # check POSIXct is provided
  if (!is.POSIXct(x)) {
    stop(
      "Error in parsing datetime value ",
      x,
      ". Ensure value is provided as POSIXct type."
    )
  }
  x <- x %>%
    as.character() %>%
    str_remove(pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}") %>%
    trimws()
  # check time format is correct for non-NA values
  if (any(!is.na(x) & !grepl("[0-9]{2}:[0-9]{2}:[0-9]{2}", x))) {
    problem_values <- x[!grepl("[0-9]{2}:[0-9]{2}:[0-9]{2}", x)]
    stop(
      "Parsing resulted in unexpected format, not matching 'HH:MM:SS': ",
      paste(unique(problem_values), collapse = ", ")
    )
  }
  x
}

#' Get a list of columns which are custom PostgreSQL types
#'
#' @return vector of strings which list the columns which are custom PostgreSQL types
cmpr_list_enum_columns <- function() {
  return
  c(
    "county_name",
    "province_name",
    "station_classification",
    "flag_value",
    "qc_summary_flag",
    "depth_crosscheck_flag",
    "sensor_depl_status",
    "depl_status",
    "string_config",
    "acoustic_release",
    "biofouling_prevention",
    "potential_tampering",
    "datum",
    "photos_taken",
    "depl_tide_direction",
    "variable_type",
    "variable_units"
  )
}

#' Clean up PostgreSQL custom enum type columns
#'
#' @return columns with type converted to R types
cmpr_clean_enum_types <- function(df) {
  enum_cols <- cmpr_list_enum_columns()
  df <- df |>
    mutate(
      across(any_of(enum_cols), ~ as.character(.x)),
    )
  df
}
