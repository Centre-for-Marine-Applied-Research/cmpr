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

#' Get a list of columns in the CMPDB which are custom PostgreSQL types
#'
#' @return vector of strings which list the columns in the CMPDB which are custom PostgreSQL types
cmpr_list_enum_columns <- function() {
  return(
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
  )
}

#' Clean up PostgreSQL custom enum type columns
#'
#' @return data frame with column types converted to R types
cmpr_clean_enum_types <- function(df) {
  enum_cols <- cmpr_list_enum_columns()
  df <- df |>
    mutate(
      across(any_of(enum_cols), ~ as.character(.x)),
    )
  df
}

#' Get a mapping of `sensorstrings` column names to CMPDB column names
#'
#' @param mode string "forward" or "reverse" to indicate whether to return a mapping of `sensorstrings` columns names to CMPDB column names or CMPDB column names to `sensorstrings` column names.
#'
#' @return named vector which maps each `sensorstrings` column name to each CMPDB column name or vice versa. Only returns a mapping for column names which differ between `sensorstrings` and CMPDB.
cmpr_get_ss_cmpdb_col_mapping <- function(mode) {
  forward <-
    c(
      station = "station_name",
      waterbody = "waterbody_name",
      county = "county_name",
      province = "province_name",
      lease = "lease_num",
      latitude = "station_latitude",
      longitude = "station_longitude",
      classification = "station_classification",
      sensor_depth_at_low_tide_m = "sensor_depth_m",
      variable = "variable_name",
      value = "variable_value",
      string_configuration = "string_config",
      sensor_type = "sensor_model"
    )
  if (mode == "forward") {
    return(forward)
  } else if (mode == "reverse") {
    # swap names and and values, easy peasy
    reverse <- setNames(names(forward), forward)
    return(reverse)
  } else {
    stop(
      "Error: valid modes are 'forward' and 'reverse', for sensorstrings to cmpdb and cmpdb to sensorstrings, respectively."
    )
  }
}

#' Convert from CMPDB column names to `sensorstrings` R package columns
#'
#' @return columns
cmpr_convert_to_ss_cols <- function(df) {
  cmpdb_to_ss_col_map <- cmpr_get_ss_cmpdb_col_mapping("forward")
  df <- df |>
    dplyr::rename(any_of(cmpdb_to_ss_col_map))
  df
}
