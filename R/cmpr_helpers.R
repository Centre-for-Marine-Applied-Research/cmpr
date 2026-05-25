#' Parse time from POSIXct
#'
#' Can we use lubridate::parse_datetime() for this?
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
  x <- x |>
    as.character() |>
    str_remove(pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}") |>
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

#' Columns in the CMPDB that are custom PostgreSQL types
#'
#' @return vector of strings listing columns in the CMPDB that are custom
#'   PostgreSQL types.

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
#' @param df data frame with columns to clean.
#' @return data frame with column types converted to R types
#'
#' @importFrom dplyr across any_of contains mutate

cmpr_clean_enum_types <- function(df) {
  enum_cols <- cmpr_list_enum_columns()
  df <- df |>
    mutate(
      across(any_of(enum_cols), ~ as.character(.x)),
    )
  df
}

#' Get a mapping of \code{sensorstrings} column names to CMPDB column names
#'
#' @param mode string "forward" or "reverse" to indicate whether to return a
#'   mapping of \code{sensorstrings} columns names to CMPDB column names or CMPDB
#'   column names to \code{sensorstrings} column names.
#'
#' @return named vector which maps each \code{sensorstrings} column name to each
#'   CMPDB column name or vice versa. Only returns a mapping for column names
#'   which differ between \code{sensorstrings} and CMPDB.
#'
#' @importFrom stats setNames

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
      sensor_serial_number = "sensor_serial_num",
      sensor_depth_at_low_tide_m = "sensor_depth_m",
      variable = "variable_name",
      value = "variable_value",
      string_configuration = "string_config",
      sensor_type = "sensor_model",
      qc_flag_value = "qc_summary_flag"
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

#' Convert from CMPDB column names to \code{sensorstrings} R package columns
#'
#' @param df data frame with columns to be renamed.
#'
#' @return columns
#'
#' @importFrom dplyr any_of rename

cmpr_convert_to_ss_cols <- function(df) {
  cmpdb_to_ss_col_map <- cmpr_get_ss_cmpdb_col_mapping("forward")

  df <- df |>
    dplyr::rename(any_of(cmpdb_to_ss_col_map))
  df
}

#' Convert from `sensorstrings` R column names to CMPDB column names
#'
#' @param df data frame with columns to be renamed
#'
#' @return columns
#'
#' @importFrom dplyr any_of rename

cmpr_convert_to_db_cols <- function(df) {
  cmpdb_to_ss_col_map <- cmpr_get_ss_cmpdb_col_mapping("reverse")

  df <- df |>
    dplyr::rename(any_of(cmpdb_to_ss_col_map))
  df
}

#' Prepend any missing zeroes to lease numbers
#'
#' @param lease_num lease number that may be missing prepended zeroes due to spreadsheet formatting idiosyncracies
#'
#' @returns string lease number with prepended zeroes
#'
#' @export
cmpr_prepend_lease_zeroes <- function(lease_num) {
  # Ensure it is a lease number, i.e. is entirely numbers
  if (grepl(pattern = "^[0-9]*$", lease_num)) {
    if (!is.na(lease_num)) {
      while (nchar(lease_num) < 4) {
        lease_num <- paste0("0", lease_num)
      }
    }
  }
  return(lease_num)
}
