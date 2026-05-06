#' Get measurement data from a specific CMP deployment
#'

#' @param station_name character vectors of the name of the station(s) of interest.
#' @inheritParams cmpr_get_depl_data
#'
#' @returns data frame of the specified variable measured at the specified
#'   station(s).
#' @export
#'
#' @importFrom glue glue
#' @importFrom rlang as_string
#' @importFrom DBI dbSendQuery
#' @importFrom DBI dbFetch
#' @importFrom DBI dbClearResult
#' @importFrom dplyr mutate
#' @importFrom stringr str_to_title
#'
cmpr_get_station_data <- function(
  conn,
  station_name,
  variable_type = NULL,
  variable_name = NULL
) {
  # Input checks
  if (!is.null(variable_type) & !is.null(variable_name)) {
    stop("Error: only one of variable_type and variable_name can be defined")
  }
  # TODO: ensure date parses correctly
  # TODO: ensure variable type is valid
  selected_station_list_string <-
    paste(glue::glue("'{station_name}'"), collapse = ", ")

  # Construct CTE for selected stations
  selected_station_cte <- glue::glue(
    "SelectedStation AS (
      SELECT ss_station.station_id, ss_station.station_name, ss_depl.depl_id, depl_date, retrieval_date, sensor_depth_m, sensor_model, sensor_depl.sensor_serial_num
      FROM sensorstring.ss_depl
      LEFT JOIN sensorstring.ss_station
      ON ss_depl.station_id = ss_station.station_id
      LEFT JOIN sensorstring.sensor_depl
      ON ss_depl.depl_id = sensor_depl.depl_id
	    LEFT JOIN sensorstring.sensor
	    ON sensor_depl.sensor_serial_num = sensor.sensor_serial_num
      WHERE station_name IN ({selected_station_list_string})
      )"
  )

  # Construct CTE for selected variable type or name
  if (is.null(variable_type) & is.null(variable_name)) {
    selected_var_cte <- glue::glue(
      ", SelectedVariable AS (
      SELECT *
      FROM sensorstring.ss_variable)"
    )
  } else {
    if (is.null(variable_type)) {
      var_selection_val <- variable_name
      var_selection_col <- "variable_name"
    } else {
      var_selection_val <- variable_type
      var_selection_col <- "variable_type"
    }
    selected_var_cte <- glue::glue(
      ", SelectedVariable AS (
      SELECT *
      FROM sensorstring.ss_variable
      WHERE {var_selection_col} = '{var_selection_val}')"
    )
  }

  # Queries are constructed using paste0 since sending them to the DB requires a "string" object not a "glue" object
  # TODO: please keep values?!?!?
  query <-
    paste0(
      "WITH ",
      selected_station_cte,
      selected_var_cte,
      "SELECT station_name, depl_date, retrieval_date, sensor_depth_m, sensor_model, sensor_depl_measurement.sensor_serial_num, timestamp_utc, variable_type, variable_name, variable_value, qc_summary_flag
      FROM SelectedStation
      LEFT JOIN sensorstring.sensor_depl_measurement
      ON SelectedStation.depl_id = sensor_depl_measurement.depl_id
      AND SelectedStation.sensor_serial_num = sensor_depl_measurement.sensor_serial_num
      RIGHT JOIN SelectedVariable
      ON SelectedVariable.variable_id = sensor_depl_measurement.variable_id;"
    )

  # TODO: Implement pagination and a progress bar
  res <- DBI::dbSendQuery(
    conn,
    query
  )

  station_table <- DBI::dbFetch(res)
  DBI::dbClearResult(res)

  # Clean up types and columns
  station_table <- station_table |>
    cmpr_clean_enum_types() |>
    cmpr_convert_to_ss_cols() |>
    mutate(qc_flag_value = str_to_title(qc_flag_value))

  return(station_table)
}
