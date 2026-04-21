#' Get list of CMP stations
#'
#' @param conn database connection object
#'
#' @returns data frame of all stations and their attributes
#' @export
#'
#' @importFrom glue glue
#' @importFrom rlang as_string
#' @importFrom DBI dbSendQuery
#' @importFrom DBI dbFetch
#' @importFrom DBI dbClearResult
#' @importFrom dplyr mutate
#'
cmpr_get_depl_data <- function(
  conn,
  station_name,
  depl_date,
  variable_type = NULL,
  variable_name = NULL
) {
  # Input checks
  if (!is.null(variable_type) & !is.null(variable_name)) {
    stop("Error: only one of variable_type and variable_name can be defined")
  }
  # TODO: ensure date parses correctly
  # TODO: ensure variable type is valid
  # TODO: make this work with some arguments as optional (this will require changing the query structure)
  # Suggestion - separate out the SelectedVariable CTE to be constructed separately, then pasted in?
  # Construct CTE for selected variable type or name
  # if (is.null(variable_type) & is.null(variable_name)) {
  #   selected_var_cte <- NULL
  # } else {
  #   var_selection <- is.null(variable_type)?variable_name:variable_type
  #   var_selection_col <- rlang::as_string(var_selection)
  #   selected_var_cte <- glue::glue(
  #     ", SelectedVariable AS (
  #     SELECT *
  #     FROM sensorstring.ss_variable
  #     WHERE {var_selection} = '{var_selection}')"
  #   )
  # }
  res <- DBI::dbSendQuery(
    conn,
    paste0(
      "WITH SelectedDeployment AS (
      SELECT ss_station.station_id, ss_station.station_name, ss_depl.depl_id, depl_date, retrieval_date, sensor_depth_m
      FROM sensorstring.ss_depl
      LEFT JOIN sensorstring.ss_station
      ON ss_depl.station_id = ss_station.station_id
      LEFT JOIN sensorstring.sensor_depl
      ON ss_depl.depl_id = sensor_depl.depl_id
      WHERE station_name = '",
      station_name,
      "' AND depl_date = '",
      depl_date,
      "'
      ), SelectedVariable AS (
        SELECT *
        FROM sensorstring.ss_variable
        WHERE variable_type = '",
      variable_type,
      "'
      )
      SELECT station_name, depl_date, retrieval_date, sensor_serial_num, sensor_depth_m, variable_type, variable_name 
      FROM SelectedDeployment
      LEFT JOIN sensorstring.sensor_depl_measurement
      ON SelectedDeployment.depl_id = sensor_depl_measurement.depl_id
      RIGHT JOIN SelectedVariable
      ON SelectedVariable.variable_id = sensor_depl_measurement.variable_id;"
    )
  )
  depl_table <- DBI::dbFetch(res)
  DBI::dbClearResult(res)

  # Clean up types and columns
  depl_table <- depl_table |>
    mutate(across(contains("name"), ~ as.character(.x))) |>
    mutate(variable_type = as.character(variable_type))
  depl_table
}
