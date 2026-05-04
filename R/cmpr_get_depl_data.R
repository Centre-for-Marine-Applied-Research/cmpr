#' Get measurement data from a specific CMP deployment
#'
#' @param conn database connection object.
#' @param station_name character string indicating the station of interest.
#' @param depl_date string indicating the deployment of interest, in the order
#'   yyyy-mm-dd. This must match a deployment start date for one of the
#'   deployments at \code{station}.
#' @param variable_type type of variable to extract. Must be one of
#'   "dissolved_oxygen", "salinity", "sensor_depth_measured", or "temperature".
#'   This is useful for extracting dissolved oxygen data measured in both units.
#'   If \code{variable_type} is specified, \code{variable_name} must be
#'   \code{NULL}.
#' @param variable_name name of variable to extract. Must be one of:
#'   "dissolved_oxygen_percent_saturation", "dissolved_oxygen_mg_per_l",
#'   "salinity_psu", "sensor_depth_measured_m", or "temperature_degree_c". If
#'   \code{variable_name} is specified, \code{variable_type} must be
#'   \code{NULL}.
#'
#' @returns data frame of the specified variable measured over the specified
#'   station deployment.
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

  # Construct CTE for selected deployment
  selected_depl_cte <- glue::glue(
    "SelectedDeployment AS (
      SELECT ss_station.station_id, ss_station.station_name, ss_depl.depl_id, depl_date, retrieval_date, sensor_depth_m, sensor_serial_num
      FROM sensorstring.ss_depl
      LEFT JOIN sensorstring.ss_station
      ON ss_depl.station_id = ss_station.station_id
      LEFT JOIN sensorstring.sensor_depl
      ON ss_depl.depl_id = sensor_depl.depl_id
      WHERE station_name = '{station_name}'",
    " AND depl_date = '{depl_date}')"
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
      selected_depl_cte,
      selected_var_cte,
      "SELECT station_name, depl_date, retrieval_date, sensor_depth_m, sensor_depl_measurement.sensor_serial_num, timestamp_utc, variable_type, variable_name, variable_value, qc_summary_flag
      FROM SelectedDeployment
      LEFT JOIN sensorstring.sensor_depl_measurement
      ON SelectedDeployment.depl_id = sensor_depl_measurement.depl_id
      AND SelectedDeployment.sensor_serial_num = sensor_depl_measurement.sensor_serial_num
      RIGHT JOIN SelectedVariable
      ON SelectedVariable.variable_id = sensor_depl_measurement.variable_id;"
    )

  # TODO: Implement pagination and a progress bar
  res <- DBI::dbSendQuery(
    conn,
    query
  )

  depl_table <- DBI::dbFetch(res)
  DBI::dbClearResult(res)

  # Clean up types and columns
  depl_table <- depl_table |>
    cmpr_clean_enum_types() |>
    cmpr_convert_to_ss_cols()

  return(depl_table)
}
