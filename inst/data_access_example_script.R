#library(cmpr)
devtools::load_all()

conn <- cmpr_connect_to_db(connection_config = "admin")

# Look at all stations and their location data
# TODO: Look at all station data overall
# TODO: Accept vector of stations
station_metadata <- cmpr_get_station_metadata(conn)

# Look at all the deployments at a station
station_depls <- cmpr_get_station_depl(conn, station_name = "Jewers Bay")
# TODO: Add option to get retrieval data and other log details

# TODO (low priority): Look at the variables available at different stations
# Avoid going via the measurements since there are so many rows
# Instead go via sensor -> sensor_model -> sensor_model_variable -> ss_variable

# TODO (low priority):
# Look at all the sensors in a given deployment
# sensor_metadata <- cmpr_get_depl_sensors(
#   conn,
#   station_name = "Wedgeport",
#   depl_date = "2020-06-16"
# )

# Get all the data from a given station
station_data <- cmpr_get_station_data(
  conn,
  station_name = c("Wedgeport", "Jewers Bay", "Tickle Island"),
  variable_type = "temperature"
)

# Get all the data from a specific deployment
# (options for specifying which sensors or variables whether to include qc flags)
depl_data <- cmpr_get_depl_data(
  conn,
  station_name = "Wedgeport",
  depl_date = "2022-06-21",
  variable_type = "temperature"
)

DBI::dbDisconnect(conn)
