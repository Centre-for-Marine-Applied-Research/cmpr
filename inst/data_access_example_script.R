#library(cmpr)
devtools::load_all()

conn <- cmpr_connect_to_db(connection_config = "admin")

# Look at all stations and their location data
station_metadata <- cmpr_get_station_metadata(conn)

# Look at all the deployments at a station
station_depls <- cmpr_get_station_depl(conn, station_name = "Jewers Bay")

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
