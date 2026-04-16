#library(cmpr)
devtools::load_all()

conn <- cmpr_connect_to_db(connection_config = "admin")

# Look at all stations and their location data
station_data <- cmpr_get_station_data(conn)

# Look at all the deployments at a station
depl_data <- cmpr_get_station_depl(conn, station_name = "Wedgeport")

DBI::dbDisconnect(conn)
