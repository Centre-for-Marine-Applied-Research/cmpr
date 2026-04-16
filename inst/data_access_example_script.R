#library(cmpr)
devtools::load_all()

conn <- cmpr_connect_to_db(connection_config = "admin")

station_data <- cmpr_get_station_data(conn)

DBI::dbDisconnect(conn)
