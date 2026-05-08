#library(cmpr)
devtools::load_all()

conn <- cmpr_connect_to_db(connection_config = "admin")

# TODO: Import metadata, both for locations and for deployments

# TODO: Read in processed data .rds files for bulk import

DBI::dbDisconnect(conn)
