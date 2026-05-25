#library(cmpr)
devtools::load_all()

filepath <- "R:/tracking_sheets/metadata_tracking/water_quality_deployment_tracking.xlsx"
conn <- cmpr_connect_to_db(connection_config = "admin")

# TODO: Import metadata, both for locations and for deployments
location_metadata_sheet <- cmpr_read_location_metadata_sheet(filepath)

# TODO: Read in processed data .rds files for bulk import

# TODO: Filter processed data for new deployments to insert

# TODO: Insert new deployments

# TODO: Filter processed data for updates to existing deployments

# TODO: Update existing deployments

DBI::dbDisconnect(conn)
