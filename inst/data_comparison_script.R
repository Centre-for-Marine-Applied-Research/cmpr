library(diffr)
library(here)
library(readr)
library(readxl)
library(dplyr)

# SETUP ----
# Read in metadata spreadsheet and convert to .csv
metadata_sheet <- read_xlsx(
  path = "R:/tracking_sheets/metadata_tracking/water_quality_deployment_tracking.xlsx",
  sheet = "station_waterbody_county"
) |>
  dplyr::arrange(station) |>
  dplyr::select(-c(latitude_ddm, longitude_ddm, status))

# Remove quotations from file saved from database query
db_export <- read_csv(
  file = here(
    "inst",
    "station_location_import_compare.csv"
  )
) |>
  dplyr::mutate(
    across(
      everything(),
      ~ stringr::str_remove_all(.x, pattern = '"')
    ),
    across(
      everything(),
      ~ na_if(.x, "NULL")
    ),
    station_latitude = as.numeric(station_latitude),
    station_longitude = as.numeric(station_longitude)
  ) |>
  dplyr::arrange(station_name)

# DIFF ----
# Neat visual but as soon as one file has a line the other does not have, ]
# it gets offset for the rest of the file
# Write to file for diffr function
metadata_sheet |>
  write_csv(file = here("inst", "water_quality_deployment_tracking.csv"))

db_export |>
  write_csv(file = here("inst", "station_location_import_compare.csv"))

diffr(
  here(
    "inst",
    "station_location_import_compare.csv"
  ),
  here(
    "inst",
    "water_quality_deployment_tracking.csv"
  )
)

# JOIN ----
# Identifying missing values using joins
db_export_missing_in_metadata_sheet <- anti_join(
  db_export,
  metadata_sheet,
  by = join_by(
    station_name == station,
    # waterbody_name == waterbody,
    county_name == county #,
    # lease_num == lease_num,
    # station_latitude == latitude,
    # station_longitude == longitude
  )
)

metadata_sheet_missing_in_db_export <- anti_join(
  metadata_sheet,
  db_export,
  by = join_by(
    station == station_name,
    # waterbody == waterbody_name,
    county == county_name #,
    #lease_num == lease_num,
    # latitude == station_latitude,
    # longitude == station_longitude
  )
)
