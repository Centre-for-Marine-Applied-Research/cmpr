cmpr_create_station <- function(station_name, waterbody_name, province_code) {
  # TODO: retrieve database waterbodies and province codes

  # TODO: Check that the input matches a waterbody and a province
  # TODO: Pull out the corresponding waterbody_id (province_code is already a pkey) Need to flag if the waterbody doesn't exist!
  # TODO: Check that the station does not yet exist (should this also check for numbered versions of the station e.g. Centreville 1)
  station_metadata <- cmpr_get_station_metadata()
  # Assuming the waterbody and province exist
  # TODO: Add station to ss_station table
}
