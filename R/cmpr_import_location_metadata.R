#' Import Location Metadata
#'
#' @param conn database connection object
#' @param location_metadata_sheet data frame of values read in from the location metadata Excel sheetS
#'
#' @return tbd
#'
#' @export
#'
#' @importFrom dplyr across contains filter select left_join join_by
#'
cmpr_import_location_metadata <- function(conn, location_metadata_sheet) {
    # Pull in station, waterbody and county values from the database to distinguish updates from inserts
    station_metadata <- cmpr_get_station_metadata(conn)
    waterbody_metadata <- cmpr_get_waterbody_metadata(conn)
    county_metadata <- cmpr_get_county_metadata(conn)

    # Identify existing versus new waterbodies and stations
    # Waterbodies come first because stations have a dependency on them
    existing_waterbodies <- location_metadata_sheet |>
        dplyr::distinct(waterbody, .keep_all = TRUE) |>
        dplyr::left_join(
            waterbody_metadata,
            by = dplyr::join_by(waterbody == waterbody_name)
        ) |>
        dplyr::filter(
            waterbody %in% waterbody_metadata$waterbody_name
        ) |>
        dplyr::select(waterbody_id, waterbody_name = waterbody)

    new_waterbodies <- location_metadata_sheet |>
        dplyr::distinct(waterbody, .keep_all = TRUE) |>
        dplyr::filter(
            !(waterbody %in% waterbody_metadata$waterbody_name)
        ) |>
        dplyr::select(waterbody_name = waterbody)

    # Put updated waterbodies into the database first so stations can be linked correctly afterwards
    if (nrow(new_waterbodies) > 0) {
        cmpr_insert_location_metadata(
            conn,
            new_location_metadata = new_waterbodies,
            mode = "waterbody"
        )
    }
    if (nrow(existing_waterbodies) > 0) {
        # cmpr_update_location_metadata(
        #     conn,
        #     updated_location_metadata = existing_waterbodies,
        #     mode = "waterbody",
        #     notes = "waterbody update"
        # )
    }

    # Get updated waterbody data to match any new waterbodies to new stations in those waterbodies
    waterbody_metadata <- cmpr_get_waterbody_metadata(conn)
    # Feels like there should be a better way to generate both data frames...
    # Tried looking into group_by and group_split in dplyr but didn't really seem any better it'd need a grouping column
    existing_stations <- location_metadata_sheet |>
        dplyr::filter(
            station %in% station_metadata$station
        ) |>
        dplyr::mutate(
            station_classification = "coastal"
        ) |>
        cmpr_convert_to_db_cols() |>
        dplyr::left_join(
            waterbody_metadata,
            by = dplyr::join_by(waterbody_name)
        ) |>
        dplyr::left_join(
            county_metadata,
            by = dplyr::join_by(county_name)
        ) |>
        dplyr::select(
            waterbody_id,
            province_code,
            county_code,
            station_name,
            station_latitude,
            station_longitude,
            station_classification,
            station_notes = notes
        )

    new_stations <- location_metadata_sheet |>
        dplyr::filter(
            !(station %in% station_metadata$station)
        ) |>
        dplyr::mutate(
            station_classification = "coastal"
        ) |>
        cmpr_convert_to_db_cols() |>
        dplyr::left_join(
            waterbody_metadata,
            by = dplyr::join_by(waterbody_name)
        ) |>
        dplyr::left_join(
            county_metadata,
            by = dplyr::join_by(county_name)
        ) |>
        dplyr::select(
            waterbody_id,
            province_code,
            county_code,
            station_name,
            station_latitude,
            station_longitude,
            station_classification,
            station_notes = notes
        )

    # TODO: Update existing entries in the database
    #cmpr_update_location_metadata()
}
