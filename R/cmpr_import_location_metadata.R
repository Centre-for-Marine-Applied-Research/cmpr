#' Import Location Metadata
#'
#' @param filepath location of the metadata tracking sheet Excel file, include
#'   file name and extension.
#'
#' @return data frame of the deployment metadata sheet
#'
#' @export
#'
#' @importFrom dplyr across contains filter select
#' @importFrom readxl read_excel
#'
cmpr_import_location_metadata <- function(conn, location_metadata) {
    # Pull in station, waterbody and county values from the database to distinguish updates from inserts
    station_metadata <- cmpr_get_station_metadata(conn)
    waterbody_metadata <- cmpr_get_waterbody_metadata(conn)
    county_metadata <- cmpr_get_county_metadata(conn)

    # Identify existing versus new stations, waterbodies, and counties
    # Feels like there should be a better way to do this... Tried looking into group_by and group_split
    # in dplyr but they didn't really seem any better since they'd need a grouping column
    existing_stations <- location_metadata |>
        dplyr::filter(
            station %in% station_metadata$station
        ) |>
        dplyr::mutate(station_classification = "coastal") |>
        dplyr::select(
            station,
            waterbody,
            station_latitude = latitude,
            station_longitude = longitude,
            station_classification,
            station_notes = notes
        )
    new_stations <- location_metadata |>
        dplyr::filter(
            !(station %in% station_metadata$station)
        ) |>
        dplyr::mutate(station_classification = "coastal") |>
        dplyr::select(
            station,
            waterbody,
            station_latitude = latitude,
            station_longitude = longitude,
            station_classification,
            station_notes = notes
        )

    existing_waterbodies <- location_metadata |>
        dplyr::filter(
            waterbody %in% waterbody_metadata$waterbody_name
        )
    new_waterbodies <- location_metadata |>
        dplyr::filter(
            !(waterbody %in% waterbody_metadata$waterbody)
        )

    existing_counties <- location_metadata |>
        dplyr::filter(
            county %in% county_metadata$county_name
        )
    new_counties <- location_metadata |>
        dplyr::filter(
            !(county %in% county_metadata$counties)
        )

    # TODO: Insert new entries into the database
    #cmpr_insert_location_metadata()

    # TODO: Update existing entries in the database
    #cmpr_update_location_metadata()
}
