#' Update Location Metadata in database
#'
#' @param conn database connection object
#' @param updated_location_metadata data frame of updated location metadata, matching database format,
#' @param mode indicates which kind of location data is coming into the database and thus which tables it
#'     should go into, the only option is currently 'station'
#'
#' @returns tbd
#'
#' @importFrom foreach foreach %do%
#' @import doParallel
#' @export
cmpr_update_location_metadata <- function(
  conn,
  updated_location_metadata,
  mode
) {
  if (mode == "station") {
    schema <- "sensorstring"
    table_name <- "ss_station"
    query_notes <- "automated station metadata update"
  } else {
    (stop("Error: the only valid mode at this time is 'station'"))
  }

  # Set up parallel clusters to allow multiple processing
  #cluster <- parallel::makeCluster(2)

  # Generate individual query strings, one for each row of the data frame
  # This is done so the inputs can be parameterized

  query_text <- glue::glue_sql(
    "UPDATE {`schema`}.{`table_name`}
    SET 
      waterbody_id = data.waterbody_id,
      province_code = data.province_code,
      county_code = data.county_code,
      station_name = data.station_name
      # TODO!!!!!!!!!!!!!!!
      FROM (
        VALUES 
          (?waterbody_name)
      ) AS data(waterbody_name)
    WHERE waterbody_id = ?waterbody_id;",
    .con = conn
  )
  # res <- DBI::dbSendStatement(conn, query)
  # DBI::dbBind(res, unname(unlist(updated_location_metadata[1, ])))
  # DBI::dbGetRowsAffected(res)
  #DBI::dbClearResult(res)
  # results <- foreach::foreach(
  #   i = 1:nrow(updated_location_metadata),
  #   .combine = "rbind",
  #   .inorder = FALSE
  # ) %dopar%
  #   {
  #     sql_query <- DBI::sqlInterpolate(
  #       conn,
  #       query_text,
  #       waterbody_name = updated_location_metadata[i, ]$waterbody_name,
  #       waterbody_id = updated_location_metadata[i, ]$waterbody_id
  #     )
  #     res <- DBI::dbSendStatement(conn, sql_query)
  #   }
  results <- foreach::foreach(
    i = 1:nrow(updated_location_metadata),
    .combine = "rbind",
    .inorder = FALSE
  ) %do%
    {
      sql_query <- DBI::sqlInterpolate(
        conn,
        query_text,
        waterbody_name = updated_location_metadata[i, ]$waterbody_name,
        waterbody_id = updated_location_metadata[i, ]$waterbody_id
      )
      res <- DBI::dbSendStatement(conn, sql_query)
    }

  # Begin transaction
  DBI::dbBegin(conn)
  tryCatch(
    {
      # INSERT data
      DBI::dbAppendTable(
        conn,
        name = DBI::Id(schema = schema, table = table_name),
        value = new_location_metadata
      )
      # Log data insertion
      cmpr_record_data_import(
        conn,
        data_name = "ns_wq_metadata",
        notes = query_notes
      )
      # Commit changes
      DBI::dbCommit(conn)
    },
    error = function(e) {
      # Revert changes and communicate error if detected
      DBI::dbRollback(conn)
      message("ERROR: Automated metadata insertion failed.", e)
    }
  )
}
