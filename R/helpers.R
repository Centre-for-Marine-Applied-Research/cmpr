#' Parse time from POSIXct
#'
#' @param x POSIXct value
#'
#' @return string representing a time in the format HH:MM:SS
#' @export
#' @importFrom stringr str_remove
#' @importFrom lubridate is.POSIXct
#'
#' @examples
#' datetime_from_excel <- as.POSIXct("2024-10-10 10:00:00")
#' parse_time_from_excel(datetime_from_excel)
parse_time_from_excel <- function(x) {
  # check POSIXct is provided
  if (!is.POSIXct(x)) {
    stop("Error in parsing datetime value ", x, ". Ensure value is provided as POSIXct type.")
  }
  x <- x %>%
    as.character() %>%
    str_remove(pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}") %>%
    trimws()
  # check time format is correct
  if (!grepl("[0-9]{2}:[0-9]{2}:[0-9]{2}", x)) {
    stop("Parsing resulted in unexpected format, not matching HH:MM:SS")
  }
  x
}
