#' Parse time from date time
#'
#' @param x datetime string in YYYY-MM-DD HH:MM:SS format
#'
#' @return string representing a time in the format HH:MM:SS
#' @export
#' @importFrom stringr str_remove
#'
#' @examples
#' parse_time_from_excel("2024-10-10 10:00:00")
parse_time_from_excel <- function(x) {
  x %>%
    as.character() %>%
    str_remove(pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}") %>%
    trimws()
}
