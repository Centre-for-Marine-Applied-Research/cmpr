#' Title
#'
#' @param x
#'
#' @return
#' @export
#' @importFrom stringr str_remove
#'
#' @examples
parse_time_from_excel <- function(x) {
  x %>%
    as.character() %>%
    str_remove(pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}") %>%
    trimws()
}
