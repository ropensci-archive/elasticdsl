#' Attempt to coerce output to a single data.frame
#'
#' @importFrom dplyr rbind_all as_data_frame
#' @export
#' @param x Input
#' @details Note that this may not always succeed, as we can't predict what
#' various ways your data in formatted in your Elasticsearch instance.
#' @examples \dontrun{
#' elastic::connect()
#' x <- index("shakespeare") %>%
#'   filter() %>%
#'   prefix(speaker = "we") %>%
#'   size(200) %>%
#'   exec()
#' x %>% tabl()
#'
#' x <- index("shakespeare") %>%
#'   filter() %>%
#'   range(line_number <= 20) %>%
#'   size(200) %>%
#'   exec
#' x %>% tabl()
#' }
tabl <- function(x, source_only = TRUE, n = 10){
  z <- x$hits$hits
  dplyr::rbind_all(lapply(pluck(z, "_source"), dplyr::as_data_frame))
}
