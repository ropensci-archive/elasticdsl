#' Attempt to coerce output to a single data.frame
#'
#' @importFrom dplyr rbind_all as_data_frame
#' @export
#' @param x Input
#' @param source_only (logical) Return source only?
#' @param n (integer) Count to print.
#' @details Note that this may not always succeed, as we can't predict what
#' various ways your data in formatted in your Elasticsearch instance.
#' @examples \dontrun{
#' elastic::connect()
#'
#' index("shakespeare") %>%
#'   prefix(speaker = "we") %>%
#'   size(200) %>% tabl()
#'
#' x <- index("shakespeare") %>%
#'   filter() %>%
#'   range(line_number <= 20) %>%
#'   size(200) %>%
#'   exec
#' x %>% tabl()
#' }
tabl <- function(x, source_only = TRUE, n = 10){
  pipe_autoexec(toggle = TRUE)
  z <- exec2(structure(as_esdsl(x), class = "esdsl"))$hits$hits
  dplyr::bind_rows(lapply(pluck(z, "_source"), dplyr::as_data_frame))
}
