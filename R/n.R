#' Get a count of number of documents found
#'
#' @export
#' @param .data Input
#' @examples \dontrun{
#' index("gbif") %>% n()
#' }
n <- function(.data) {
  pipe_autoexec(toggle = FALSE)
  exec2(structure(.data, class = "esdsl"))$hits$total
}
