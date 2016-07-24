#' Specify how many documents to return
#'
#' @export
#' @param .data Input
#' @param size (numeric) number of documents to return
#' @examples \dontrun{
#' index("gbif") %>% size(1)
#' }
size <- function(.data, size) {
  pipe_autoexec(toggle = TRUE)
  .data$params <- as.list(c(.data$params, size = size))
  structure(.data, class = "esdsl")
}
