#' Specify how many documents to return
#'
#' @export
#' @param .data Input
#' @param size (numeric) number of documents to return
#' @examples \dontrun{
#' index("gbif") %>% size(1)
#' index("gbif") %>% size(0)
#' index("gbif") %>% size(300)
#'
#' index("gbif") %>% size(1)
#' index("gbif") %>% size(1) %>% from(1)
#' }
size <- function(.data, size) {
  pipe_autoexec(toggle = TRUE)
  .data <- as_esdsl(.data)
  .data$params <- as.list(c(.data$params, size = size))
  structure(.data, class = "esdsl")
}

#' @export
#' @rdname size
from <- function(.data, from) {
  pipe_autoexec(toggle = TRUE)
  .data <- as_esdsl(.data)
  .data$params <- as.list(c(.data$params, from = from))
  structure(.data, class = "esdsl")
}
