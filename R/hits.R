#' Get results (hits slot)
#'
#' @export
#' @param x Input
#' @examples \dontrun{
#' index("shakespeare") %>%
#'  ids(1, 2, 150) %>%
#'  hits()
#' }
hits <- function(x) {
  pipe_autoexec(toggle = TRUE)
  exec2(structure(as_esdsl(x), class = "esdsl"))$hits$hits
}
