#' Get results
#'
#' @export
#' @param x Input
#' @examples \dontrun{
#' index("shakespeare") %>%
#'  ids(c(1, 2, 150)) %>%
#'  exec() %>%
#'  hits()
#' }
hits <- function(x){
  x$hits$hits
}
