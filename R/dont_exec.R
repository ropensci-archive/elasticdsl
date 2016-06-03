#' Prevent executing search
#'
#' @export
#' @param .obj An index object. If nothing passed defaults to all indices, equivalent to
#' doing e.g., \code{localhost:9200/_search}
dont_exec <- function(.obj) {
  pipe_autoexec(toggle = FALSE)
  return(.obj)
}
