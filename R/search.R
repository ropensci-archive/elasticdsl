#' Search method for the DSL
#'
#' @export
#' @param .obj things stuff
#' @param ... Further args passed on to \code{\link[elastic]{Search}}
#' @rdname Search
Search_ <- function(.obj = "", body = list(), params = list(), ...){
  obj <- if (is(.obj, "index")) {
    attr(.obj, "index")
  } else {
    .obj
  }
  if (as.character(body) == "[]") {
    body <- list()
  }
  # Search(index = obj, body = body, ...)
  do.call("Search", c(index = obj, body = body, unlist(params, recursive = FALSE)))
}
