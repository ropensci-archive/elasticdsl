#' Search method for the DSL
#'
#' @export
#' @param .obj things stuff
#' @param body Body stuff in a list
#' @param params Parameters in a list
#' @param ... Further args passed on to \code{\link[elastic]{Search}}
#' @rdname Search
Search_ <- function(.obj = "", body = list(), params = list(), ...){
  obj <- if (inherits(.obj, "index")) {
    attr(.obj, "index")
  } else {
    .obj
  }
  do.call("Search", as.list(c(index = obj, body = body, unlist(params, recursive = FALSE))))
}
