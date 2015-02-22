#' Search method for the DSL
#'
#' @export
#' @param .obj things stuff
#' @param ... Further args passed on to \code{\link[elastic]{Search}}
#' @rdname Search
Search_ <- function(.obj = "", ...){
  obj <- if(is(.obj, "index")){
    attr(.obj, "index")
  } else {
    .obj
  }
  Search(index = obj, ...)
}
