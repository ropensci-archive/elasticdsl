#' elastic DSL utilities
#'
#' @name utils
#' @param x Input
#' @param ... Further args passed on to \code{\link[jsonlite]{toJSON}}
#' @details Various utilities.
#' \itemize{
#'  \item n - Get a count of number of documents found
#' }
NULL

#' @export
#' @rdname utils
n <- function(x) x$hits$total

#' @export
#' @rdname utils
explain <- function(x, pretty = TRUE, ...){
  as.fjson(x, pretty = pretty, ...)
}

#' @export
#' @rdname utils
exec <- function(.obj, query){
  Search_(attr(.obj, "index"), body=as.fjson(.obj))
}

# combine query statements
combine <- function(.obj, ..., .dots){
  list(.obj, lazyeval::all_dots(.dots, ...))
}

# execute on Search
execute <- function(.obj, query){
  Search_(.obj, body=query)
}
