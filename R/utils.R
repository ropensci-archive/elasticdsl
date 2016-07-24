#' elastic DSL utilities
#'
#' @name utils
#' @param x Input
#' @param pretty Pretty print
#' @param .obj Input
#' @param query Query statement
#' @param y  Input
#' @param .dots Input
#' @param ... Further args passed on to \code{\link[jsonlite]{toJSON}}
#' @details Various utilities.
#' \itemize{
#'  \item n - Get a count of number of documents found
#' }
#'
#' @examples \dontrun{
#' # sort
#' x <- index("gbif") %>% filter()
#' x <- x %>% sort(county)
#' x %>% glimpse
#' x %>% exec
#'
#' x <- index("gbif") %>% filter()
#' x <- x %>% sort(asc(county))
#' x %>% glimpse
#' x %>% exec
#'
#' x <- index("gbif") %>% filter()
#' x <- x %>% sort(asc(county), desc(country))
#' x %>% glimpse
#' x %>% exec
#' }
NULL

#' @export
#' @rdname utils
fields <- function(x, ...){
  structure(x, fields = lazyeval::lazy_dots(...))
}
