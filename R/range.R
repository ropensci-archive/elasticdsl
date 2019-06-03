#' range dsl
#'
#' @export
#' @param .obj An index object. If nothing passed defaults to all indices, equivalent to
#' doing e.g., \code{localhost:9200/_search}
#' @param boost Explanation...
#' @param time_zone Explanation...
#' @param execution Explanation...
#' @param cache Explanation...
#' @param .dots Explanation...
#' @param ... Further args passed on
#' @family query-dsl
#' @examples \dontrun{
#' elastic::connect(errors = "complete")
#'
#' x <- index("shakespeare") %>% range( speech_number <= 5 ) %>% size(200)
#' max(vapply(x$hits$hits, "[[", 1, c("_source", "speech_number")))
#' x <- index("shakespeare") %>% range( speech_number >= 5 ) %>% size(200)
#' min(vapply(x$hits$hits, "[[", 1, c("_source", "speech_number")))
#' # index("shakespeare") %>% range( speech_number <= c(1,5) ) # doens't work
#' # index("shakespeare") %>% range( speech_number >= c(1,5) ) # doens't work
#' }
range <- function(.obj=list(), ..., boost=1, time_zone=NULL, execution=NULL, cache=FALSE) {
  range_(.obj, .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname range
range_ <- function(.obj=list(), ..., .dots) {
  pipe_autoexec(toggle = TRUE)
  dots <- lazyeval::all_dots(.dots, ...)
  query <- as.json(structure(dots, class = c("range", "lazy_dots")))
  structure(list(index = .obj, query = query), class = "esdsl")
}
