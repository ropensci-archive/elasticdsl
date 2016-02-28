#' elastic DSL aggs
#'
#' @name aggs
#'
#' @param .obj An index object. If nothing passed defaults to all indices, equivalent to
#' doing e.g., \code{localhost:9200/_search}
#' @param ... Further args passed on
NULL

#' Aggregations
#'
#' @export
#' @rdname aggs
#'
#'
#' @examples
#' library(elastic)
#' target <- Search(
#' index = "gbif",
#' body = list(aggs = list(statistic = list(avg = list(field = "decimalLatitude")))))
#'
#' aggs_example <- index("gbif") %>%
#'   aggs(x = list(statistic = list(avg = list(field = "decimalLatitude"))))
#'
#' identical(target$aggregations, aggs_example$aggregations)
#'
aggs <- function(.obj = list(), ...) {
  aggs_(.obj, .dots = lazyeval::lazy_dots(...))
}

aggs_ <- function(.obj=list(), ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)

  as.json.aggs <- function(x, ...) {
    jsonlite::toJSON(list(aggs = x), ..., auto_unbox = TRUE)
  }

  query <- as.json.aggs(structure(lazy_eval(dots$x), class=c("aggs", "lazy_dots", "list")))
  execute <- function(.obj, query){
    Search_(.obj, body = query)
  }
  execute(.obj, query)
}
