#' ids dsl
#'
#' @export
#' @param .obj An index object. If nothing passed defaults to all indices, equivalent to
#' doing e.g., \code{localhost:9200/_search}
#' @param .dots Explanation...
#' @param type type of id to match on. Default: \code{NULL}
#' @param ... Further args passed on
#' @examples \dontrun{
#' elastic::connect(errors = "complete")
#'
#' # ids query
#' index("shakespeare") %>%
#'  ids(1, 2, 150) %>%
#'  n()
#' }
ids <- function(.obj=list(), ..., type = NULL){
  ids_(.obj, .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname ids
ids_ <- function(.obj=list(), ..., .dots, type = NULL){
  pipe_autoexec(toggle = TRUE)
  dots <- lazyeval::all_dots(.dots, ...)
  query <- as.json(structure(dots, class = c("ids", "lazy_dots")), type = type)
  structure(list(index = .obj, query = query), class = "esdsl")
}
