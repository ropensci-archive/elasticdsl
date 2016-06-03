#' term dsl
#'
#' @param .obj An index object. If nothing passed defaults to all indices, equivalent to
#' doing e.g., \code{localhost:9200/_search}
#' @param .dots Explanation...
#' @param ... Further args passed on
#' @examples \dontrun{
#' elastic::connect(errors = "complete")
#'
#' index("shakespeare") %>% term(speaker = HOTSPUR)
#' }
term <- function(.obj=list(), ...) {
  term_(.obj, .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname term
term_ <- function(.obj=list(), ..., .dots) {
  pipe_autoexec(toggle = TRUE)
  dots <- lazyeval::all_dots(.dots, ...)
  query <- as.json(structure(dots, class = c("term", "lazy_dots")))
  structure(list(index = .obj, query = query), class = "esdsl")
}
