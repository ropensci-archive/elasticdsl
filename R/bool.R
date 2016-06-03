#' bool dsl
#'
#' @param .obj An index object. If nothing passed defaults to all indices, equivalent to
#' doing e.g., \code{localhost:9200/_search}
#' @param .dots Explanation...
#' @param ... Further args passed on
#' @examples \dontrun{
#' elastic::connect(errors = "complete")
#'
#' bool(must_not = list(term=list(speaker="KING HENRY IV")))
#' index("shakespeare") %>%
#'    bool(must_not = list(term=list(speaker="KING HENRY IV")))
#' index("shakespeare") %>%
#'    bool(should = list(list(term=list(speech_number=1))))
#' }
bool <- function(.obj=list(), ...){
  bool_(.obj, .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname bool
bool_ <- function(.obj=list(), ..., .dots){
  pipe_autoexec(toggle = TRUE)
  dots <- lazyeval::all_dots(.dots, ...)
  query <- as.json(structure(dots, class = c("bool","lazy_dots")))
  structure(list(index = .obj, query = query), class = "esdsl")
}
