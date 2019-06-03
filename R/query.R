#' query dsl
#'
#' @export
#' @param .data An index object. If nothing passed defaults to all indices, equivalent to
#' doing e.g., \code{localhost:9200/_search}
#' @param .dots Explanation...
#' @param ... Further args passed on
#' @details
#' The DSL for \code{elastic} makes it easy to do queries against an Elasticsearch
#' instance, either local or remote.
#'
#' The workflow with the DSL:
#' \enumerate{
#'  \item Start with the index to use, e.g., \code{index("shakespeare")}
#'  \item Define queries, e.g., \code{range(speech_number == 5, line_id > 3)}
#'  \item Execute search, e.g., \code{Search()}
#' }
#'
#' Alternatively, if nothing follows a query definition, \code{\link{Search}} is called
#' to execute the search with the query as given. In a sense, this is essentially like
#' what \code{dplyr} does.
#' @examples \dontrun{
#' elastic::connect(errors = "complete")
#'
#' index("shakespeare") %>% query("prefix", speaker = "we")
#' }
query <- function(.data=list(), ...) {
  query_(.data, .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname query
query_ <- function(.data=list(), ..., .dots) {
  pipe_autoexec(toggle = TRUE)
  dots <- lazyeval::all_dots(.dots, ...)
  # first lazy arg must be unnamed, of length 1, and be in an acceptable set
  if (
    names(dots)[1] != "" ||
    length(dots[[1]]$expr) != 1 ||
    !dots[[1]]$expr %in% names(query_names)) {

    stop("first argument must be an unnamed query term,\nof length 1,\nand in the set ?query_names", call. = FALSE)
  }
  # second + other lazy args must be named
  if (any(names(dots[-1]) == "")) {
    stop("additional arguments must be named", call. = FALSE)
  }
  query <-
    as.json(structure(dots[-1], class = c(dots[[1]]$expr, "lazy_dots")))
  structure(list(index = .data, query = query), class = "esdsl")
}

query_names <- list(
  common = NULL,
  fuzzy = NULL,
  fuzzy_like_this = NULL,
  fuzzy_like_this_field = NULL,
  geo_shape = NULL,
  ids = NULL,
  match = NULL,
  match_phrase = NULL,
  match_phrase_prefix = NULL,
  more_like_this = NULL,
  more_like_this_field = NULL,
  multi_match = NULL,
  prefix = NULL,
  query_string = NULL,
  range = NULL,
  regexp = NULL,
  simple_query_string = NULL,
  span_term = NULL,
  template = NULL,
  term = NULL,
  terms = NULL,
  wildcard = NULL
)
