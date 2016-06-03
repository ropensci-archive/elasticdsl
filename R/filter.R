#' elastic DSL filters
#'
#' new setup, not quite ready yet
#' this will require queries to be passed inside this function call
#' to indicate that you want a query to be a filter type query
#'
#' @export
#' @keywords internal
#' @param .obj An index object. If nothing passed defaults to all indices, equivalent to
#' doing e.g., \code{localhost:9200/_search}
#' @param ... xxxx
#'
#' @examples \dontrun{
#' filter(term, speaker = "KING HENRY IV")
#' }
filter <- function(x) {
  stop("not ready yet", call. = FALSE)
}
