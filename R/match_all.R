#' match_all dsl
#'
#' @export
#' @param .obj An index object. If nothing passed defaults to all indices, equivalent to
#' doing e.g., \code{localhost:9200/_search}
#' @param boost Explanation...
#' @examples \dontrun{
#' elastic::connect(errors = "complete")
#'
#' # match all
#' index("shakespeare") %>% match_all()
#' index("shakespeare") %>% match_all(boost = 2)
#' }
match_all <- function(.obj=list(), boost = NULL) {
  pipe_autoexec(toggle = TRUE)
  query <- as.json(structure(list(boost = boost), class = c("match_all", "lazy_dots")))
  structure(list(index = .obj, query = query), class = "esdsl")
}
