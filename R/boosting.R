#' boosting dsl
#'
#' @param .obj An index object. If nothing passed defaults to all indices, equivalent to
#' doing e.g., \code{localhost:9200/_search}
#' @param .dots Explanation...
#' @param negative_boost Explanation...
#' @param ... Further args passed on
#' @examples \dontrun{
#' elastic::connect(errors = "complete")
#'
#' # boosting query
#' boost <- '{
#'  "query" : {
#'   "boosting" : {
#'       "positive" : {
#'           "term" : {
#'               "play_name" : "henry"
#'           }
#'       },
#'       "negative" : {
#'           "term" : {
#'               "text_entry" : "thou"
#'           }
#'       },
#'       "negative_boost" : 0.8
#'     }
#'  }
#' }'
#' elastic::Search(index="shakespeare", body=boost)
#' index("shakespeare") %>%
#'  boosting(positive = list(term = list(play_name = "henry")),
#'    negative_boost = 0.8)
#' index("shakespeare") %>%
#'  boosting(positive = list(term = list(play_name = "henry")),
#'          negative = list(term = list(text_entry = "thou")),
#'          negative_boost = 0.8)
#'
#' }
boosting <- function(.obj=list(), ..., negative_boost=NULL){
  boosting_(.obj, .dots = lazyeval::lazy_dots(...), negative_boost = negative_boost)
}

#' @export
#' @rdname boosting
boosting_ <- function(.obj=list(), ..., .dots, negative_boost=NULL){
  pipe_autoexec(toggle = TRUE)
  dots <- lazyeval::all_dots(.dots, ...)
  dots
  # query <- as.json(structure(dots, class = c("boosting", "lazy_dots")), negative_boost = negative_boost)
  # structure(list(index = .obj, query = query), class = "esdsl")
}
