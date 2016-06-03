#' common dsl
#'
#' @param .obj An index object. If nothing passed defaults to all indices, equivalent to
#' doing e.g., \code{localhost:9200/_search}
#' @param field Explanation...
#' @param query Explanation...
#' @param cutoff_frequency Explanation...
#' @param low_freq_operator Explanation...
#' @param minimum_should_match Explanation...
#' @examples \dontrun{
#' elastic::connect(errors = "complete")
#'
#' # common query - not working yet
#' # index("shakespeare") %>% common( speech_number <= 5 )
#'
#' ## common query without DSL
#' x <- '{
#'   "query": {
#'     "query_string" : {
#'       "default_field" : "text_entry",
#'       "query" : "this AND that OR thus"
#'     }
#'   }
#' }'
#' Search("shakespeare", body = body)
#'
#' body <- '{
#'  "query" : {
#'    "common": {
#'       "body": {
#'            "query": "against"
#'        }
#'      }
#'   }
#' }'
#' elastic::Search('shakespeare', body=body)
#' }
common <- function(.obj=list(), field, query=NULL, cutoff_frequency=NULL, low_freq_operator=NULL,
                   minimum_should_match=NULL){
  common_(.obj, field = field, query = query,
          cutoff_frequency = cutoff_frequency,
          low_freq_operator = low_freq_operator,
          minimum_should_match = minimum_should_match)
}

#' @export
#' @rdname common
common_ <- function(.obj=list(), field, query=NULL, cutoff_frequency=NULL, low_freq_operator=NULL,
                    minimum_should_match=NULL){
  pipe_autoexec(toggle = TRUE)
  args <- ec(list(field = field, query = query,
                  cutoff_frequency = cutoff_frequency,
                  low_freq_operator = low_freq_operator,
                  minimum_should_match = minimum_should_match))
  dots <- lazyeval::as.lazy_dots(args)
  query <- as.json(
    structure(dots, class = c("common", "lazy_dots"))
  )
  execute(.obj, query)
}
