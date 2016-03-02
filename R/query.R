#' The elastic DSL
#'
#' @import elastic
#' @import lazyeval
#' @importFrom jsonlite unbox
#' @name query
#'
#' @param .obj An index object. If nothing passed defaults to all indices, equivalent to
#' doing e.g., \code{localhost:9200/_search}
#' @param boost Explanation...
#' @param time_zone Explanation...
#' @param execution Explanation...
#' @param cache Explanation...
#' @param .dots Explanation...
#' @param field Explanation...
#' @param negative_boost Explanation...
#' @param query Explanation...
#' @param cutoff_frequency Explanation...
#' @param low_freq_operator Explanation...
#' @param minimum_should_match Explanation...
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
#' # DSL queries default to search across all indices
#' bool(must_not = list(term=list(speaker="KING HENRY IV")))
#'
#' # bool query
#' bool(must_not = list(term=list(speaker="KING HENRY IV")))
#' index("shakespeare") %>%
#'    bool(must_not = list(term=list(speaker="KING HENRY IV")))
#' index("shakespeare") %>%
#'    bool(should = list(list(term=list(speech_number=1))))
#'
#' # range query
#' index("shakespeare") %>% range( speech_number <= 5 )
#' index("shakespeare") %>% range( speech_number >= 5 )
#' # index("shakespeare") %>% range( speech_number <= c(1,5) ) # doens't work
#' # index("shakespeare") %>% range( speech_number >= c(1,5) ) # doens't work
#'
#' # geographic query
#' ## point
#' index("geoshape") %>%
#'    geoshape(field = "location", type = "envelope", coordinates = list(c(-30, 50), c(30, 0)))
#' # circle and radius
#' index("geoshape") %>%
#'    geoshape(field = "location", type = "circle", radius = "2000km",
#'             coordinates = c(-10, 45)) %>%
#'    n()
#' index("geoshape") %>%
#'    geoshape(field = "location", type = "circle", radius = "5000km",
#'             coordinates = c(-10, 45)) %>%
#'    n()
#' # polygon
#' coords <- list(c(80.0, -20.0), c(-80.0, -20.0), c(-80.0, 60.0), c(40.0, 60.0), c(80.0, -20.0))
#' index("geoshape") %>%
#'    geoshape(field = "location", type = "polygon", coordinates = coords) %>%
#'    n()
#'
#' # common query - not working yet
#' # index("shakespeare") %>% common( speech_number <= 5 )
#' }
NULL

#' @export
#' @rdname query
range <- function(.obj=list(), ..., boost=1, time_zone=NULL, execution=NULL, cache=FALSE) {
  range_(.obj, .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname query
range_ <- function(.obj=list(), ..., .dots) {
  pipe_autoexec(toggle = TRUE)
  dots <- lazyeval::all_dots(.dots, ...)
  query <- as.json(structure(dots, class = c("range", "lazy_dots")))
  structure(list(index = .obj, query = query), class = "esdsl")
  #execute(.obj, query)
}

#' @export
#' @rdname query
bool <- function(.obj=list(), ...){
  bool_(.obj, .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname query
bool_ <- function(.obj=list(), ..., .dots){
  pipe_autoexec(toggle = TRUE)
  dots <- lazyeval::all_dots(.dots, ...)
  query <- as.json(structure(dots, class = c("bool","lazy_dots")))
  structure(list(index = .obj, query = query), class = "esdsl")
  #execute(.obj, query)
}

#' @export
#' @rdname query
geoshape <- function(.obj=list(), ..., field=NULL){
  geoshape_(.obj, .dots = lazyeval::lazy_dots(...), field = field)
}

#' @export
#' @rdname query
geoshape_ <- function(.obj=list(), ..., .dots, field=NULL){
  pipe_autoexec(toggle = TRUE)
  dots <- lazyeval::all_dots(.dots, ...)
  query <- as.json(structure(dots, class = c("geoshape", "lazy_dots")), field = field)
  structure(list(index = .obj, query = query), class = "esdsl")
}

#' @export
#' @rdname query
boosting <- function(.obj=list(), ..., negative_boost=NULL){
  boosting_(.obj, .dots = lazyeval::lazy_dots(...), negative_boost = negative_boost)
}

#' @export
#' @rdname query
boosting_ <- function(.obj=list(), ..., .dots, negative_boost=NULL){
  dots <- lazyeval::all_dots(.dots, ...)
  query <- as.json(structure(dots, class = c("boosting", "lazy_dots")), negative_boost = negative_boost)
  structure(list(index = .obj, query = query), class = "esdsl")
  #execute(.obj, query)
}

#' @export
#' @rdname query
common <- function(.obj=list(), field, query=NULL, cutoff_frequency=NULL, low_freq_operator=NULL,
                   minimum_should_match=NULL){
  common_(.obj, field = field, query = query,
          cutoff_frequency = cutoff_frequency,
          low_freq_operator = low_freq_operator,
          minimum_should_match = minimum_should_match)
}

#' @export
#' @rdname query
common_ <- function(.obj=list(), field, query=NULL, cutoff_frequency=NULL, low_freq_operator=NULL,
                    minimum_should_match=NULL){
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



as.json <- function(x, ...) UseMethod("as.json")

as.json.range <- function(x, ...){
  x <- list(query = list(range = parse_range(get_eq(x[[1]]))))
  jsonlite::toJSON(x, ..., auto_unbox = TRUE)
}

as.json.bool <- function(x, ...){
  tmp <- setNames(list(lazy_eval(x[[1]]$expr)), names(x))
  x <- list(query = list(bool = tmp))
  jsonlite::toJSON(x, ..., auto_unbox = TRUE)
}

as.json.geoshape <- function(x, field, ...){
  out <- list()
  for (i in seq_along(x)) {
    dat <- if (is.character(x[[i]]$expr)) {
      unbox(x[[i]]$expr)
    } else {
      list(eval(x[[i]]$expr))
    }
    out[[names(x[i])]] <- dat
  }
  tmp <- setNames(list(list(shape = out)), field)
  alldat <- list(query = list(geo_shape = tmp))
  json <- jsonlite::toJSON(alldat, ..., auto_unbox = TRUE)
  gsub_geoshape(out$type[[1]], json)
}

as.json.common <- function(x, field, ...){
  tmp <- setNames(list(list(query = as.character(x$query$expr),
                            cutoff_frequency = as.numeric(x$cutoff_frequency$expr))),
                  as.character(x$field$expr))
  alldat <- list(query = list(common = tmp))
  jsonlite::toJSON(alldat, ..., auto_unbox = TRUE)
}

gsub_geoshape <- function(type, x){
  switch(type,
         envelope = gsub('\\]\\]\\]', "\\]\\]", gsub('\\[\\[\\[', "\\[\\[", x)),
         circle = gsub('\\]\\]', "\\]", gsub('\\[\\[', "\\[", x)),
         polygon = x
  )
}

get_eq <- function(y) {
  dat <- getParseData(parse(text = deparse(y$expr)))
  tmp <- list(var = dat[ dat$token == "SYMBOL", "text"],
              eq = dat$token[ dat$token %in% c("LT","GT","GE","LE","EQ_ASSIGN","EQ","NE") ],
              num = dat[ dat$token == "NUM_CONST", "text"]
  )
  tmp$eq <- switch(tolower(tmp$eq), lt = "lt", gt = "gt", ge = "gte",
                   le = "lte", eq_assign = NA, eq = NA)
  tmp
}

parse_range <- function(x){
  setNames(list(as.list(setNames(x$num, x$eq))), x$var)
}

# #' @export
# print.equ <- function(x, pretty = TRUE, auto_unbox = TRUE, ...){
#   print(jsonlite::toJSON(unclass(x), pretty=pretty, auto_unbox = auto_unbox))
# }
