#' elastic DSL filters
#'
#' @name filters
#'
#' @param .obj An index object. If nothing passed defaults to all indices, equivalent to
#' doing e.g., \code{localhost:9200/_search}
#' @param x Input to various functions
#' @param boost (numeric) Sets the boost value of the query. Default: 1.0
#' @param time_zone (character) The time zone
#' @param execution (character) See details.
#' @param cache (logical) To cache or not.
#' @param .dots Further args
#' @param ... Further args passed on
#' @details
#' The execution option controls how the range filter internally executes. The execution
#' option accepts the following values:
#' \itemize{
#'  \item index Uses the field's inverted index in order to determine whether documents
#'  fallwithin the specified range.
#'  \item fielddata Uses fielddata in order to determine whether documents fall within
#'  the specified range.
#' }
#'
#' @examples \dontrun{
#' # DSL filters default to search across all indices
#' bool(must_not = list(term=list(speaker="KING HENRY IV")))
#'
#' # At the end either
#' ## glimpse the query to be sent
#' index("shakespeare") %>%
#'  ids(c(1, 2, 150)) %>%
#'  glimpse()
#' ## or execute
#' index("shakespeare") %>%
#'  ids(c(1, 2, 150)) %>%
#'  exec()
#'
#' # boolean operators, use to assign within the filter
#' index("shakespeare") %>% and() %>% attributes
#' index("shakespeare") %>% or() %>% attributes
#' index("shakespeare") %>% not() %>% attributes
#'
#' # prefix filter
#' index("shakespeare") %>%
#'  prefix(speaker = "we") %>%
#'  n()
#'
#' # ids filter
#' index("shakespeare") %>%
#'  ids(c(1, 2, 150)) %>%
#'  n()
#'
#' # chain queries/filters together
#' index("shakespeare") %>%
#'  filter() %>%
#'  or() %>%
#'  prefix(speaker = "we") %>%
#'  ids(c(1, 2, 150)) %>%
#'  exec() %>%
#'  n()
#'
#' # not filter
#' index("shakespeare") %>%
#'  filter() %>%
#'  not() %>%
#'  prefix(speaker = "we") %>%
#'  exec()
#'
#' # bool filter
#' index("shakespeare") %>%
#'    bool(must_not = list(term=list(speaker="KING HENRY IV"))) %>%
#'    exec
#' index("shakespeare") %>%
#'    bool(should = list(list(term=list(speech_number=1)))) %>%
#'    exec
#'
#' # range filter
#' index("shakespeare") %>%
#'    range( speech_number <= 5 )
#' index("gbif") %>%
#'    filter() %>%
#'    range( decimalLatitude <= 40 ) %>%
#'    exec()
#' index("gbif") %>%
#'    filter() %>%
#'    or() %>%
#'    range(decimalLatitude <= 20) %>%
#'    range(decimalLongitude <= 10) %>%
#'    exec()
#' }
NULL

#' @export
#' @rdname filters
filter <- function(x){
  structure(x, class = c(class(x), "filtered"), filtered = TRUE)
}

#' @export
#' @rdname filters
or <- function(x){
  structure(x, operand = "or")
}

#' @export
#' @rdname filters
and <- function(x){
  structure(x, operand = "and")
}

#' @export
#' @rdname filters
not <- function(x){
  structure(x, operand = "not")
}

#' @export
#' @rdname filters
frange <- function(.obj=list(), ..., boost=1, time_zone=NULL, execution=NULL, cache=FALSE) {
  frange_(.obj, .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname filters
frange_ <- function(.obj=list(), ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  structure(ec(list(popindex(.obj), structure(dots, class=c("range","lazy_dots")))),
            class="comb", index=getindex(.obj), filtered=getfiltered(.obj),
            operand=attr(.obj, "operand"))
}

#' @export
#' @rdname filters
fbool <- function(.obj=list(), ...){
  fbool_(.obj, .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname filters
fbool_ <- function(.obj=list(), ..., .dots){
  dots <- lazyeval::all_dots(.dots, ...)
  structure(ec(list(popindex(.obj), structure(dots, class=c("bool","lazy_dots")))),
            class="comb", index=getindex(.obj), filtered=getfiltered(.obj),
            operand=attr(.obj, "operand"))
}

#' @export
#' @rdname filters
prefix <- function(.obj=list(), ...){
  prefix_(.obj, .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname filters
prefix_ <- function(.obj=list(), ..., .dots){
  #pipe_autoexec(toggle = TRUE)
  dots <- lazyeval::all_dots(.dots, ...)
  # list(popindex(.obj), structure(dots, class=c("prefix","lazy_dots")))
  structure(ec(list(popindex(.obj), structure(dots, class=c("prefix","lazy_dots")))),
            class="comb", index=getindex(.obj), filtered=getfiltered(.obj),
            operand=attr(.obj, "operand"))
#   query <- as.fjson(structure(dots, class=c("prefix","lazy_dots")))
#   execute(.obj, query)
}

#' @export
#' @rdname filters
ids <- function(.obj=list(), ...){
  ids_(.obj, .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname filters
ids_ <- function(.obj=list(), ..., .dots){
  dots <- lazyeval::all_dots(.dots, ...)
  structure(ec(list(popindex(.obj), structure(dots, class=c("ids","lazy_dots")))),
            class="comb", index=getindex(.obj), filtered=getfiltered(.obj),
            operand=attr(.obj, "operand"))
#   dots <- lazyeval::all_dots(.dots, ...)
#   structure(dots, class=c("ids","lazy_dots"))
#   query <- as.fjson(structure(dots, class=c("ids","lazy_dots")))
#   execute(.obj, query)
}

getindex <- function(x){
  clz <- class(x)
  switch(clz[1],
         index = attr(x, "index"),
         lazy_dots = x$index,
         comb = attr(x, "index")
  )
}

getfiltered <- function(x){
  clz <- class(x)
  switch(clz[1],
         index = attr(x, "filtered"),
         comb = attr(x, "filtered")
  )
}

popindex <- function(x){
  clz <- class(x)
  switch(clz[1],
         comb = x[[1]],
         index = NULL
  )
}

# helpers
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
  tmp$eq <- switch(tolower(tmp$eq), lt="lt", gt="gt", ge="gte", le="lte", eq_assign=NA, eq=NA)
  tmp
}

parse_range <- function(x){
  setNames(list(as.list(setNames(x$num, x$eq))), x$var)
}

# combine query statements
combine <- function(.obj, ..., .dots){
  list(.obj, lazyeval::all_dots(.dots, ...))
}
