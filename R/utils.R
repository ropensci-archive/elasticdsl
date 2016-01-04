#' elastic DSL utilities
#'
#' @name utils
#' @param x Input
#' @param pretty Pretty print
#' @param .obj Input
#' @param query Query statement
#' @param y  Input
#' @param .dots Input
#' @param ... Further args passed on to \code{\link[jsonlite]{toJSON}}
#' @details Various utilities.
#' \itemize{
#'  \item n - Get a count of number of documents found
#' }
#'
#' @examples \dontrun{
#' # sort
#' x <- index("gbif") %>% filter()
#' x <- x %>% sort(county)
#' x %>% glimpse
#' x %>% exec
#'
#' x <- index("gbif") %>% filter()
#' x <- x %>% sort(asc(county))
#' x %>% glimpse
#' x %>% exec
#'
#' x <- index("gbif") %>% filter()
#' x <- x %>% sort(asc(county), desc(country))
#' x %>% glimpse
#' x %>% exec
#' }
NULL

#' @export
#' @rdname utils
n <- function(x) x$hits$total

#' @export
#' @rdname utils
glimpse <- function(x, pretty = TRUE, ...){
  as.fjson(x, pretty = pretty, ...)
}

#' @export
#' @rdname utils
fields <- function(x, ...){
  structure(x, fields = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname utils
size <- function(x, y) {
  #pipe_autoexec(toggle = TRUE)
  # structure(x, size = y)
  c(x, size = y)
}

#' @export
#' @rdname utils
sort <- function(.obj=list(), ...){
  sort_(.obj, .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname utils
sort_ <- function(.obj=list(), ..., .dots){
  dots <- lazyeval::all_dots(.dots, ...)
  dots <- proc_sort(.dots)
  structure(ec(list(popindex(.obj), structure(dots, class = c("params", "lazy_dots"), param = "sort"))),
            class = "comb", index = getindex(.obj), filtered = getfiltered(.obj),
            operand = attr(.obj, "operand"))
}

proc_sort <- function(x) {
  cl(sapply(x, function(z) {
    tmp <- deparse(z$expr)
    if (grepl("asc", tmp)) {
      paste(gsub("\\(|\\)|(asc)", "", tmp), "asc", sep = ":")
    } else if (grepl("desc", tmp)) {
      paste(gsub("\\(|\\)|(desc)", "", tmp), "desc", sep = ":")
    } else {
      tmp
    }
  }))
}

cl <- function(x) if (is.null(x)) NULL else paste0(x, collapse = ",")

desc <- function(x) {
  -xtfrm(x)
}

# combine query statements
combine <- function(.obj, ..., .dots){
  list(.obj, lazyeval::all_dots(.dots, ...))
}
