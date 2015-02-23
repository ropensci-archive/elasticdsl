#' As query functions
#'
#' @name as.query
#' @keywords internal
#' @param x Input
#' @param comb (logical) A combined query or not
#' @param ... Ignored
NULL

#' @export
#' @rdname as.query
as.query <- function(x, comb = FALSE, ...){
  UseMethod("as.query")
}

#' @export
#' @rdname as.query
as.query.prefix <- function(x, field, comb=FALSE, ...){
  tmp <- setNames(list(x[[1]]$expr), names(x))
  if(comb){
    list(prefix = tmp)
  } else {
    list(query = list(constant_score = list(filter = list(prefix = tmp))))
  }
}

#' @export
#' @rdname as.query
as.query.ids <- function(x, comb=FALSE, ...){
  tmp <- setNames(list(eval(x[[1]]$expr)), "values")
  if(comb){
    list(ids = tmp)
  } else {
    list(query = list(filtered = list(filter = list(ids = tmp))))
  }
}

#' @export
#' @rdname as.query
as.query.bool <- function(x, comb=FALSE, ...){
  tmp <- setNames(list(lazy_eval(x[[1]]$expr)), names(x))
  if(comb){
    list(bool = tmp)
  } else {
    list(query = list(filtered = list(filter = list(bool = tmp))))
  }
}

#' @export
#' @rdname as.query
as.query.range <- function(x, comb=FALSE, ...){
  x <- parse_range(get_eq(x[[1]]))
  if(comb){
    list(range = x)
  } else {
    list(query = list(filtered = list(filter = list(range = x))))
  }
}
