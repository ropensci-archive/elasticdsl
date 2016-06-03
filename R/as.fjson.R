as.fjson <- function(x, ...) UseMethod("as.fjson")

as.fjson.comb <- function(x, ...){
  oper <- attr(x, "operand")

  # get parameters to pass to query
  params <- Filter(function(z) inherits(z, "params"), x)
  params2 <- lapply(params, as.query, comb = TRUE)

  # remove parameter inputs
  x <- Filter(function(z) !inherits(z, "params"), x)
  out <- lapply(x, as.query, comb = TRUE)
  if (length(out) == 1) out <- out[[1]]

  if (!is.null(attr(x, "filtered")) && attr(x, "filtered")) {
    if (is.null(oper)) {
      quer <- list(query = list(filtered = list(filter = out)))
    } else {
      quer <- list(query = list(filtered = list(filter = stats::setNames(list(out), oper))))
    }
  } else {
    quer <- list(query = out)
  }

  if (!is.null(attr(x, "fields"))) {
    quer$fields <- as.character(sapply(attr(x, "fields"), "[[", "expr"))
  }

  if (!is.null(attr(x, "size"))) {
    params2$size <- attr(x, "size")
  }

  if (length(quer$query) == 0) {
    quer <- list()
  }

  list(params = params2, body = jsonlite::toJSON(quer, ..., auto_unbox = TRUE))
}

as.fjson.query <- function(x, ...){
  x <- list(query = list(range = parse_range(get_eq(x[[1]]))))
  jsonlite::toJSON(x, ..., auto_unbox = TRUE)
}

as.fjson.esdsl <- function(x, ...){
  x <- list(query = list(range = parse_range(get_eq(x[[1]]))))
  jsonlite::toJSON(x, ..., auto_unbox = TRUE)
}

as.fjson.range <- function(x, ...){
  x <- list(query = list(range = parse_range(get_eq(x[[1]]))))
  jsonlite::toJSON(x, ..., auto_unbox = TRUE)
}

as.fjson.bool <- function(x, ...){
  tmp <- stats::setNames(list(lazy_eval(x[[1]]$expr)), names(x))
  x <- list(query = list(bool = tmp))
  jsonlite::toJSON(x, ..., auto_unbox = TRUE)
}

as.fjson.geoshape <- function(x, field, ...){
  out <- list()
  for (i in seq_along(x)) {
    dat <- if (is.character(x[[i]]$expr)) {
      unbox(x[[i]]$expr)
    } else {
      list(eval(x[[i]]$expr))
    }
    out[[names(x[i])]] <- dat
  }
  tmp <- stats::setNames(list(list(shape = out)), field)
  alldat <- list(query = list(geo_shape = tmp))
  json <- jsonlite::toJSON(alldat, ..., auto_unbox = TRUE)
  gsub_geoshape(out$type[[1]], json)
}

as.fjson.common <- function(x, field, ...){
  tmp <- stats::setNames(list(list(query = as.character(x$query$expr),
                            cutoff_frequency = as.numeric(x$cutoff_frequency$expr))),
                  as.character(x$field$expr))
  alldat <- list(query = list(common = tmp))
  jsonlite::toJSON(alldat, ..., auto_unbox = TRUE)
}

as.fjson.missing <- function(x, field, ...){
  tmp <- stats::setNames(list(list(query = as.character(x$query$expr),
                            existence = existence, null_value = null_value),
                       as.character(x$field$expr)))
  alldat <- list(query = list(common = tmp))
  jsonlite::toJSON(alldat, ..., auto_unbox = TRUE)
}

as.fjson.prefix <- function(x, field, ...){
  tmp <- stats::setNames(list(x[[1]]$expr), names(x))
  alldat <- list(query = list(constant_score = list(filter = list(prefix = tmp))))
  jsonlite::toJSON(alldat, ..., auto_unbox = TRUE)
}

as.fjson.ids <- function(x, ...){
  tmp <- stats::setNames(list(eval(x[[1]]$expr)), "values")
  alldat <- list(query = list(filtered = list(filter = list(ids = tmp))))
  jsonlite::toJSON(alldat, ..., auto_unbox = TRUE)
}

# as.fjson.sort <- function(x, ...){
#   tmp <-
#     x[[1]]$expr
#   alldat <- list(query = list(filtered = list(filter = list(sort = tmp))))
#   jsonlite::toJSON(alldat, ..., auto_unbox = TRUE)
# }
