as.json <- function(x, ...) UseMethod("as.json")

as.json.range <- function(x, ...){
  x <- list(query = list(range = parse_range(get_eq(x[[1]]))))
  jsonlite::toJSON(x, ..., auto_unbox = TRUE)
}

as.json.match_all <- function(x, ...){
  if (is.null(x$boost)) {
    boost <- c()
  } else {
    boost <- list(boost = x$boost)
  }
  x <- list(query = list(match_all = boost))
  jsonlite::toJSON(x, ..., auto_unbox = TRUE)
}

as.json.term <- function(x, ...){
  tmp <- stats::setNames(list(as.character(x[[1]]$expr)), names(x))
  x <- list(query = list(term = c(tmp)))
  jsonlite::toJSON(x, ..., auto_unbox = TRUE)
}

as.json.bool <- function(x, ...){
  tmp <- stats::setNames(list(lazy_eval(x[[1]]$expr)), names(x))
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
  tmp <- stats::setNames(list(list(shape = out)), field)
  alldat <- list(query = list(geo_shape = tmp))
  json <- jsonlite::toJSON(alldat, ..., auto_unbox = TRUE)
  gsub_geoshape(out$type[[1]], json)
}

as.json.common <- function(x, field, ...){
  tmp <- stats::setNames(list(list(query = as.character(x$query$expr),
                            cutoff_frequency = as.numeric(x$cutoff_frequency$expr))),
                  as.character(x$field$expr))
  alldat <- list(query = list(common = tmp))
  jsonlite::toJSON(alldat, ..., auto_unbox = TRUE)
}

as.json.boosting <- function(x, field, ...){
  tmp <- stats::setNames(list(list(query = as.character(x$query$expr),
                            cutoff_frequency = as.numeric(x$cutoff_frequency$expr))),
                  as.character(x$field$expr))
  alldat <- list(query = list(common = tmp))
  jsonlite::toJSON(alldat, ..., auto_unbox = TRUE)
}

as.json.prefix <- function(x, ...){
  tmp <- stats::setNames(list(as.character(x[[1]]$expr)), names(x))
  x <- list(query = list(prefix = c(tmp)))
  jsonlite::toJSON(x, ..., auto_unbox = TRUE)
}

as.json.ids <- function(x, type, ...){
  tmp <- as.character(unname(sapply(x, function(z) z$expr)))
  x <- list(query = list(ids = ec(list(type = type, values = tmp))))
  jsonlite::toJSON(x, ..., auto_unbox = TRUE)
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
  dat <- utils::getParseData(parse(text = deparse(y$expr)))
  tmp <- list(var = dat[ dat$token == "SYMBOL", "text"],
              eq = dat$token[ dat$token %in% c("LT","GT","GE","LE","EQ_ASSIGN","EQ","NE") ],
              num = dat[ dat$token == "NUM_CONST", "text"]
  )
  tmp$eq <- switch(tolower(tmp$eq), lt = "lt", gt = "gt", ge = "gte",
                   le = "lte", eq_assign = NA, eq = NA)
  tmp
}

parse_range <- function(x){
  stats::setNames(list(as.list(stats::setNames(x$num, x$eq))), x$var)
}
