ec <- function(l) Filter(Negate(is.null), l)

cl <- function(x) if (is.null(x)) NULL else paste0(x, collapse = ",")

pluck <- function(x, name, type) {
  if (missing(type)) {
    lapply(x, "[[", name)
  } else {
    vapply(x, "[[", name, FUN.VALUE = type)
  }
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
  dat <- utils::getParseData(parse(text = deparse(y$expr)))
  tmp <- list(var = dat[ dat$token == "SYMBOL", "text"],
              eq = dat$token[ dat$token %in% c("LT","GT","GE","LE","EQ_ASSIGN","EQ","NE") ],
              num = dat[ dat$token == "NUM_CONST", "text"]
  )
  tmp$eq <- switch(tolower(tmp$eq), lt="lt", gt="gt", ge="gte", le="lte", eq_assign=NA, eq=NA)
  tmp
}

parse_range <- function(x){
  stats::setNames(list(as.list(stats::setNames(x$num, x$eq))), x$var)
}

# combine query statements
combine <- function(.obj, ..., .dots){
  list(.obj, lazyeval::all_dots(.dots, ...))
}

