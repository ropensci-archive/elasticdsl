ec <- function (l) Filter(Negate(is.null), l)

cl <- function(x) if(is.null(x)) NULL else paste0(x, collapse = ",")

pluck <- function(x, name, type) {
  if (missing(type)) {
    lapply(x, "[[", name)
  } else {
    vapply(x, "[[", name, FUN.VALUE = type)
  }
}
