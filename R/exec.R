#' Execute Elasticsearch query
#'
#' @export
#' @rdname utils
exec <- function(.obj, query, ...) {
  pipe_autoexec(toggle = FALSE)
  tmp <- as.fjson(.obj)
  Search_(attr(.obj, "index"), body = tmp$body, params = tmp$params, ...)
}

exec2 <- function(.obj, query, ...) {
  pipe_autoexec(toggle = FALSE)
  Search_(.obj$index, body = .obj$query, params = .obj$params, ...)
}

# execute on Search
execute <- function(.obj, query){
  Search_(.obj, body = query)
}

f_index <- function(x) {
  if (inherits(x, "index")) attr(x, "index") else x
}
