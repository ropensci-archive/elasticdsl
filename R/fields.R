#' Retrieve specific fields
#'
#' @export
#' @param .data Input. An object of class \code{esdsl}, or something
#' that can be coerced to that
#' @param ...	Comma separated list of unquoted variable names
#' @param .dots	Used to work around non-standard evaluation
#' @examples \dontrun{
#' index("shakespeare") %>% fields(play_name, speaker)
#' }
fields <- function(.data=list(), ...){
  fields_(.data, .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname fields
fields_ <- function(.data=list(), ..., .dots){
  pipe_autoexec(toggle = TRUE)
  .data <- as_esdsl(.data)
  dots <- lazyeval::all_dots(.dots, ...)
  ff <- paste0(unname(sapply(dots, function(z) deparse(z$expr))), collapse = ",")
  if (nchar(ff) != 0) {
    .data$params <- as.list(c(.data$params, fields = list(ff)))
  }
  structure(.data, class = "esdsl")
}

as_esdsl <- function(x) {
  if (inherits(x, "index")) {
    list(index = x, query = list(), params = list())
  } else {
    x
  }
}
