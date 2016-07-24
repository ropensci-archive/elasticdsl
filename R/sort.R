#' Sort documents
#'
#' @export
#' @param .data Input
#' @param .dots Input
#' @param ...	Comma separated list of unquoted variable names
#' @examples \dontrun{
#' index("gbif") %>% sort(asc(county))
#' index("gbif") %>% sort(asc(county), desc(country))
#' }
sort <- function(.data=list(), ...){
  sort_(.data, .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname sort
sort_ <- function(.data=list(), ..., .dots){
  pipe_autoexec(toggle = TRUE)
  dots <- lazyeval::all_dots(.dots, ...)
  dots <- proc_sort(.dots)
  .data <- list(index = .data, query = list())
  .data$params <- as.list(c(.data$params, sort = dots))
  structure(.data, class = "esdsl")
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
