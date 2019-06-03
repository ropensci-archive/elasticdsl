#' Explain a query
#'
#' @export
#' @param .data (list) input, using higher level interface
#' @examples \dontrun{
#' elastic::connect()
#'
#' shakespeare <- system.file("examples", "shakespeare_data.json", package = "elastic")
#' invisible(elastic::docs_bulk(shakespeare))
#' # index("shakespeare") %>% range( speech_number <= 5 ) %>% describe
#'
#' index("shakespeare") %>%
#'    bool(must_not = list(term=list(speaker="KING HENRY IV"))) %>%
#'    describe
#'
#' geoshape <- system.file("examples", "gbif_geoshape.json", package = "elastic")
#' invisible(elastic::docs_bulk(geoshape))
#' index("geoshape") %>%
#'    geoshape(field = "location", type = "envelope",
#'             coordinates = list(c(-30, 50), c(30, 0))) %>%
#'    describe()
#' }
describe <- function(.data) {
  pipe_autoexec(toggle = FALSE)
  if (!inherits(.data, "esdsl")) stop("must be of class esdsl", call. = FALSE)
  structure(make_query(.data), class = "elasticdsl_query")
}

#' @export
print.elasticdsl_query <- function(x, ...) {
  cat("<elasticdsl query>", sep = "\n")
  cat(paste0("  base: ", x$url), sep = "\n")
  cat(paste0("  index: ", x$index), sep = "\n")
  cat(paste0("  query: \n", x$query), sep = "\n")
}

make_query <- function(x) {
  list(
    url = es_make_url(elastic::connection()),
    index = attr(x$index, "index"),
    query = jsonlite::prettify(x$query)
  )
}

es_make_url <- function(x) {
  tmp <- sprintf("%s://%s", x$transport, x$host)
  if (!is.null(x$port) && nchar(x$port) != 0) {
    tmp <- paste(tmp, x$port, sep = ":")
  }
  if (!is.null(x$path)) {
    tmp <- file.path(tmp, x$path)
  }
  sub('/$', '', tmp)
}
