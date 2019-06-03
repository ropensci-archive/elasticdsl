#' geoshape dsl
#'
#' @export
#' @param .obj An index object. If nothing passed defaults to all indices, equivalent to
#' doing e.g., \code{localhost:9200/_search}
#' @param .dots Explanation...
#' @param field Explanation...
#' @param ... Further args passed on
#' @examples \dontrun{
#' elastic::connect(errors = "complete")
#'
#' ## point
#' index("geoshape") %>%
#'    geoshape(field = "location", type = "envelope", coordinates = list(c(-30, 50), c(30, 0))) %>%
#'    n()
#'
#' ## circle and radius
#' index("geoshape") %>%
#'    geoshape(field = "location", type = "circle", radius = "2000km",
#'             coordinates = c(-10, 45)) %>%
#'    n()
#'
#' index("geoshape") %>%
#'    geoshape(field = "location", type = "circle", radius = "5000km",
#'             coordinates = c(-10, 45)) %>%
#'    n()
#'
#' ## polygon
#' coords <- list(c(80.0, -20.0), c(-80.0, -20.0), c(-80.0, 60.0), c(40.0, 60.0), c(80.0, -20.0))
#' index("geoshape") %>%
#'    geoshape(field = "location", type = "polygon", coordinates = coords) %>%
#'    n()
#' }
geoshape <- function(.obj=list(), ..., field=NULL){
  geoshape_(.obj, .dots = lazyeval::lazy_dots(...), field = field)
}

#' @export
#' @rdname geoshape
geoshape_ <- function(.obj=list(), ..., .dots, field=NULL){
  pipe_autoexec(toggle = TRUE)
  dots <- lazyeval::all_dots(.dots, ...)
  query <- as.json(structure(dots, class = c("geoshape", "lazy_dots")), field = field)
  structure(list(index = .obj, query = query), class = "esdsl")
}
