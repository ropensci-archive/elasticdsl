# #' @export
# #' @rdname filters
# geo_boundingbox <- function(.obj=list(), ..., field=NULL){
#   geo_boundingbox_(.obj, .dots = lazyeval::lazy_dots(...), field=field)
# }
#
# #' @export
# #' @rdname filters
# geo_boundingbox_ <- function(.obj=list(), ..., .dots, field=NULL){
#   dots <- lazyeval::all_dots(.dots, ...)
#   query <- as.fjson(structure(dots, class=c("geo_boundingbox","lazy_dots")), field=field)
#   execute(.obj, query)
# }
#
# #' @export
# #' @rdname filters
# geo_distance <- function(.obj=list(), ..., field=NULL){
#   geo_distance_(.obj, .dots = lazyeval::lazy_dots(...), field=field)
# }
#
# #' @export
# #' @rdname filters
# geo_distance_ <- function(.obj=list(), ..., .dots, field=NULL){
#   dots <- lazyeval::all_dots(.dots, ...)
#   query <- as.fjson(structure(dots, class=c("geo_distance","lazy_dots")), field=field)
#   execute(.obj, query)
# }
#
# #' @export
# #' @rdname filters
# geo_distance_range <- function(.obj=list(), ..., field=NULL){
#   geo_distance_range_(.obj, .dots = lazyeval::lazy_dots(...), field=field)
# }
#
# #' @export
# #' @rdname filters
# geo_distance_range_ <- function(.obj=list(), ..., .dots, field=NULL){
#   dots <- lazyeval::all_dots(.dots, ...)
#   query <- as.fjson(structure(dots, class=c("geo_distance_range","lazy_dots")), field=field)
#   execute(.obj, query)
# }

# #' @export
# #' @rdname filters
# missing <- function(.obj=list(), ..., existence=NULL, null_value=NULL){
#   missing_(.obj, .dots = lazyeval::lazy_dots(...), existence=existence, null_value=null_value)
# }
#
# #' @export
# #' @rdname filters
# missing_ <- function(.obj=list(), ..., .dots, existence=NULL, null_value=NULL){
#   dots <- lazyeval::all_dots(.dots, ...)
#   query <- as.fjson(structure(dots, class=c("missing","lazy_dots")),
#                    existence=existence, null_value=null_value)
#   execute(.obj, query)
# }
