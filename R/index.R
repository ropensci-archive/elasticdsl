#' Set index, and optionally type
#'
#' @export
#'
#' @param index Index name
#' @param type Type name, Default: NULL, so all types
#' @param what In \code{\link{index}}, whether to get mappings, aliases, or settings.
#' @param ... Further args passed on
#' @examples \dontrun{
#' index("shakespeare")
#' index("shakespeare", "scene")
#' index("shakespeare", "act")
#' index("plos")
#' index("gbif")
#' }
index <- function(index, type=NULL, what="mappings", ...){
  structure(get_map(index, type, ...), class="index", index=index, type=type)
}

#' @export
print.index <- function(x, ...){
  cat("<index>", attr(x, "index"), "\n")
  cat("  type:", attr(x, "type"), "\n")
  nmz <- names(x)
  cat("  mappings:", "\n")
  for(i in seq_along(nmz)){
    cat(sprintf("    %s:", nmz[i]), "\n")
    for(j in seq_along(x[[i]]$properties)){
      tmp <- x[[i]]$properties[j]
      cat(sprintf("      %s: %s", names(tmp), tmp[[1]]$type), "\n")
    }
  }
}

# shake <- get_map("shakespeare")
# shake$line
# # field names
# names(shake$line$properties)
# # field types
# pluck(shake$line$properties, "type", "")
# shake$scene
# shake$act
get_map <- function(index, type=NULL, ...){
  tmp <- mapping_get(index, type, ...)
  tmp[[index]]$mappings
}
