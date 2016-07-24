#' elasticdsl: An Elasticsearch DSL R client.
#'
#' @section Security:
#'
#' Elasticsearch is insecure out of the box! If you are running Elasticsearch
#' locally on your own machine without exposing a port to the outside world, no
#' worries, but if you install on a server with a public IP address, take the
#' necessary precautions. There are a few options:
#'
#' \itemize{
#'  \item Shield \url{https://www.elastic.co/products/shield} - This is a paid
#'  product - so probably only applicable to enterprise users
#'  \item DIY security - there are a variety of techniques for securing your
#'  Elasticsearch. I collected a number of resources in a blog post at
#'  \url{http://recology.info/2015/02/secure-elasticsearch/}
#' }
#'
#' @examples \dontrun{
#' # start Elasticsearch, then connect
#' elastic::connect()
#'
#' # Search on the index 'shakespeare', and for the prefix 'we'
#' index("shakespeare") %>% query("prefix", speaker = "we")
#' }
#'
#' @import elastic
#' @import lazyeval
#' @importFrom jsonlite unbox
#' @docType package
#' @aliases elasticdsl-package
#' @name elasticdsl
#' @author Scott Chamberlain
NULL
