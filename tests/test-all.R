library('testthat')
library('elasticdsl')

if(Sys.getenv("ES_IP") != "") {
  # The ES_IP environmental var can be set the root ip of the
  # Elasticsearch for convenience. Continuous integration uses
  # the default localhost address.
  #
  # If developing with docker, it is possible to forward the VM's ip
  # to localhost using a reverse proxy server like nginx.
  # https://forums.docker.com/t/using-localhost-for-to-access-running-container/3148/6
  invisible(elastic::connect(Sys.getenv("ES_IP")))
} else {
  invisible(elastic::connect())
}

shakespeare <- system.file("examples", "shakespeare_data.json", package = "elastic")
invisible(elastic::docs_bulk(shakespeare))
gbif <- system.file("examples", "gbif_data.json", package = "elastic")
invisible(elastic::docs_bulk(gbif))

test_check("elasticdsl")
