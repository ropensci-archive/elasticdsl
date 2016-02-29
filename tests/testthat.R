library(testthat)
library(elasticdsl)

test_check("elasticdsl")
connect(paste0("http://", Sys.getenv("ES_IP")))
