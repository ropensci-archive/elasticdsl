elasticdsl
=======



[![Build Status](https://api.travis-ci.org/ropensci/elasticdsl.png)](https://travis-ci.org/ropensci/elasticdsl)

**An R DSL for [Elasticsearch](http://elasticsearch.org)**

## Elasticsearch info

* [Elasticsearch home page](http://elasticsearch.org)
* [API docs](http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/index.html)
* This client is being developed under `v1.4` of Elasticsearch.

## Quick start

### Install elasticdsl


```r
install.packages("devtools")
devtools::install_github("ropensci/elasticdsl")
```


```r
library('elasticdsl')
```

### Setup

Instructions for installing, upgrading, starting Elasticsearch, and loading example data at [ropensci/elastic](https://github.com/ropensci/elastic#install-elasticsearch)

### Initialization

The function `elastic::connect()` is used before doing anything else to set the connection details to your remote or local elasticdslsearch store. The details created by `connect()` are written to your options for the current session, and are used by `elasticdsl` functions.


```r
library("elastic")
connect(es_port = 9200)
#> uri:       http://127.0.0.1 
#> port:      9200 
#> username:  NULL 
#> password:  NULL 
#> api key:   NULL 
#> elasticsearch details:   
#>       status:                  200 
#>       name:                    Luna 
#>       Elasticsearch version:   1.4.3 
#>       ES version timestamp:    2015-02-11T14:23:15Z 
#>       lucene version:          4.10.3
```

## Meta

* Please [report any issues or bugs](https://github.com/ropensci/elasticdsl/issues)
* License: MIT
* Get citation information for `elasticdsl` in R doing `citation(package = 'elasticdsl')`

[![rofooter](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
