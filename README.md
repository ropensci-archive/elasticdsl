elasticdsl
=======



[![Build Status](https://api.travis-ci.org/ropensci/elasticdsl.svg)](https://travis-ci.org/ropensci/elasticdsl)
[![Build status](https://ci.appveyor.com/api/projects/status/r810moreouuq18ox?svg=true)](https://ci.appveyor.com/project/sckott/elasticdsl)
[![codecov.io](https://codecov.io/github/ropensci/elasticdsl/coverage.svg?branch=master)](https://codecov.io/github/ropensci/elasticdsl?branch=master)

**An R DSL for [Elasticsearch](http://elasticsearch.org)**

## Elasticsearch info

* [Elasticsearch home page](http://elasticsearch.org)
* [API docs](http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/index.html)
* This client is being developed under `v1.4` of Elasticsearch.

## Security

You're fine running ES locally on your machine, but be careful just throwing up ES on a server with a public IP address - make sure to think about security.

* [Shield](https://www.elastic.co/products/shield) - This is a paid product provided by Elastic - so probably only applicable to enterprise users
* DIY security - there are a variety of techniques for securing your Elasticsearch. A number of resources are collected in a [blog post](http://recology.info/2015/02/secure-elasticsearch/) - tools include putting your ES behind something like Nginx, putting basic auth on top of it, using https, etc.

## Install elasticdsl


```r
install.packages("devtools")
devtools::install_github("ropensci/elasticdsl")
```


```r
library('elasticdsl')
```

## Setup

Instructions for installing, upgrading, starting Elasticsearch, and loading example data at [ropensci/elastic](https://github.com/ropensci/elastic#install-elasticsearch)

## Initialization

The function `elastic::connect()` is used before doing anything else to set the connection details to your remote or local elasticdslsearch store. The details created by `connect()` are written to your options for the current session, and are used by `elasticdsl` functions.


```r
elastic::connect(es_port = 9200)
#> url:       http://127.0.0.1 
#> port:      9200 
#> username:  NULL 
#> password:  NULL 
#> elasticsearch details:   
#>    status:                  200 
#>    name:                    Gloom 
#>    Elasticsearch version:   1.7.2 
#>    ES version timestamp:    2015-09-14T09:49:53Z 
#>    lucene version:          4.10.4
```

## Set the index to use


```r
index("shakespeare")
#> <index> shakespeare 
#>   type: 
#>   mappings: 
#>     line: 
#>       line_id: long 
#>       line_number: string 
#>       play_name: string 
#>       speaker: string 
#>       speech_number: long 
#>       text_entry: string 
...
```

## Print query as pretty json


```r
index("shakespeare") %>%
  filter() %>% 
  ids(c(1, 2, 150)) %>%
  explain() # doesn't exist yet
```

## Execute query


```r
res <- index("shakespeare") %>%
  filter() %>% 
  ids(c(1, 2)) %>%
  exec()
```

## n() to get number of results


```r
index("shakespeare") %>%
  ids(c(1, 2)) %>%
  exec() %>% 
  n()
#> [1] 2
```

## Request size


```r
index("shakespeare") %>%
  filter() %>% 
  prefix(speaker = "we") %>%
  size(2) %>% 
  fields(play_name) %>% 
  exec() %>% 
  n()
#> [1] 44
```

## Request certain fields


```r
s <- index("shakespeare") %>%
  filter() %>% 
  prefix(speaker = "we") %>%
  size(2)
```


```r
s %>% fields(play_name) %>% exec() %>% .$hits %>% .$hits
#> [[1]]
#> [[1]]$`_index`
#> [1] "shakespeare"
#> 
#> [[1]]$`_type`
#> [1] "line"
#> 
#> [[1]]$`_id`
#> [1] "42"
#> 
...
```


```r
s %>% fields(play_name, text_entry) %>% exec() %>% .$hits %>% .$hits
#> [[1]]
#> [[1]]$`_index`
#> [1] "shakespeare"
#> 
#> [[1]]$`_type`
#> [1] "line"
#> 
#> [[1]]$`_id`
#> [1] "42"
#> 
...
```


```r
s %>% fields(play_name, text_entry, line_id) %>% exec() %>% .$hits %>% .$hits
#> [[1]]
#> [[1]]$`_index`
#> [1] "shakespeare"
#> 
#> [[1]]$`_type`
#> [1] "line"
#> 
#> [[1]]$`_id`
#> [1] "42"
#> 
...
```

## Filters vs. queries

Filters are boolean queries and are much more computationally efficient than queries. 

### Filters 

`prefix` filter


```r
index("shakespeare") %>%
  filter() %>% 
  prefix(speaker = "we") %>%
  exec() %>% 
  n()
#> [1] 44
```

`ids` filter


```r
index("shakespeare") %>%
  filter() %>% 
  ids(c(1, 2, 150)) %>%
  exec() %>% 
  n()
#> [1] 3
```

### Queries

`geoshape` query (filters have a much larger range of geo queries)


```r
index("geoshape") %>%
  geoshape(field = "location", type = "envelope", coordinates = list(c(-30, 50), c(30, 0))) %>% 
  n()
#> [1] 10
```

## Meta

* Please [report any issues or bugs](https://github.com/ropensci/elasticdsl/issues)
* License: MIT
* Get citation information for `elasticdsl` in R doing `citation(package = 'elasticdsl')`
* Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

[![rofooter](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
