elasticdsl
=======



[![Build Status](https://api.travis-ci.org/ropensci/elasticdsl.png)](https://travis-ci.org/ropensci/elasticdsl)

**An R DSL for [Elasticsearch](http://elasticsearch.org)**

## Elasticsearch info

* [Elasticsearch home page](http://elasticsearch.org)
* [API docs](http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/index.html)
* This client is being developed under `v1.4` of Elasticsearch.

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
#>     scene: 
#>       line_id: long 
#>       line_number: string 
#>       play_name: string 
#>       speaker: string 
#>       speech_number: long 
#>       text_entry: string 
#>     act: 
#>       line_id: long 
#>       line_number: string 
#>       play_name: string 
#>       speaker: string 
#>       speech_number: long 
#>       text_entry: string
```

## Print query as pretty json


```r
index("shakespeare") %>%
  filter() %>% 
  ids(c(1, 2, 150)) %>%
  explain()
#> Error: length(url) == 1 is not TRUE
```

## Execute query


```r
index("shakespeare") %>%
  filter() %>% 
  ids(c(1, 2)) %>%
  exec() %>% 
  n()
#> [1] 2
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
#> [[1]]$`_version`
#> [1] 1
#> 
#> [[1]]$`_score`
#> [1] 1
#> 
#> [[1]]$fields
#> [[1]]$fields$play_name
#> [[1]]$fields$play_name[[1]]
#> [1] "Henry IV"
#> 
#> 
#> 
#> 
#> [[2]]
#> [[2]]$`_index`
#> [1] "shakespeare"
#> 
#> [[2]]$`_type`
#> [1] "line"
#> 
#> [[2]]$`_id`
#> [1] "47"
#> 
#> [[2]]$`_version`
#> [1] 1
#> 
#> [[2]]$`_score`
#> [1] 1
#> 
#> [[2]]$fields
#> [[2]]$fields$play_name
#> [[2]]$fields$play_name[[1]]
#> [1] "Henry IV"
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
#> [[1]]$`_version`
#> [1] 1
#> 
#> [[1]]$`_score`
#> [1] 1
#> 
#> [[1]]$fields
#> [[1]]$fields$text_entry
#> [[1]]$fields$text_entry[[1]]
#> [1] "Against the irregular and wild Glendower,"
#> 
#> 
#> [[1]]$fields$play_name
#> [[1]]$fields$play_name[[1]]
#> [1] "Henry IV"
#> 
#> 
#> 
#> 
#> [[2]]
#> [[2]]$`_index`
#> [1] "shakespeare"
#> 
#> [[2]]$`_type`
#> [1] "line"
#> 
#> [[2]]$`_id`
#> [1] "47"
#> 
#> [[2]]$`_version`
#> [1] 1
#> 
#> [[2]]$`_score`
#> [1] 1
#> 
#> [[2]]$fields
#> [[2]]$fields$text_entry
#> [[2]]$fields$text_entry[[1]]
#> [1] "By those Welshwomen done as may not be"
#> 
#> 
#> [[2]]$fields$play_name
#> [[2]]$fields$play_name[[1]]
#> [1] "Henry IV"
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
#> [[1]]$`_version`
#> [1] 1
#> 
#> [[1]]$`_score`
#> [1] 1
#> 
#> [[1]]$fields
#> [[1]]$fields$line_id
#> [[1]]$fields$line_id[[1]]
#> [1] 43
#> 
#> 
#> [[1]]$fields$play_name
#> [[1]]$fields$play_name[[1]]
#> [1] "Henry IV"
#> 
#> 
#> [[1]]$fields$text_entry
#> [[1]]$fields$text_entry[[1]]
#> [1] "Against the irregular and wild Glendower,"
#> 
#> 
#> 
#> 
#> [[2]]
#> [[2]]$`_index`
#> [1] "shakespeare"
#> 
#> [[2]]$`_type`
#> [1] "line"
#> 
#> [[2]]$`_id`
#> [1] "47"
#> 
#> [[2]]$`_version`
#> [1] 1
#> 
#> [[2]]$`_score`
#> [1] 1
#> 
#> [[2]]$fields
#> [[2]]$fields$line_id
#> [[2]]$fields$line_id[[1]]
#> [1] 48
#> 
#> 
#> [[2]]$fields$play_name
#> [[2]]$fields$play_name[[1]]
#> [1] "Henry IV"
#> 
#> 
#> [[2]]$fields$text_entry
#> [[2]]$fields$text_entry[[1]]
#> [1] "By those Welshwomen done as may not be"
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

[![rofooter](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
