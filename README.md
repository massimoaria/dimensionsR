
# dimensionsR

<!-- badges: start -->
<!-- badges: end -->

The goal of Rdimensions is to gather bibliographic records from DS Dimensions using DSL API


## Installation

You can install the developer version of dimensionsR from [github](https://github.com) with:

``` r
install.packages("devtools")
devtools::install_github("massimoaria/dimensionsR")
```

You can install the released version of dimensionsR from [CRAN](https://CRAN.R-project.org) with:

``` r
# not yet on CRAN
# install.packages("dimensionsR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(dimensionsR)

token <- dsAuth(username = "username", password = "password")

query <- dsQueryBuild(item = "publications", 
                      words = "bibliometric*", 
                      type = "article", 
                      categories = "management", 
                      start_year = 1980, end_year = 2020)

D <- dsApiRequest(token = token, query = query, limit = 50000)

M <- dsApi2df(D)

```

"# dimensionsR" 
