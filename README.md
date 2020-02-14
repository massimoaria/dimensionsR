
# dimensionsR

<!-- badges: start -->
<!-- badges: end -->

The goal of Rdimensions is to ...

## Installation

You can install the released version of Rdimensions from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("dimensionsR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(dimensionsR)

token <- dsAuth(username = "username", password = "password")

query <- dsQueryBuild(item = "publications", words = "bibliometric*", type = "article", categories="management", start_year=1980,end_year = 2020)

D <- dsApiRequest(token = token, query = query, limit = 50000)

M <- dsApi2df(D)

```

"# dimensionsR" 
"# dimensionsR" 
