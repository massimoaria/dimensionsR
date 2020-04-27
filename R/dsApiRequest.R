#' Gather bibliographic records using Digital Science Dimensions API
#' 
#' It gathers bibliographic records from Digigtal Science Dimensions.
#' The function \code{dsApiRequest} queries Dimensions using a DSL query formulated through the function \code{dsQueryBuild}.
#'
#' @param token is a character. It contains a valid token to query Dimensions database through DSL API. The token can be obtain using the function \code{dsAuth} with valid credentials (account and password) .
#' @param query is a character. It contains a search query formulated using the DSL API language. A query can be automatically generated using the function \code{dsQueryBuild}.
#' @param limit is numeric. It indicates the max number of records to download. limit cannot be higher than 50.000 (as stated by Dimensions rules).
#' @param verbose is logical.
#'
#' @return a list cointaining bibliographic metadata downloaded from Dimensions.
#' 
#' To obtain a free access to Dimenions API for no commercial use, please visit: \href{https://ds.digital-science.com/NoCostAgreement}{https://ds.digital-science.com/NoCostAgreement}
#'
#' For more extensive information about dimensions API, please visit: \href{https://www.dimensions.ai/dimensions-apis/}{https://www.dimensions.ai/dimensions-apis/}
#'
#' @examples
#'
#' \dontrun{
#' token <- dsAuth(username = "my.email@my.domain", password = "mypassword")
#' query <- dsQueryBuild(item = "publications", words = "bibliometric*", 
#'                        type = "article", categories = "management", 
#'                        start_year=1980,end_year = 2020)
#' D <- dsApiRequest(token = token, query = query, limit = 50000)
#' }
#'
#' @seealso \code{\link{dsQueryBuild}}
#' @seealso \code{\link{dsAuth}}
#' @seealso \code{\link{dsApi2df}}
#' 
#' @export

dsApiRequest <- function(token, query, limit = 50000, verbose = FALSE){

  l <- 1000
  s <- 0
  stop <-  FALSE
  ds.limit <- 50000
  D <- list()
  cont <- n <- 0

  limit <- max(limit,1)
  
  if (limit > ds.limit){
    limit <-  ds.limit
    cat("\nLimit is to high. Dimensions API rules do not allow to download more than 50.000 records for each query!\nNew limit is set to 50.000\n\n")
  }

  if (limit>l){
    step_query <- paste0(' limit ', l, ' skip ', s)
  } else {
    step_query <- paste0(' limit ', limit, ' skip ', s)
  }

  Q <- paste0(query,step_query)



  while (!isTRUE(stop)) {
    cont <- cont + 1
    if (isTRUE(verbose)) {
      d <-
        POST(
          "https:///app.dimensions.ai/api/dsl.json",
          add_headers(Authorization = paste0("JWT ", token)),
          body = Q,
          encode = "json",
          verbose()
        )
    } else{
      d <-
        POST(
          "https:///app.dimensions.ai/api/dsl.json",
          add_headers(Authorization = paste0("JWT ", token)),
          body = Q,
          encode = "json"
        )
    }

    if (d$status_code != 200) {
      cat("\n Something goes wrong. Try to set verbose = TRUE to display th error log\n")
      error <- 1
      return(error)
    }

    DD <-
      fromJSON(content(d, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    item <- strsplit(query," ")[[1]][2]
    n <- n + length(DD[[item]])
    D <- c(D, DD[[item]])

    if (cont == 1){
      tot <- min(DD$`_stats`$total_count, limit)
    }
    if (tot <= (s + l)) {
      stop <- TRUE
    } else{
      #print(Q)
      s <- s + 1000
      if ((s+l) > limit){
        l <- limit-s
      }
      step_query <- paste0(' limit ', l, ' skip ', s)
      Q <- paste0(query, step_query)

    }
    cat("Records: ", n, "of", tot, "\n")
  }


  P <- list(data = D, item = item, query = query, total_count = DD$`_stats`$total_count)
  return(P)
}
