#' Gather altmetric metadata from a DOI 
#' 
#' It gathers altemtric metadata from a DOI using Altmetric API (www.altmetric.com).
#' A single research output may live online in multiple websites and can be talked about across dozens of different platforms.
#'  
#' Altmetric is a search engine which collects and collates all of this disparate information to obtain an informative view of the
#' online activity surrounding your scholarly content.
#'
#' @param doi is a character. It contains a list of DOIs. A DOI is a persistent identfier of a scholarly document.
#'
#' @return a data frame. Each row contains the full metadata record for each scholarly document.
#' 
#' For more extensive information about Altmetric, please visit: \href{https://www.altmetric.com/}
#'
#' @examples
#'
#' \dontrun{
#' doi = "10.1016/j.joi.2017.08.007"
#' 
#' df <- altmetric(doi = doi)
#' }
#' 
#'
#' @export
#'
altmetric <- function(doi = "10.1016/j.joi.2017.08.007"){
  
  start <- 0
  pb <- utils::txtProgressBar(min = 1, max = n, initial = 1, char = "=")
  for (i in 1:length(doi)){
    utils::setTxtProgressBar(pb, i)
    
    url <- paste("https://api.altmetric.com/v1/doi/",doi[i],sep="")
    d <- httr::GET(url)
    if (d$status_code==200){
      ## download altmetric metadata for a doi
      DD <- unlist(jsonlite::fromJSON(httr::content(d, "text", encoding = "UTF-8"), simplifyDataFrame = T))
     
      ## save metadata in a data frame
      if (start==0){
        start <- 1
        data <- data.frame(rbind(DD),stringsAsFactors = F)
        items <- names(DD)
      }else{
        missItems <- setdiff(items,names(DD))
        data[missItems]=NA
        lab <- names(DD)
        #items <- intersect(items,names(DD))
        #data <- data[items]
        data[i,lab] <- data.frame(rbind(DD[lab]),stringsAsFactors = F)
      }
    }else{
      #alt$score[i] <- NA
      data[i,] <- NA 
    }
    
  }
  data["doi"] <- doi
  data$score <- as.numeric(data$score)
  row.names(data) <- doi
  
  close(pb)
  
  return(data)
}
