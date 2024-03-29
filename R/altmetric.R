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
#' For more extensive information about Altmetric, please visit: https://www.altmetric.com
#' 
#' @examples
#'
#' \dontrun{
#' doi = "10.1016/j.joi.2017.08.007"
#' 
#' df <- altmetric(doi = doi)
#' }
#'
#' @export
#'
altmetric <- function(doi = "10.1016/j.joi.2017.08.007"){
  
  doi <- doi[!is.na(doi)]
  label <- c("doi", "title")
  data <- data.frame(rbind(rep(NA,length(label))))
  names(data)=label
  r <- 0
  start <- 0
  df <- list()
  pb <- oa_progress(length(doi), "Altmetric downloading")
  for (i in 1:length(doi)){
    pb$tick()
    
    url <- paste("https://api.altmetric.com/v1/doi/",doi[i],sep="")
    d <- httr::GET(url)
    if (d$status_code==200){
      r <- r+1
      ## download altmetric metadata for a doi
      DD <- unlist(jsonlite::fromJSON(httr::content(d, "text", encoding = "UTF-8"), simplifyDataFrame = T))
      df[[r]] <- data.frame(rbind(DD),stringsAsFactors = F)
      
      
      # ## save metadata in a data frame
      # missItems <- setdiff(names(DD),label)
      # print(missItems)
      # data[missItems]=NA
      # lab <- names(DD)
      # #items <- intersect(items,names(DD))
      # ##data <- data[items]
      # data[i,lab] <- data.frame(rbind(DD[lab]),stringsAsFactors = F)
      # label <- names(data)
    }
    
  }
  data <- bind_rows(df) 
  data <- data %>% select(order(colnames(data))) %>% 
    select("doi","title", "score", everything()) %>% 
    mutate(score = as.numeric(.data$score)) %>% 
    rename(altmetric_score = .data$score)

  row.names(data) <- NULL

  #close(pb)
  
  return(data)
}

oa_progress <- function(n, text = "converting") {
  progress::progress_bar$new(
    format = paste(" ", text, "[:bar] :percent eta: :eta"),
    total = n, clear = FALSE, width = 60
  )
}
