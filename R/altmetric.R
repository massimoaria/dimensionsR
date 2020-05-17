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
  
  label <- c("title","doi","altmetric_jid","issns","journal","cohorts.sci",
             "cohorts.pub","cohorts.com","cohorts.doc","context.all.count",
             "context.all.mean","context.all.rank","context.all.pct",
             "context.all.higher_than","context.journal.count","context.journal.mean",
             "context.journal.rank","context.journal.pct","context.journal.higher_than",
             "context.similar_age_3m.count","context.similar_age_3m.mean",
             "context.similar_age_3m.rank","context.similar_age_3m.pct",
             "context.similar_age_3m.higher_than","context.similar_age_journal_3m.count",
             "context.similar_age_journal_3m.mean","context.similar_age_journal_3m.rank",
             "context.similar_age_journal_3m.pct","context.similar_age_journal_3m.higher_than",
             "authors1","authors2","type","altmetric_id","schema","is_oa",
             "publisher_subjects.name","publisher_subjects.scheme","cited_by_posts_count",
             "cited_by_tweeters_count","cited_by_accounts_count","last_updated","score",
             "history.1y","history.6m","history.3m","history.1m","history.1w","history.6d",
             "history.5d","history.4d","history.3d","history.2d","history.1d","history.at",
             "url","added_on","published_on","scopus_subjects1","scopus_subjects2",
             "scopus_subjects3","scopus_subjects4","scopus_subjects5","readers.citeulike",
             "readers.mendeley","readers.connotea","readers_count","images.small",
             "images.medium","images.large","details_url")
  data <- data.frame(rbind(rep(NA,length(label))))
  names(data)=label
  start <- 0
  pb <- utils::txtProgressBar(min = 1, max = length(doi)+1, initial = 1, char = "=")
  for (i in 1:length(doi)){
    utils::setTxtProgressBar(pb, i)
    
    url <- paste("https://api.altmetric.com/v1/doi/",doi[i],sep="")
    d <- httr::GET(url)
    if (d$status_code==200){
      ## download altmetric metadata for a doi
      DD <- unlist(jsonlite::fromJSON(httr::content(d, "text", encoding = "UTF-8"), simplifyDataFrame = T))
     
      ## save metadata in a data frame
        missItems <- unique(c(setdiff(label,names(DD)),setdiff(names(DD),label)))
        data[missItems]=NA
        lab <- names(DD)
        #items <- intersect(items,names(DD))
        #data <- data[items]
        data[i,lab] <- data.frame(rbind(DD[lab]),stringsAsFactors = F)
      
    }else{
      #alt$score[i] <- NA
      data[i,] <- NA
    }
    
  }
  data["doi"] <- doi
  data$score <- as.numeric(data$score)
  #row.names(data) <- doi
  row.names(data) <- NULL
  
  close(pb)
  
  return(data)
}
