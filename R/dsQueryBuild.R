#' Generate a DSL query from a set of parameters
#' It generates a valid query, written following the Dimensions Search Language (DSL), from a set of search parameters.
#'
#' @param item is a character. It indicates the type of document to search. 
#' The argument can be equal to \code{item = ("publications", "grants", "patents", "clinical_trials", "policy_documents")}. 
#' Default value is \code{item = "publications"}.
#' @param words is a character. It contains the search terms.
#' @param full.search is logical. If TRUE, full-text search finds all instances of a term (keyword) in a document, or group of documents. If False, the search finds all instances in titles and abstracts only.
#' @param type is a character. It indicates the document type to include in the search. Default is \code{type = "article"}.
#' @param categories is a character. It indicates the research categories to include in the search. If empty \code{categories = ""}, all categories will be included in the search.
#' @param start_year is integer. It indicate the starting publication year of the search timespan.
#' @param end_year is integer. It indicate the ending publication year of the search timespan.
#'
#' @return a character containing the query in DSL format.
#'
#' For more extensive information about Dimensions Search Language (DSL), please visit: \href{https://docs.dimensions.ai/dsl/}{https://docs.dimensions.ai/dsl/}
#'
#' To obtain a free access to Dimenions API fro no commercial use, please visit: \href{https://ds.digital-science.com/NoCostAgreement}{https://ds.digital-science.com/NoCostAgreement}
#'
#' @examples
#'
#' \dontrun{
#' query <- dsQueryBuild(item = "publications", words = "bibliometric*", 
#'                        type = "article", categories = "management", 
#'                        start_year=1980,end_year = 2020)
#' }
#' 
#' @seealso \code{\link{dsApiRequest}}
#' @seealso \code{\link{dsAuth}}
#' @seealso \code{\link{dsApi2df}}
#'
#' @export
#'
dsQueryBuild <- function(item = "publications", words = "bibliometric*", full.search=FALSE, type = "article", categories = "", start_year =NULL, end_year=NULL){

  # item
  # item = c("publications", "grants", "patents", "clinical trials")

  # search terms
  words_query <- paste0(' for "\\"', words, '\\""')

  #search type
  if (isTRUE(full.search)) {
    search_type <- " in full_data "
  } else{
    search_type <- " in title_abstract_only "
  }
  # type
  # type = 'article; chapter'

  # filters

  # by years
  if (is.null(start_year))
    start_year <-  "1900"
  if (is.null(end_year))
    end_year <-  substr(Sys.Date(), 1, 4)
  
  switch(item,
         publications={
           year <- "year in ["
         },
         grants={
           year <- "start_year in ["
           type <- ""
         },
         patents={
           year <- "year in ["
           type <- ""
         },
         clinical_trials={
           year <- "active_years in ["
           type <- ""
           categories <- ""
         },
         policy_documents={
           year <- "year in ["
           type <- ""
           search_type <- gsub("_abstract","",search_type)
         })
  
  filter_period <- paste(year, start_year, ':', end_year, ']')
  
  # by category separated by ;
  # partuial matching '~'
  # categories = "management; economics"

  if (nchar(categories) > 0) {
    a <- trimws(unlist(strsplit(categories, ";")))
    filter_category <-
      paste('category_for.name ~\"',
            a,
            "\" or ",
            collapse = "",
            sep = "")
    filter_category <-
      substr(filter_category, 1, nchar(filter_category) - 4)
  }


  # by document type

  if (nchar(type) > 0) {
    a <- trimws(unlist(strsplit(type, ";")))
    a <- paste('"', a, '"', collapse = ",", sep = "")
    filter_type <- paste('type in [', a, ']')
  } 


  return_item = paste0(item, "[all]")

  #step_query <- ' limit 1000 skip 0'
  
  query <- paste0('search ', item,
                  search_type,
                  words_query,
                  ' where ',
                  filter_period)
  if (nchar(type)>0){
    query <-  paste0(query,
                     ' and ',
                     filter_type)
  }
  
  if (nchar(categories) > 0) {
    query <-  paste0(query,
                     ' and (',
                     filter_category,
                     ')')
  }
  
  query <- paste0(query,
                  ' return ',
                   return_item)
  

  return(query)

}


