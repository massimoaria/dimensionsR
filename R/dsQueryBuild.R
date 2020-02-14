#' Generate a DSL query froma set of parameters
#' It generates a valid query, written following the Dimensions Search Language (DSL), from a set of search parameters.
#'
#' @param item is a character. It indicates the sub-database to query ("publications", "grants", "patents", "clinical_trias").
#' @param words is a character. It contains the search terms.
#' @param full.search is logical. If TRUE, full-text search finds all instances of a term (keyword) in a document, or group of documents. If False, the search finds all instances in titles and abstracts only.
#'
#' @param query is a character. It contains a search query formulated using the DSL API language. A query can be automatically generated using the function \code{dsQueryBuild}.
#' @param limit is numeric. It indicates the max numebr of records to download. limit cannot be higher than 50.000 (as stated by Dimensions rules).
#' @param verbose is logical.
#'
#' @return a list cointaining bibliographic data downloaded from Dimenbsions.
#'
#' For more extensive information about dimensions API, please see: \href{https://www.dimensions.ai/dimensions-apis/}{https://www.dimensions.ai/dimensions-apis/}
#'
#' @examples
#'
#' # token <- dsAuth(username = "my.email@my.domain", password = "mypassword")
#' # query <- dsQueryBuild(item = "publications", words = "bibliometric*", type = "article", categories="management", start_year=1980,end_year = 2020)
#' # D <- dsApiRequest(token = token, query = query, limit = 50000)
#'
#' @export
#'
dsQueryBuild <- function(item = "publications", words = "bibliometric*", full.search=FALSE, type = "article", categories = "management", start_year = 1900, end_year=NULL){

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


  # by years
  if (is.null(end_year))
    end_year = substr(Sys.Date(), 1, 4)

  filter_period <- paste('year in [', start_year, ':', end_year, ']')

  # by document type

  if (nchar(type) > 0) {
    a <- trimws(unlist(strsplit(type, ";")))
    a <- paste('"', a, '"', collapse = ",", sep = "")
    filter_type <- paste('type in [', a, ']')
  }


  return_item = paste0(item, "[all]")

  #step_query <- ' limit 1000 skip 0'

  if (nchar(categories) > 0) {
    query <-  paste0(
      'search ',
      item,
      search_type,
      words_query,
      ' where ',
      filter_period,
      ' and ',
      filter_type,
      ' and (',
      filter_category,
      ')',
      ' return ',
      return_item
    )
  } else {
    query <-  paste0(
      'search ',
      item,
      search_type,
      words_query,
      ' where ',
      filter_period,
      ' and ',
      filter_type,
      ' return ',
      return_item
    )
  }

  return(query)

}


