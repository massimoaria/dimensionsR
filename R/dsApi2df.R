#' Convert json dimensions bibliographic data into a dataframe
#' 
#' It converts dimensions data, downloaded using DSL API, into a dataframe
#' 
#' @param P is a list in json dimensions structure downloaded using the function \code{dsApiRequest}.
#' @param format is a character. If \code{format = "bibliometrix"} data will be converted in the bibliometrix complatible data format. 
#' If \code{format = "raw"} data will save in a data frame without any other data editing procedure.
#' 
#' @return a dataframe containing bibliographic records or grants information.
#' 
#' To obtain a free access to Dimenions API fro no commercial use, please visit: \href{https://ds.digital-science.com/NoCostAgreement}{https://ds.digital-science.com/NoCostAgreement}
#' 
#' For more extensive information about dimensions API, please visit: \href{https://www.dimensions.ai/dimensions-apis/}{https://www.dimensions.ai/dimensions-apis/}
#' 
#' For more extensive information about bibliometrix R packagee, please visit: \href{https://www.bibliometrix.org}{https://www.bibliometrix.org}
#' 
#' @examples
#'
#'
#' # Example 1: Querying a collection of publications
#' 
#' \dontrun{
#' token <- dsAuth(username = "my.email@my.domain", password = "mypassword")
#' query <- dsQueryBuild(item = "publications", words = "bibliometric*", 
#'                        type = "article", categories = "management", 
#'                        start_year=1980,end_year = 2020)
#' D <- dsApiRequest(token = token, query = query, limit = 50000)
#' M <- dsApi2df(D)
#' }
#'
#' # Example 2: Querying a collection of grants
#'
#' \dontrun{
#' token <- dsAuth(username = "my.email@my.domain", password = "mypassword")
#' query <- dsQueryBuild(item = "grants", words = "bibliometric*", 
#'                        type = "", categories = "management", 
#'                        start_year=1980,end_year = 2020)
#' D <- dsApiRequest(token = token, query = query, limit = 50000)
#' M <- dsApi2df(D)
#' }
#' 
#' @seealso \code{\link{dsApiRequest}}
#' @seealso \code{\link{dsAuth}}
#' @seealso \code{\link{dsQueryBuild}}
#'
#' @export
dsApi2df <- function(P, format = "bibliometrix"){
  
  query <- P$query
  item <- P$item
  P <- P$data
  
switch(item,
       publications={
         df <- pub2df(P,format)
       },
       grants={
         df <- grants2df(P)
       },
       patents={
         df <- patents2df(P)
       },
       clinical_trials={
         df <- clinicaltrials2df(P)
       },
       policy_documents={
         df <- policydocuments2df(P)
       })

return(df)

}

#### Publications #### 
pub2df <- function(P, format){
  
  n <- length(P)
  
  
  ### Data Conversion
  
  df <- data.frame(AU=rep(NA,n), AF="NA",TI="NA", SO="NA", SO_LIST=NA, LA="English", DT=NA,DE=NA,ID=NA,AB="NA",C1=NA,RP=NA,OI=NA,FU=NA,CR=NA,
                   ALT=NA, TC=NA, TCR=NA,PU=NA,SN=NA, J9=NA, JI=NA, PY=NA, VL=NA, IS=NA, DI=NA, PG=NA, SC=NA, OA=NA, URL=NA, DB="DIMENSIONS",
                   AU_UN=NA, AU1_UN=NA, AU_CO=NA, AU1_CO=NA, SR_FULL=NA,  stringsAsFactors = FALSE)
  
  pb <- utils::txtProgressBar(min = 1, max = n, initial = 1, char = "=")
  
  for (i in 1:n) {
    #if (i%%100==0 | i==n) cat("Documents converted  ",i,"of",n, "\n")
    #print(i)
    utils::setTxtProgressBar(pb, i)
    if (P[[i]]$type %in% c("article", "chapter", "preprint")){
      a <- list2char(P[[i]])
      
      items<- names(a)
      ## Document Type
      df$DT[i] <- a["type"]
      
      df$AB[i] <- a["abstract"]
      
      ## Title
      df$TI[i] <- a["title"]
      
      ## Publication Year
      df$PY[i] <- a["year"]
      
      ### Co-Authors
      AU_last_ind <- which(items == "authors.last_name")
      AU_first_ind <- which(items == "authors.first_name")
      name <-  paste(a[AU_last_ind], a[AU_first_ind], sep=", ")
      df$AF[i] <- paste(name, collapse = ";")
      
      ## Countries
      CO_ind <- which(items == "authors.affiliations.country")
      country <- a[CO_ind]
      
      ## Affiliations
      Aff_name_ind <- which(items == "authors.affiliations.name")
      Affiliations <- a[Aff_name_ind]
      
      Aff_city_ind <- which(items == "authors.affiliations.city")
      city <- a[Aff_city_ind]
      
      df$C1[i] <- paste(Affiliations, country, sep=", ", collapse=";")
      
      ## Author's countries
      df$AU_CO[i] <- paste(country, collapse = ";")
      
      ## Author's Affilaiiton standardized
      df$AU_UN[i] <- paste(Affiliations, collapse = ";")
      
      ## Corresponding Author
      AU_corr <- which(items =="authors.corresponding")
      j <- which(a[AU_corr]=="TRUE")[1]
      if (is.na(j)) j <- 1
      
      df$RP[i] <- paste(Affiliations[j],country[j],sep=",",collapse=";")
      df$AU1_UN[i] <- Affiliations[j]
      df$AU1_CO[i] <- country[j]
      
      
      
      ## Subject categories
      SC_ind <- which(items == "category_for.name")
      df$SC[i] <- trimws(gsub('[[:digit:]]+', '', paste(a[SC_ind], collapse =";")))
      
      
      ## Keywords
      ID_ind <- which(regexpr("concepts",items)>-1)
      df$ID[i] <- df$DE[i] <- paste(a[ID_ind],collapse=";")
      
      ## Journals
      
      SO_ind <- which(items %in% c("journal.title", "book_title"))
      df$SO[i] <- a[SO_ind[1]]
      
      
      ## Doi
      df$DI[i] <- a["doi"]
      
      ## Journal List
      SO_list_ind <- which(regexpr("journal_lists",items)>-1)
      df$SO_LIST[i] <- paste(a[SO_list_ind],collapse=";")
      
      ## URL
      df$URL[i] <- a["linkout"]
      
      ## Total Citations
      df$TC[i] <- a["times_cited"]
      
      ## Altmetrics
      df$ALT[i] <- a["altmetric"]
      
      ## Recent TC
      df$TCR[i] <- a["recent_citations"]
      
      
      ## References
      CR_ind <- which(regexpr("reference_ids",items)>-1)
      df$CR[i] <- paste(a[CR_ind], collapse = ";")
      
      ## ISSN
      df$SN[i] <- a["issn"]
      
      ## Pages
      df$PG[i] <- a["pages"]
      
      ## Founders
      FU_name_ind <- which(regexpr("funders.name",items)>-1)
      FU_acronym_ind <- which(regexpr("funders.acronym",items)>-1)
      FU_city <- which(regexpr("funders.city_name",items)>-1)
      FU_country <- which(regexpr("funders.country_name",items)>-1)
      df$FU[i] <- paste(a[FU_name_ind],a[FU_acronym_ind],a[FU_city],a[FU_country],sep=",",collapse=";")
      
      ## Publisher
      df$PU[i] <- a["publisher"]
      
      ## Volume
      df$VL[i] <- a["volume"]
      
      ## Issue
      df$IS[i] <- a["issue"]
      
      ## Orcid ID
      OI_orcid_ind <- which(items == "researchers.orcid_id")
      
      df$OI[i] <- paste(a[OI_orcid_ind],collapse=";")
      
      ## Open Access
      df$OA[i] <- a["open_access_categories.name"]
      
    }
  }
  
  
  if (format == "bibliometrix") {
    DI <- df$DI
    URL <- df$URL
    df <- data.frame(lapply(df, toupper), stringsAsFactors = FALSE)
    df$DI <- DI
    df$URL <- URL
  }
  
  ### PY
  df$PY <- as.numeric(df$PY)
  
  ### TC and TCR
  df$TCR <- as.numeric(df$TCR)
  df$TCR[is.na(df$TCR)] <- 0
  df$TC <- as.numeric(df$TC)
  df$TC[is.na(df$TC)] <- 0
  
  ###  remove empy rows
  df=df[!is.na(df$DT),]
  
  ### Author AU
  
  df$AU <- df$AF
  
  df$AU <- gsub("\\s+", " ", df$AU)
  df$AU <- trimws(gsub("\\(|\\)","",df$AU))
  
  listAU <- strsplit(df$AU, ";")
  
  AU <- lapply(listAU, function(l) {
    lastname <- trimws(gsub(",.*", "", l))
    firstname <- strsplit(trimws(gsub(".*,", "", l)), " ")
    i <- which(nchar(lastname)<2)
    if (length(i)>0){
      lastname <- lastname[-i]
      firstname <- firstname[-i]
    }
    firstname <- lapply(firstname, function(x) {
      if (length(x) > 0) {
        x <- paste(substr(x, 1, 1), sep = "", collapse = "")
      } else {
        x = ""
      }
      return(x)
    })
    AU <- paste(lastname,
                unlist(firstname),
                sep = " ",
                collapse = ";")
    return(AU)
  })
  
  df$AU <- unlist(AU)
  df$AU[df$AU=="NA N"] <- NA
  
  #### To Add in convert2df
  ### SR field creation
  #suppressWarnings(df <- metaTagExtraction(df, Field="SR"))
  
  #row.names(df) <- df$SR
  close(pb)
  
  return(df)
  
}



#### Patents ####
patents2df <- function(P){
  
  n <- length(P)
  
  
  ### Data Conversion
  
  df <- data.frame(title=rep(NA,n), assignee=NA, year=NA, date=NA, exp_date=NA, assignee_country=NA, assignee_city=NA, category=NA,
                   abstract=NA, cited_by=NA, patent_number=NA, jurisdiction=NA, status=NA, citing=NA, references=NA, TC=NA, stringsAsFactors = FALSE)
  
  pb <- utils::txtProgressBar(min = 1, max = n, initial = 1, char = "=")
  
  for (i in 1:n) {
    #if (i%%100==0 | i==n) cat("Documents converted  ",i,"of",n, "\n")
    #print(i)
    utils::setTxtProgressBar(pb, i)
    
    a <- list2char(P[[i]])
    items <- names(a)
    
    ## Title
    df$title[i] <- a["title"]
    

    ## Investigator's affiliations
    Aff_name_ind <- which(regexpr("assignee_names",items)>-1)
    df$assignee[i] <- paste(a[Aff_name_ind], collapse=";")
    
    ## Year
    df$year[i] <- a["year"]
    
    ## date
    df$date[i] <- a['date']
    
    ## Expiration_date
    df$exp_date[i] <- a["expiration_date"]
    
    ## Countries
    CO_ind <- which(items == "assignee_countries.name")
    df$assignee_country[i] <- paste(a[CO_ind], collapse=";")
    
    # City
    Aff_city_ind <- which(items == "current_assignees.city_name")
    df$assignee_city[i] <- paste(a[Aff_city_ind], collapse=";")
    
    ## Subject categories
    SC_ind <- which(items == "category_for.name")
    df$category[i] <- trimws(gsub('[[:digit:]]+', '', paste(a[SC_ind], collapse =";")))
    
    
    ## Abstract
    df$abstract[i] <- a['abstract']
    
    ## Grant number
    cited_ind <- which(regexpr("cited_by_ids",items)>-1)
    df$cited_by[i] <- paste(a[cited_ind], collapse=";")     
    
    ## Patent number
    df$patent_number[i] <- a['id']
    
    ## jurisdiction
    df$jurisdiction[i] <- a["jurisdiction"]
    
    ## status
    df$status[i] <- a["status" ]
    
    ## Citing
    ref_ind <- which(regexpr("reference_ids",items)>-1)
    df$citing[i] <- paste(a[ref_ind], collapse=";")   
    
    ## References
    pub_ind <- which(regexpr("publication_ids",items)>-1)
    df$references[i] <- paste(a[pub_ind], collapse=";") 
    
    ## TC
    df$TC[i] <- a["times_cited"]
    
    
  }
  
  close(pb)
  
  return(df)
  
}





#### grants ####
grants2df <- function(P){
  
  n <- length(P)
  
  
  ### Data Conversion
  
  df <- data.frame(title=rep(NA,n), investigator=NA, role=NA, affiliation=NA, start_year=NA, start_date=NA, end_date=NA, research_org=NA, research_org_country=NA, research_org_city=NA, category=NA,
                   concepts=NA, abstract=NA, funders=NA, grant_number=NA, project_number=NA, URL=NA, language=NA, funding_usd=NA, funding_eur=NA, stringsAsFactors = FALSE)
  
  pb <- utils::txtProgressBar(min = 1, max = n, initial = 1, char = "=")
  
  for (i in 1:n) {
    #if (i%%100==0 | i==n) cat("Documents converted  ",i,"of",n, "\n")
    #print(i)
    utils::setTxtProgressBar(pb, i)
    
      a <- list2char(P[[i]])
      items <- names(a)
      
      ## Title
      df$title[i] <- a["title"]
      
      ## Investigators
      AU_last_ind <- which(items == "investigator_details.last_name")
      AU_first_ind <- which(items == "investigator_details.first_name")
      name <-  paste(a[AU_last_ind], a[AU_first_ind], sep=", ")
      df$investigator[i] <- paste(name, collapse = ";")
      
      ## Role
      Role_ind <- which(items == "investigator_details.role")
      df$role[i] <- paste(a[Role_ind],collapse=";")
      
      ## Investigator's affiliations
      Aff_name_ind <- which(items == "investigator_details.affiliations.name")
      df$affiliation[i] <- paste(a[Aff_name_ind], collapse=";")
      
      ## Start Year
      df$start_year[i] <- a["start_year"]
      
      ## Start date
      df$start_date[i] <- a['start_date']
      
      ## End date
      df$end_date[i] <- a['end_date']
      
      ## Countries
      CO_ind <- which(items == "research_org_countries.name")
      df$research_org_country[i] <- paste(a[CO_ind], collapse=";")
      
      ## Research Organizations
      Aff_name_ind <- which(items == "research_org_name")
      df$research_org[i] <- paste(a[Aff_name_ind],collapse=";")
      
      Aff_city_ind <- which(items == "research_orgs.city_name")
      df$research_org_city[i] <- paste(a[Aff_city_ind], collapse=";")
      
      ## Subject categories
      SC_ind <- which(items == "category_for.name")
      df$category[i] <- trimws(gsub('[[:digit:]]+', '', paste(a[SC_ind], collapse =";")))
      
      
      ## Keywords
      ID_ind <- which(regexpr("concepts",items)>-1)
      df$concepts[i] <- paste(a[ID_ind],collapse=";")
      
      ## Abstract
      df$abstract[i] <- a['abstract']

      ## Funders
      FU_name_ind <- which(regexpr("funders.name",items)>-1)
      FU_acronym_ind <- which(regexpr("funders.acronym",items)>-1)
      FU_city <- which(regexpr("funders.city_name",items)>-1)
      FU_country <- which(regexpr("funders.country_name",items)>-1)
      df$funders[i] <- paste(a[FU_name_ind],a[FU_acronym_ind],a[FU_city],a[FU_country],sep=",",collapse=";")
      
      ## Grant number
      df$grant_number[i] <- a['grant_number']      
      
      ## URL
      df$URL[i] <- a["linkout"]
      
      ## Project numebr
      df$project_number[i] <- a['project_num']
      
      ## Language
      df$language[i] <- a['language']
      
      ## Funding value
      df$funding_usd[i] <- a['funding_usd']
      df$funding_eur[i] <- a['funding_eur']
      
    
  }
  
  close(pb)
  
  return(df)
  
}





#### clinical trials ####
clinicaltrials2df <- function(P){
  
  n <- length(P)
  
  
  ### Data Conversion
  
  df <- data.frame(title=rep(NA,n), start_year=NA, end_year=NA, start_date=NA, research_org=NA, research_org_country=NA, 
                   abstract=NA, registry=NA, id=NA, gender=NA, URL=NA, phase=NA, stringsAsFactors = FALSE)
  
  pb <- utils::txtProgressBar(min = 1, max = n, initial = 1, char = "=")
  
  for (i in 1:n) {
    #if (i%%100==0 | i==n) cat("Documents converted  ",i,"of",n, "\n")
    #print(i)
    utils::setTxtProgressBar(pb, i)
    
    a <- list2char(P[[i]])
    items <- names(a)
    
    ## Title
    df$title[i] <- a["title"]
    
    ## Start Year
    PY_ind <- which(regexpr("active_years",items)>-1)
    PY <- sort(a[PY_ind])
    df$start_year[i] <- PY[1]
    
    ## End Year
    df$end_year[i] <- PY[length(PY)]
    ## Start date
    df$start_date[i] <- a['date']
    
    ## Countries
    CO_ind <- which(items == "research_orgs.country_name")
    df$research_org_country[i] <- paste(a[CO_ind], collapse=";")
    
    ## Research Organizations
    Aff_name_ind <- which(items == "research_orgs.name")
    df$research_org[i] <- paste(a[Aff_name_ind],collapse=";")
    
    ## Abstract
    df$abstract[i] <- a['abstract']
    
    ## Funders
    df$registry[i] <- a['registry']
    
    ## Grant number
    df$id[i] <- a['id']   
    
    ## Gender
    df$gender[i] <- a['gender']
    
    ## URL
    df$URL[i] <- a["linkout"]
    
    ## Phase
    df$phase[i] <- a['phase']
    
  }
  
  close(pb)
  
  return(df)
  
}





#### Policy Documents ####
policydocuments2df <- function(P){
  
  n <- length(P)
  
  
  ### Data Conversion
  
  df <- data.frame(title=rep(NA,n), publisher=NA, year=NA, date_inserted=NA, publisher_country=NA, publisher_city=NA, publisher_type=NA, publisher_url=NA,
                   category=NA, source=NA, id=NA, URL=NA, stringsAsFactors = FALSE)
  
  pb <- utils::txtProgressBar(min = 1, max = n, initial = 1, char = "=")
  
  for (i in 1:n) {
    #if (i%%100==0 | i==n) cat("Documents converted  ",i,"of",n, "\n")
    #print(i)
    utils::setTxtProgressBar(pb, i)
    
    a <- list2char(P[[i]])
    items <- names(a)
    
    ## Title
    df$title[i] <- a["title"]
    
    
    ## Investigator's affiliations
    Aff_name_ind <- which(regexpr("publisher_org.name",items)>-1)
    df$publisher[i] <- paste(a[Aff_name_ind], collapse=";")
    
    ## Year
    df$year[i] <- a["year"]
    
    ## date
    df$date_inserted[i] <- a['date_inserted']
    
    ## Countries
    CO_ind <- which(items == "publisher_org.country_name")
    df$publisher_country[i] <- paste(a[CO_ind], collapse=";")
    
    # City
    Aff_city_ind <- which(items == "publisher_org.city_name")
    df$publisher_city[i] <- paste(a[Aff_city_ind], collapse=";")
    
    # Publisher type
    PT_ind <- which(items == "publisher_org.types")
    df$publisher_type[i] <- paste(a[PT_ind], collapse=";")
    
    # Publisher URL
    df$publisher_url[i] <- a["publisher_org.linkout" ]
    
    
    ## Subject categories
    SC_ind <- which(items == "category_for.name")
    df$category[i] <- trimws(gsub('[[:digit:]]+', '', paste(a[SC_ind], collapse =";")))
    
    ## Source
    df$source[i] <- a["source_name"]
    
    ## Document number
    df$id[i] <- a['id']
    
    ## URL
    df$URL[i] <- a['linkout']
    
    
  }
  
  close(pb)
  
  return(df)
  
}



#### function list2char ####
list2char <- function (x, use.names = TRUE, classes = "ANY") 
{
  lung <- sum(rapply(x, function(x) 1L, classes = classes))
  Ch <- vector("list", lung)
  i <- 0L
  items <- rapply(x, function(x) {
    i <<- i + 1L
    Ch[[i]] <<- x
    TRUE
  }, classes = classes)
  if (use.names && !is.null(nm <- names(items))) 
    names(Ch) <- nm
  Ch <- unlist(Ch)
  return(Ch)
}
