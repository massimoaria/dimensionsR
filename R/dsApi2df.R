#' Convert data in json dimensions structure to a dataframe
#' 
#' It converts dimensions data downloaded using DSL API into a data frame
#' 
#' @param P is a list in json dimensions structure downloaded using the function \code{dsApiRequest}.
#' 
#' @return a bibliographic dataframe.
#' 
#' To obtain a free access to Dimenions API fro no commercial use, please visit: \href{https://ds.digital-science.com/NoCostAgreement}{https://ds.digital-science.com/NoCostAgreement}
#' 
#' For more extensive information about dimensions API, please visit: \href{https://www.dimensions.ai/dimensions-apis/}{https://www.dimensions.ai/dimensions-apis/}
#'
#' @examples
#'
#' # token <- dsAuth(username = "my.email@my.domain", password = "mypassword")
#' # query <- dsQueryBuild(item = "publications", words = "bibliometric*", 
#' #                        type = "article", categories = "management", 
#' #                       start_year=1980,end_year = 2020)
#' # D <- dsApiRequest(token = token, query = query, limit = 50000)
#' # M <- dsApi2df(D)
#'
#' @seealso \code{\link{dsApiRequest}}
#' @seealso \code{\link{dsAuth}}
#' @seealso \code{\link{dsQueryBuild}}
#'
#' @export
dsApi2df <- function(P){


#library(bibliometrix)

n <- length(P)


### Data Conversion

df <- data.frame(AU=rep(NA,n), AF="NA",TI="NA", SO="NA", SO_LIST=NA, LA="English", DT=NA,DE=NA,ID=NA,AB="NA",C1=NA,RP=NA,OI=NA,FU=NA,CR=NA,
                 ALT=NA, TC=NA, TCR=NA,PU=NA,SN=NA, J9=NA, JI=NA, PY=NA, VL=NA, IS=NA, DI=NA, PG=NA, SC=NA, OA=NA, URL=NA, DB="DIMENSIONS",
                 AU_UN=NA, AU1_UN=NA, SR_FULL=NA,  stringsAsFactors = FALSE)

for (i in 1:n) {
  if (i%%100==0 | i==n) cat("Documents converted  ",i,"of",n, "\n")
  #print(i)
  if (P[[i]]$type %in% c("article", "chapter")){

    ## Document Type
    if (length(P[[i]]$type)>0){df$DT[i] <- P[[i]]$type}

    ## Title
    if (length(P[[i]]$title)>0){df$TI[i] <- P[[i]]$title}

    ## Publication Year
    if (length(P[[i]]$year)>0){df$PY[i] <- P[[i]]$year}

    ## N. of Co-Authors
    n_AU <- length(P[[i]]$authors)

    ## Authors, Affiliations and Countries
    name <- affiliation <- affiliation_name <- country <- rep(NA, n_AU)
    for (j in 1:n_AU) {
      if (length(P[[i]]$authors[[j]])>0){
        name[j] <-
          paste(P[[i]]$authors[[j]]$last_name, P[[i]]$authors[[j]]$first_name, sep=", ")}else{name[j]="NA"}
      if (length(P[[i]]$authors[[j]]$affiliations)>0){
        if (length(P[[i]]$authors[[j]]$affiliations[[1]]$country)>0) {
          country[j] <- P[[i]]$authors[[j]]$affiliations[[1]]$country
        } else{
          country[j] = ""
        }
        affiliation_name[j] <- P[[i]]$authors[[j]]$affiliations[[1]]$name
        affiliation[j] <-
          if (nchar(country[j])>0){
          paste(affiliation_name[j],
                P[[i]]$authors[[j]]$affiliations[[1]]$city,
                country[j],
                sep = ",")}else{
                  paste(affiliation_name[j],
                        P[[i]]$authors[[j]]$affiliations[[1]]$city,
                        sep = ",")}
      }else{
        affiliation_name[j] <- affiliation[j] <- "NA"
      }
      if (length(P[[i]]$authors[[j]]$corresponding == "TRUE")>0){
      if (P[[i]]$authors[[j]]$corresponding == "TRUE") {
        df$RP[i] <- affiliation[j]
        df$AU1_UN[i] <- affiliation_name[j]
      }}
    }

    df$AF[i] <- paste(name, collapse = ";")
    df$C1[i] <- paste(affiliation, collapse = ";")
    df$AU_CO[i] <- paste(country, collapse = ";")
    df$AU_UN[i] <- paste(affiliation_name, collapse = ";")

    ## Subject categories
    if (length(P[[i]]$category_for )>0){
      SC <- unlist(P[[1]]$category_for)
      df$SC[i] <-
        trimws(gsub('[[:digit:]]+', '', paste(SC[which(names(SC) == "name")], collapse =
                                                ";")))}

    ## Keywords
    if (length(P[[i]]$concepts)>0){
      df$ID[i] <- paste(unlist(P[[i]]$concepts), collapse = ";")
      df$DE[i] <- paste(unlist(P[[i]]$terms), collapse = ";")}

    ## Journals
    switch(df$DT[i],
           chapter={
             if (length(P[[i]]$book_title)>0){
               df$SO[i] <- df$JI[i] <- df$J9[i] <- P[[i]]$book_title}},
           article={
             if (length(P[[i]]$journal$title)>0){
               df$SO[i] <- df$JI[i] <- df$J9[i] <- P[[i]]$journal$title}})



    ## Doi
    if (length(P[[i]]$doi )>0){df$DI[i] <- P[[i]]$doi}

    ## Journal List
    if (length(P[[i]]$journal_lists )>0){
      df$SO_LIST[i] <- paste(unlist(P[[i]]$journal_lists), collapse = ";")}

    ## URL
    if (length(P[[i]]$linkout)){
      df$URL[i] <- P[[i]]$linkout}

    ## Total Citations
    if (length(P[[i]]$times_cited )>0){
      df$TC[i] <- P[[i]]$times_cited}

    ## Altmetrics
    if (length(P[[i]]$altmetric)>0){
      df$ALT[i] <- P[[i]]$altmetric}

    ## Recent TC
    if (length(P[[i]]$recent_citations )>0){
      df$TCR[i] <- P[[i]]$recent_citations}

    ## References
    if (length(P[[i]]$reference_ids )>0){
      df$CR[i] <- paste(unlist(P[[i]]$reference_ids), collapse = ";")}

    ## ISSN
    if (length(P[[i]]$issn )>0){df$SN[i] <- P[[i]]$issn[[1]]}

    ## Pages
    if (length(P[[i]]$pages )>0){df$PG[i] <- P[[i]]$pages}

    ## Founders
    if (length(P[[i]]$funders )>0){
      df$FU[i] <- paste(unlist(lapply(P[[i]]$funders, function(l) {
        l <- unlist(l)
        l <-
          l[which(names(l) %in% c("name", "acronym", "city_name", "country_name"))]
        l <-
          paste(l[c("name", "acronym", "city_name", "country_name")], collapse =
                  ",")
      })), collapse = ";")}

    ## Publisher
    if (length(P[[i]]$publisher )>0){df$PU[i] <- P[[i]]$publisher}

    ## Volume
    if (length(P[[i]]$volume )>0){df$VL[i] <- P[[i]]$volume}

    ## Issue
    if (length(P[[i]]$issue)>0){df$IS[i] <- P[[i]]$issue}

    ## Orcid ID
    if (length(P[[i]]$researchers )>0){
      df$OI[i] <- paste(unlist(lapply(P[[i]]$researchers, function(l) {
        l <- unlist(l)
        l <-
          l[which(names(l) %in% c("last_name", "first_name", "orcid_id"))]
        l <-
          paste(paste(l[c("last_name", "first_name")], collapse = " "), l["orcid_id"], sep =
                  ",")
      })), collapse = ";")}

    ## Open Access
    if (length(P[[i]]$open_access_categories )>0){
      df$OA[i] <- P[[i]]$open_access_categories[[1]]$name}
  }
}
DI<-df$DI
df <- data.frame(lapply(df,toupper),stringsAsFactors = FALSE)
df$DI <- DI

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

return(df)

}
