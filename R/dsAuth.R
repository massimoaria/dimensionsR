#' Obtain an API token from dimensions.ai
#' 
#' It generates a token request to dimensions.ai using account and password.
#'
#' @param username is a character. 
#' @param password is a character.
#' @param key is a character.
#' @param auth_endpoint is a character. It contains the authentication endpoint url of Dimensions. Default is auth_endpoint = "https://app.dimensions.ai/api/auth.json"
#' @param verbose is logical.
#'
#' @return a character cointaining an token o use dimensions API.
#'
#' To obtain a free access to Dimenions API fro no commercial use, please visit: \href{https://ds.digital-science.com/NoCostAgreement}{https://ds.digital-science.com/NoCostAgreement}
#' 
#' For more extensive information about Dimensions API, please visit: \href{https://www.dimensions.ai/dimensions-apis/}{https://www.dimensions.ai/dimensions-apis/}
#'
#' @examples
#'
#' # Obtain a token by username and password
#' \dontrun{
#' token <- dsAuth(username = "my.email@my.domain", password = "mypassword")
#' }
#' 
#' # Obtain a token by API Key
#' 
#' \dontrun{
#' token <- dsAuth(key = "myapikey")
#' }
#'
#' @seealso \code{\link{dsApiRequest}}
#' @seealso \code{\link{dsQueryBuild}}
#' @seealso \code{\link{dsApi2df}}
#'
#' @export
#' @import httr
#' @import jsonlite

dsAuth <- function(username=NULL, password=NULL, key=NULL, auth_endpoint = "https://app.dimensions.ai/api/auth.json", verbose=FALSE) {

  if (!is.null(key)){
    login <- list(key = key,
                  submit = "Login!")
  } else if (!is.null(username)){  
    login <-
      list(username = username,
         password = password,
         submit = "Login!")
  } else {
    print("you have to set a valid api key or username/password to obtain a token!")
    return()
  }

  if (isTRUE(verbose)) {
    r <-
      r <-
      POST(
        auth_endpoint,
        body = login,
        encode = "json",
        verbose()
      )

  } else {
    r <-
      POST(
        auth_endpoint,
           body = login,
           encode = "json")
  }

  ## handling erros
  if (r$status_code != 200) {
    cat("\n Something goes wrong. Server replied incorrect account or password\n")
    error <- 1
    return(error)
  }

  token <-  content(r)$token
  return(token)
}
