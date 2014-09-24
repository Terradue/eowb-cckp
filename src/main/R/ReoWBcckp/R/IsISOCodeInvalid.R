#'check country code validity
#'@description Checks if the country code entered is invalid
#'
#'@param ISO.Code The 3 letter country code based on ISO3 Country abbreviations (http://unstats.un.org/unsd/methods/m49/m49alpha.htm)
#'@return TRUE if an invalid code and an error message is returned, FALSE if the code is valid
#'@examples \dontrun{
#'IsISOCodeInvalid("PRT")
#'}
#'
#' @export

IsISOCodeInvalid <- function(ISO.code){
codes <- c(NoAm_country,SoAm_country,Oceana_country,Africa_country,Asia_country,Eur_country)
  if (nchar(ISO.code) != 3 && is.character(ISO.code)) { 
    print("Please enter a valid 3 letter country code")
    return(TRUE)
  }
  if (is.numeric(ISO.code)) {
    print("Please enter a 3 letter code, not a number")
    return(TRUE)  
  }
  if (!toupper(ISO.code)%in%codes) {
    print(paste(ISO.code,"is an invalid 3 letter country code, please refer to http://unstats.un.org/unsd/methods/m49/m49alpha.htm for a valid list",sep=" "))
    return(TRUE)
  }
  return(FALSE)
}
