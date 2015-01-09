#'provides the country polygon(s) 
#'@description Checks if the country code entered is valid 
#' and provides the country spatial polygon(s).
#'
#'@param ISO.Code The 3 letter country code based on ISO3 Country abbreviations (http://unstats.un.org/unsd/methods/m49/m49alpha.htm)
#'@return SpatialPolygons
#'@examples \dontrun{
#'GetCountry("PRT")
#'}
#'
#' @export
#' @import sp

GetCountry <- function(ISO.Code) {

  if (IsISOCodeInvalid(ISO.Code)) { 
       print(paste(ISO.Code, "is not valid", sep=" ")) 
       return (NA)
  }
   
  # because the dataframe as some NA values, i need to subset avoiding it
  result <- try(SpatialPolygons(subset(WB_cntry_cleaned, WB_cntry_cleaned$ISO_3DIGIT==toupper(ISO.Code))@polygons), silent = TRUE)
  #result <- try(SpatialPolygons(WB_cntry_cleaned[WB_cntry_cleaned$ISO_3DIGIT == toupper(ISO.Code),]@polygons), silent = TRUE)
  if (class(result) == "try-error"){
       print(paste("Error rised for the", ISO.Code, "code request")) 
       return (NA)
  }
  
  return(result)
  
}