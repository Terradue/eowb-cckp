#'provides the country EEZ spatial polygon(s)
#'@description Checks if the country code entered is valid 
#' and provides the country exclusive economic zone (EEZ) spatial polygon(s).
#'
#'@param ISO.Code The 3 letter country code based on ISO3 Country abbreviations (http://unstats.un.org/unsd/methods/m49/m49alpha.htm)
#'@return SpatialPolygons
#'@examples \dontrun{
#'GetCountryEEZ("PRT")
#'}
#'
#' @export
#' @import sp

GetCountryEEZ <- function(ISO.Code) {

  if (IsISOCodeInvalid(ISO.Code)) { stop(paste(ISO.Code, "is not valid", sep=" ")) }
  
  result <- try(SpatialPolygons(world_EEZ_V8_2014[world_EEZ_V8_2014$ISO_3digit == toupper(ISO.Code),]@polygons), silent = TRUE)
  if (class(result) == "try-error") 
       stop(paste("No EEZ associated to", ISO.Code)) 
  
  return(result)
  
}
