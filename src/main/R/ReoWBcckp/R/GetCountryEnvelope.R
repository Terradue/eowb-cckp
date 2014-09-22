#'provides the country EEZ envelope
#'@description Checks if the country code entered is valid 
#' and provides the geographical envelope of the country exclusive economic zone (EEZ) 
#'
#'@param ISO.Code The 3 letter country code based on ISO3 Country abbreviations (http://unstats.un.org/unsd/methods/m49/m49alpha.htm)
#'@return character representation of the geographical envelope as xmin, ymin, xmax, ymax.
#'@examples \dontrun{
#'GetCountryEnvelope("PRT")
#'}
#'
#' @export
#' @import rgeos

myGetCountryEnvelope <- function(ISO.Code) {

  #if (IsISOCodeInvalid(ISO.Code)) { stop(paste(ISO.Code, "is not valid", sep=" ")) }
  if (IsISOCodeInvalid(ISO.Code)) { 
      print(paste(ISO.Code, "is not valid", sep=" ")) 
      return (NA)
  }
     
  temp <- try(world_EEZ_V8_2014[world_EEZ_V8_2014$ISO_3digit == toupper(ISO.Code),], silent = TRUE)
  if (class(temp) == "try-error") {
       print(paste("No bounding box associated to", ISO.Code)) 
       return (NA)
  } 
    
  bbox <- gEnvelope(temp)@bbox
  print(paste(bbox[1,1], bbox[2,1], bbox[1,2], bbox[2,2], sep=","))     
}
