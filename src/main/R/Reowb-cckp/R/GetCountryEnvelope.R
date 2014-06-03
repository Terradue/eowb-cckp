
GetCountryEnvelope <- function(ISO.Code) {

  if (IsISOCodeInvalid(ISO.Code)) { stop(paste(ISO.Code, "is not valid", sep=" ")) }
  
  bbox <- gEnvelope(world_EEZ_V8_2014[world_EEZ_V8_2014$ISO_3digit == "PRT",])@bbox

  print(paste(bbox[1,1], bbox[2,1], bbox[1,2], bbox[2,2], sep=","))
  
}
