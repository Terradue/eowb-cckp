#' A function to define an OGC Web Coverage Service GetCoverage request URL or return the raster associated to the request.
#'
#' @param WCS.access.point URL of the OGC Web Coverage Service access point. The URL can be passed as a GetCoverage request, all parameters will be stripped and replaced with the contents of df.params
#' @param df.params data frame with the Web Coverage Service parameters (column param) and values (column value)
#' @param by.ref boolean to define the retruned object: if TRUE (default) a character URL to the Web Coverage Service is returned
#' if FALSE, a raster object is returned
#' @return WCS request character vector or a raster depending on the by.ref parameter value
#' 
#' @keywords utilities
#' @examples \dontrun{
#' wcs.template <- GetWCSTemplate()
#' 
#' wcs.template$value[wcs.template$param == "service"] <- "WCS" 
#' wcs.template$value[wcs.template$param == "version"] <- "1.0.0"
#' wcs.template$value[wcs.template$param == "request"] <- "GetCoverage"
#' wcs.template$value[wcs.template$param == "coverage"] <- "sla"
#' wcs.template$value[wcs.template$param == "format"] <- "NetCDF3"
#' wcs.template$value[wcs.template$param == "bbox"] <- GetCountryEnvelope("USA")
#' 
#' r <- GetWCSCoverage("http://catalogue.eowb-cckp.terradue.int/thredds/wcs/SeaLevel-ECV/V1.1_20131220/ESACCI-SEALEVEL-L4-MSLA-MERGED-20100815000000-fv01.nc?service=WCS&version=1.0.0&request=GetCoverage&coverage=sla&format=NetCDF3&bbox=0.1,-90,360,90", 
#'   wcs.template, by.ref=FALSE)
#' }
#'
#' @export
#' @import raster httr

GetWCSCoverage <- function(WCS.access.point, df.params, by.ref=TRUE) {

  if(IsURLInvalid(WCS.access.point)) { stop("Invalid WCS access point") }
 
  # remove the NAs if any and keep columns type and value
  df.params <- subset(df.params[complete.cases(df.params),], select=c("type", "value"))

  # get the queryables template, drop the value column
  # since the value column will come from the df.params when doing the merge
  df.template <- subset(GetWCSTemplate(), select = c("type", "param"))

  # merge the template and the parameters
  df.query <- subset(merge(df.template, df.params, by.y=c("type")), select = c("param", "value"))

  # create a named list
  params <- as.list(df.query$value)
  names(params) <- df.query$param
  
  url <- parse_url(WCS.access.point)

  url$query <- params
  
  if (by.ref) {
    return (build_url(url))
  } else {
    tmp.file <- tempfile()
    download.file(build_url(url), tmp.file)
  
    return(raster(tmp.file))
  }
  
}
