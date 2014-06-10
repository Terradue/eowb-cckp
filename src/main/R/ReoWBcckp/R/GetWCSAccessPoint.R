#' A function to return the OGC Web Coverage Request access point 
#'
#' @param WCS.url URL of the WCS service
#' @return a data frame with three columns: param, type, value (NAs) containing the OGC WCS parameters
#' @keywords utilities
#' @examples \dontrun{
#' GetWCSTemplate()
#'
#' }
#' @export
#' @import httr

GetWCSAccessPoint <- function(WCS.url) {
  
  # todo: manage port
  
  url.elements <- parse_url(WCS.url)
  
  access.point <- paste(url.elements$scheme, url.elements$hostname, sep="://")
  
  access.point <- paste(access.point, url.elements$path, sep="/")
  
  return(access.point)
  
}
