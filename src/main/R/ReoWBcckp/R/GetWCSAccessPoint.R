#' A function to return the OGC Web Coverage Request access point 
#'
#' @param WCS.url URL of the WCS service
#' @return a data frame with three columns: param, type, value (NAs) containing the OGC WCS parameters
#' @keywords utilities
#' @examples \dontrun{
#' GetWCSTemplate()
#'
#' @export
GetWCSAccessPoint <- function(WCS.url) {

  return(parse_url(wcs.url.template)$path)
  
}
