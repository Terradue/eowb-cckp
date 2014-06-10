#' creates a coverage store in a GeoServer instance
#' @description Creates a GeoServer coverage store in a GeoServer instance. This function requires the URL of a coverage previously uploaded to GeoServer 
#'
#' @param geoserver.access.point GeoServer REST API access point
#' @param workspace character vector of the GeoServer workspace 
#' @param name character vector with the coverage store name
#' @param enabled boolean to define if the coverage store is enabled, defaults to TRUE
#' @param type character vector with the coverage type, defaults to GeoTIFF
#' @param url URL of a coverage previously uploaded
#' @return boolean TRUE when the coverage store was created, FALSE otherwise (check message returned)
#' @examples \dontrun{
#' CreateGeoServerCoverageStore("localhost:8080/geoserver", "acme", "cs", "file:data/folder/coverage")
#' }
#'
#' @export
#' @import RCurl RJSONIO

CreateGeoServerCoverageStore <- function(geoserver.access.point, workspace, name, enabled=TRUE, type="GeoTIFF", url) {

  # todo: check if workspace exists, if not create it
  access.point <- paste(geoserver.access.point,
                    "workspaces", workspace, 
                    "coveragestores", sep="/")

  content.type <- "application/json"
  
  content <- toJSON(list(coverageStore=list(name=name, 
      enabled=enabled, 
      type=type, 
      url=url,
      workspace=workspace)
      ))

  return(POSTRequest(access.point, content.type, content))
  
}
