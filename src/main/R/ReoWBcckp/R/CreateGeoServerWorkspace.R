#' creates a workspace in a GeoServer instance
#' @description Creates a GeoServer workspace in a GeoServer instance. 
#'
#' @param geoserver.access.point GeoServer REST API access point
#' @param workspace workspace name to be created
#' @return boolean TRUE when the workspace was created, FALSE otherwise (check message returned)
#' @examples \dontrun{
#' CreateGeoServerWorkspace("PRT")
#' }
#'
#' @export
#' @import RCurl RJSONIO

CreateGeoServerWorkspace <- function(geoserver.access.point, workspace) {

  content <- toJSON(list(workspace=list(name=workspace)))

  server.response <- httpPUT(url=geoserver.access.point, content=content)
  
  
  
  return(server.response)

}
