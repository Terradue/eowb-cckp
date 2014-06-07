#' creates a coverage in a GeoServer instance
#' @description Creates a GeoServer coverage store in a GeoServer instance.
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

CreateGeoServerCoverageStore <- function(geoserver.access.point, 
    workspace, 
    coverage.store, 
    name, 
    title, 
    abstract="This is the abstract",
    native.crs="EPSG:4326",
    srs="EPSG:4326",
    enabled=TRUE, 
    native.bbox,
    latlon.bbox) {

  content <- list(coverage=list(name=name, 
    title=title,
    abstract=abstract,
    nativeCRS=native.crs,
    srs=srs,
    enabled=enabled,
    nativeBoundingBox=native.bbox,
    latLonBoundingBox=latlon.bbox
  ))

  server.response <- httpPUT(
          url=paste(geoserver.access.point,
                    "workspaces", workspace, 
                    "coveragestores", coverage.store, 
                    "coverages", sep="/"),
                    content=content)
  
  return(server.response)

}
