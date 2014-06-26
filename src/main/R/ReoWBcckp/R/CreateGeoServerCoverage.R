#' creates a coverage in a GeoServer instance
#' @description Creates a GeoServer coverage store in a GeoServer instance.
#'
#' @param geoserver.access.point GeoServer REST API access point
#' @param workspace character vector of the GeoServer workspace (must exist)
#' @param coveragestore character vector of the GeoServer coveragestore (must exist)
#' @param name character vector with the coverage store name
#' @param title character vector with the coverage store title
#' @param abstract character vector with the coverage store abstract
#' @param enabled boolean to define if the coverage store is enabled, defaults to TRUE
#' @param native.crs native CRS of the coverage (defaults to EPSG:4326)
#' @param latlon.crs Lat/Lon CRS of the coverage (defaults to EPSG:4326)
#' @return boolean TRUE when the coverage was created, FALSE otherwise (check the message returned)
#' @examples \dontrun{
#' CreateGeoServerCoverage("localhost:8080/geoserver", "acme", "cs", "mycoverage", "this is a title")
#' }
#'
#' @export
#' @import RCurl RJSONIO

CreateGeoServerCoverage <- function(geoserver.access.point, 
    workspace, 
    coveragestore, 
    name, 
    title, 
    abstract="This is the abstract",
    native.crs="EPSG:4326",
    srs="EPSG:4326",
    enabled="true", 
    native.bbox,
    latlon.bbox) {

  access.point <- paste(geoserver.access.point,
                    "workspaces", workspace, 
                    "coveragestores", coveragestore, 
                    "coverages", sep="/")
  print(access.point)
  content.type <- "application/json"
  
  content <- toJSON(list(coverage=list(name=name, 
    title=title,
    abstract=abstract,
    nativeCRS=native.crs,
    srs=srs,
    enabled=enabled,
    nativeBoundingBox=native.bbox,
    latLonBoundingBox=latlon.bbox
  )))
 content 
  # todo: check if workspace exists, if not create it

  return(POSTRequest(access.point, content.type, content))

}
