#' Posts a raster to a GeoServer 
#' @description Posts a raster to a GeoServer given an existing workspace and coverage store. 
#'
#' @param access.point GeoServer REST API access point
#' @param workspace GeoServer workspace 
#' @param coverage.store GeoServer coverage store
#' @param raster raster
#' @return charecter vector
#' @examples \dontrun{
#' filename <- "/Users//fbrito/Downloads/ARMOR3D_REPv3-1_20121121_20140420-2.nc"
#' r <- raster(filename)
#' POSTraster("http://localhost:8080/geoserver/rest", workspace="aname", coverage.store="astore", r)
#' }
#'
#' @export
#' @import raster

POSTraster <- function(access.point, workspace, coverage.store, raster) {
  # todo: use RCurl instead of the system call
  # todo: manage GeoServer authentication
  
  # store the current work dir
  orig.wd <- getwd()
  
  # change the work dir to a randon folder
  wd <- tempdir() 
  setwd(wd)
  
  # write the raster 
  writeRaster(raster, filename="temp.tif", format="GTiff", overwrite=TRUE)
  
  # build the access point
  end.point <- paste(access.point, "workspaces", workspace, "coveragestores", coverage.store, "file.geotiff", sep="/")
  
  command.args <- paste("-u admin:geoserver -v -XPUT -H 'Content-type: image/tiff'  --data-binary @temp.tif", end.point, sep=" ")
  
  # got back to the previous work dir
  setwd(orig.wd)
  
  # clean up the temporary folder and temp.tif
  unlink(wd, recursive = TRUE)
  
  # invoke the system call to curl.
  # I'd rather have done this with RCurl but couldn't get it working ;-(
  ret <- system2("curl", command.args, stdout=TRUE, stderr=TRUE)

  return(ret) 

}