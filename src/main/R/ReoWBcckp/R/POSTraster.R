POSTraster <- function(access.point, workspace, coverage.store, raster) {
  
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
