#!/usr/bin/Rscript --vanilla --slave --quiet
 
library("rciop")
library("ReoWBcckp")
library("rOpenSearch")
library("raster")

load("/application/.geoserver.authn.RData")

osd.url <- rciop.getparam("catalogue")
start.date <- rciop.getparam("start.date")
end.date <- rciop.getparam("end.date")
response.type <- rciop.getparam("response.type")
count <- rciop.getparam("count")

# get the GeoServer REST access point
geoserver <- rciop.getparam("geoserver")

# prepare the catalogue request
df.params <- GetOSQueryables(osd.url, response.type)

df.params$value[df.params$type == "count"] <- count 
df.params$value[df.params$type == "time:start"] <- start.date
df.params$value[df.params$type == "time:end"] <- end.date 

# submit the query
res <- Query(osd.url, response.type, df.params)

# create a named list with the WCS online resources and associated start date 
coverages <- list(online.resource=rev(xpathSApply(xmlParse(res), "//dclite4g:DataSet/dclite4g:onlineResource/ws:WCS/@rdf:about")), 
      start=rev(xpathSApply(xmlParse(res), "//dclite4g:DataSet/ical:dtstart", xmlValue)))

# prepare the WCS request
wcs.template <- GetWCSTemplate()

wcs.template$value[wcs.template$param == "service"] <- "WCS" 
wcs.template$value[wcs.template$param == "version"] <- "1.0.0"
wcs.template$value[wcs.template$param == "request"] <- "GetCoverage"
wcs.template$value[wcs.template$param == "coverage"] <- "sla"
wcs.template$value[wcs.template$param == "format"] <- "NetCDF3"

# read the stdin into a file
f <- file("stdin")
open(f)

while(length(country.code <- readLines(f, n=1)) > 0) {
  
  rciop.log("DEBUG", paste("Country ISO code:", country.code, sep=" "))
  
  # create the country workspace on GeoServer
  CreateGeoServerWorkspace(geoserver, country.code)
  
  # complete the WCS request with the country envelope (MBR) 
  wcs.template$value[wcs.template$param == "bbox"] <- GetCountryEnvelope(country.code)
  
  r.stack <- c()
  idx <- c()

  for (i in 1:length(coverages$online.resource)) {
  
    rciop.log("INFO", paste(i/length(coverages$online.resource)*100, "Processing date:",  format(as.Date(coverages$start[i]), format="%Y-%m"), sep=" "))
  
    # get the coverage 
    r <- GetWCSCoverage(coverages$online.resource[i], wcs.template, by.ref=FALSE)
    
    # issue on georef for contries with longitudes<0
    r.shift <- shift(r, x=-360,y=0)
    
    # clip with the country EEZ
    r.mask <- mask(r.shift, GetCountryEEZ(country.code))
    
    # add the clipped raster to the list
    r.stack <- c(r.stack, r.mask)

    rciop.log("INFO", format(as.Date(coverages$start[i]), format="%Y-%m"))    
    # update the index
    idx <- c(idx, format(as.Date(coverages$start[i]), format="%Y-%m"))
    
    # delete the WCS downloaded raster (the other raster are in memory)
    file.remove(r@file@name)
  
    # define the coverage store name including variable and the date 
    coverage.store <- paste("sla", format(as.Date(coverages$start[i]), format="%Y-%m"), sep="_")
  
    CreateGeoServerCoverageStore(geoserver, 
                                country.code,
                                coverage.store,
                                TRUE,
                                "GeoTIFF",
                                "file:data/test.tif")
    
    POSTraster(geoserver, country.code, coverage.store, r.mask)
  }

  
# create the stack
my.stack <- setZ(stack(r.stack), idx)
names(my.stack) <- idx

# get the mean value for the country EEZ
stack.mean <- cellStats(my.stack, 'mean')
names(stack.mean) <- idx

# create the named list with the country code and the sla values
sla.list <- list(iso=country.code, sla=stack.mean)

json.filename <- paste(TMPDIR, "/", country.code, ".json", sep="")

rciop.log("DEBUG", print(json.filename))
writeLines(toJSON(sla.list, pretty=TRUE), json.filename)

res <- rciop.publish(json.filename, FALSE, FALSE)
 
if (res$exit.code==0) { published <- res$output }

file.remove(json.filename)
}
