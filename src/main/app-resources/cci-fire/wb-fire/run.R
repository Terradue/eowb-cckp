#!/usr/bin/Rscript --vanilla --slave --quiet

library("rciop")
library("raster")
library("rOpenSearch")
library("rgeos")
library("ReoWBcckp", lib.loc="/application/share/R/library/")

osd.url <- rciop.getparam("catalogue")
start.date <- rciop.getparam("start.date")
end.date <- rciop.getparam("end.date")
response.type <- rciop.getparam("response.type")
count <- rciop.getparam("count")
data.api <- rciop.getparam("data.api")

# prepare the catalogue request
df.params <- GetOSQueryables(osd.url, response.type)

df.params$value[df.params$type == "count"] <- count 
df.params$value[df.params$type == "time:start"] <- start.date
df.params$value[df.params$type == "time:end"] <- end.date 

# submit the query
res <- Query(osd.url, response.type, df.params)

# get the series to retrieve the variable name
series <- xmlToDataFrame(nodes = getNodeSet(xmlParse(res), 
    "//dclite4g:Series"), stringsAsFactors = FALSE)

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

basePolygon <- readWKT("POLYGON((-180 -90, -180 90, 0 90, 0 -90,-180 -90))")

while(length(country.code <- readLines(f, n=1)) > 0) {
  
  if(IsISOCodeInvalid(country.code)){
       rciop.log("DEBUG", paste("Country ISO code:", country.code, "is not valid",sep=" "))
       next;
  }

  rciop.log("DEBUG", paste("Country ISO code:", country.code, sep=" "))

  # complete the WCS request with the country envelope (MBR) 
  wcs.template$value[wcs.template$param == "bbox"] <- GetCountryEnvelope(country.code)
  
  # the country.code 
  if(is.na(wcs.template$value[wcs.template$param == "bbox"])){
       rciop.log("DEBUG", paste("Country ISO code:", country.code, "wrong or no bbox associated to the Country ISO code",sep=" "))
       next;
  }

  # clipping using the confines of the country
  split.country <- FALSE
  country.extent <- extent(GetCountry(country.code))
  if(country.extent@xmin<0 & country.extent@xmax>0) {
       split.country <- TRUE
  }       
  
  # issue on georef for countries with longitudes<0
  coordinates <- unlist(strsplit(wcs.template$value[wcs.template$param == "bbox"], ","))
  country.polygon <- paste("POLYGON((",   coordinates[1], coordinates[2], ",", coordinates[1], coordinates[4], ",",
                                          coordinates[3], coordinates[4], ",", coordinates[3], coordinates[2], ",",
                                          coordinates[1], coordinates[2], "))" )
   
   # get the frontier as SpatialLines
  frontier <- as(GetCountry(country.code),"SpatialLines")

  json.list <- c()
  
  for (i in 1:length(coverages$online.resource)) {

    rciop.log("INFO", paste(i/length(coverages$online.resource)*100, "Processing date:",  format(as.Date(coverages$start[i]), format="%Y-%m"), sep=" "))

    # get the coverage 
    r <- GetWCSCoverage(coverages$online.resource[i], wcs.template, by.ref=FALSE)
   
    if(split.country){

      # the country crosses the Greenwich meridian, need to split the country in 2 parts, the est one and west one

      # west part of the country
      wcs.template.west <- wcs.template
      #bbox: xmin,xmax,ymin,ymax
      wcs.template.west$value[wcs.template.west$param == "bbox"] <- paste(coordinates[1],coordinates[2],0,coordinates[4],sep=",")
      west.country.extent <- country.extent 
      west.country.extent@xmax <- 0
      country.west<-crop(GetCountry(country.code), west.country.extent)
      r.west <- GetWCSCoverage(coverages$online.resource[i], wcs.template.west, by.ref=FALSE)
      r.west.shift <- shift(r.west, x=-360,y=0)
      r.west.mask.shift <- mask(r.west.shift, country.west)
      
      # est part of the country
      wcs.template.est <- wcs.template
      #bbox: xmin,xmax,ymin,ymax
      wcs.template.est$value[wcs.template.est$param == "bbox"] <- paste(0,coordinates[2],coordinates[3],coordinates[4],sep=",")
      est.country.extent <- country.extent 
      est.country.extent@xmin <- 0
      country.est<-crop(GetCountry(country.code), est.country.extent)
      r.est <- GetWCSCoverage(coverages$online.resource[i], wcs.template.est, by.ref=FALSE)
      r.est.mask.shift <- mask(r.est , country.est)
            
      # put the 2 raster together to get the complete country raster
      r.mask <- merge(r.west.mask.shift, r.est.mask.shift, tolerance = 0.1, ext=country.extent)

    } else {
      # country completly in the est or west part
      x.shift <- 0
      if(gContains(basePolygon, readWKT(country.polygon)))
           x.shift <- -360
      
      r.shift <- shift(r, x= x.shift, y=0)
    
      # clip with the country confines
      r.mask <- mask(r.shift, GetCountry(country.code) )
    }

    # get data along the frontier
    # see http://gis.stackexchange.com/questions/92221/extract-raster-from-raster-using-polygon-shapefile-in-r
    cr <- crop(r.mask, extent(frontier), snap="near")                    
    fr <- rasterize(frontier, cr)   
    r.boundary <- mask(x=cr, mask=fr)

    json.list <- c(json.list, list(list(iso=country.code, var=series[1,"identifier"], time=paste(format(as.Date(coverages$start[i]), format="%Y-%m"), "15", sep="-"), value=cellStats(r.boundary, stat="mean"))))
    
    # delete the WCS downloaded raster (the other raster are in memory)
    file.remove(r@file@name)
  }

json.filename <- paste(TMPDIR, "/", country.code, ".json", sep="")

writeLines(toJSON(list(items=json.list), pretty=TRUE), json.filename)

res <- rciop.publish(json.filename, metalink=TRUE, recursive=FALSE)
 
if (res$exit.code==0) { published <- res$output }

file.remove(json.filename)

  # post json to datastore
  #POSTRequest(access.point=data.api, content=toJSON(list(items=json.list)), content.type="application/json")

}