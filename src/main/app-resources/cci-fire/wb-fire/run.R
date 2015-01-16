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
wcs.template$value[wcs.template$param == "coverage"] <- "burned_area"
wcs.template$value[wcs.template$param == "format"] <- "NetCDF3"

# waiting time before retry
wait.time <- 1
# number of retry
retries <- 2

# read the stdin into a file
f <- file("stdin")
open(f)

while(length(country.code <- readLines(f, n=1)) > 0) {
  
  if(IsISOCodeInvalid(country.code)){
       rciop.log("DEBUG", paste("Country ISO code:", country.code, "is not valid",sep=" "))
       next;
  }

  rciop.log("DEBUG", paste("Country ISO code:", country.code, sep=" "))

  # complete the WCS request with the country envelope
  wcs.template$value[wcs.template$param == "bbox"] <- GetCountryEnvelope(country.code)
  
  # the country.code 
  if(is.na(wcs.template$value[wcs.template$param == "bbox"])){
       rciop.log("DEBUG", paste("Country ISO code:", country.code, "wrong or no bbox associated to the Country ISO code",sep=" "))
       next;
  }
  
   # get the frontier as SpatialLines
  frontier <- as(GetCountry(country.code),"SpatialLines")

  json.list <- c()
  
  for (i in 1:length(coverages$online.resource)) {

    # no need to split raster, the lat long are in the right range [-180:180 | -90:90]
    rciop.log("INFO", paste(i/length(coverages$online.resource)*100, "Processing date:",  format(as.Date(coverages$start[i]), format="%Y-%m"), sep=" "))
              
    # get the coverage 
    done <- FALSE
    retry <- 0
    while(TRUE){
      # check exit from loop 
      if(retry > retries)
        break;

      tryCatch({
        r <-GetWCSCoverage(coverages$online.resource[i], wcs.template, by.ref=FALSE)
        done <- TRUE
        break;
      },error=function(cond)
      {
        rciop.log("DEBUG", paste("Error:", cond, sep=" "))
        rciop.log("DEBUG", paste("New try in", wait.time, "seconds", sep=" "))
        Sys.sleep(wait.time)
      })  
      retry <- retry + 1
    }
    if (!done){
      rciop.log("DEBUG", paste(country.code, "element not computed", sep=" "))
      next;
    }

    # no need to check for shift, because lat,long in < -180:180 | -90:90 > format
    # get data inside the country confines
    r.innerArea <- mask(r, GetCountry(country.code) )
    
    json.list <- c(json.list, list(list(iso=country.code, var=series[1,"identifier"], time=paste(format(as.Date(coverages$start[i]), format="%Y-%m"), "15", sep="-"), value=cellStats(r.innerArea, stat="mean"))))
    
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