#!/usr/bin/Rscript --vanilla --slave --quiet

library("rciop")
library("raster")
library("rOpenSearch")
library("rgeos")
library("ReoWBcckp", lib.loc="/application/share/R/library/")

osd.url <- rciop.getparam("catalogue")
start.date <- rciop.getparam("start.date")
stop.date <- rciop.getparam("stop.date")
response.type <- rciop.getparam("response.type")
count <- rciop.getparam("count")
data.api <- rciop.getparam("data.api")

# prepare the catalogue request
df.params <- GetOSQueryables(osd.url, response.type)

df.params$value[df.params$type == "count"] <- count
df.params$value[df.params$type == "time:start"] <- start.date
df.params$value[df.params$type == "time:end"] <- stop.date

# submit the query
res <- Query(osd.url, response.type, df.params)

#routing variable 
var.series <- "ghrsst-ukmo"   

# create a named list with the WCS online resources and associated start date 
coverages <- list(online.resource=rev(xpathSApply(xmlParse(res), "//dclite4g:DataSet/dclite4g:onlineResource/ws:WCS/@rdf:about")), 
      start=rev(xpathSApply(xmlParse(res), "//dclite4g:DataSet/ical:dtstart", xmlValue)))

# prepare the WCS request
wcs.template <- GetWCSTemplate()

wcs.template$value[wcs.template$param == "service"] <- "WCS" 
wcs.template$value[wcs.template$param == "version"] <- "1.0.0"
wcs.template$value[wcs.template$param == "request"] <- "GetCoverage"
wcs.template$value[wcs.template$param == "coverage"] <- "analysed_sst"
wcs.template$value[wcs.template$param == "format"] <- "NetCDF3"

# waiting time before retry
wait.time <- 1
# number of retry
retries <- 1

# read the stdin into a file
f <- file("stdin")
open(f)

basePolygon <- readWKT("POLYGON((-180 -90, -180 90, 0 90, 0 -90,-180 -90))")

while(length(country.code <- readLines(f, n=1)) > 0) {
  
  jump.to.next.country <- FALSE

  # country code is valid
  if(IsISOCodeInvalid(country.code)){
       rciop.log("DEBUG", paste("Country ISO code:", country.code, "is not valid",sep=" "))
       next;
  }

  rciop.log("DEBUG", paste("Country ISO code:", country.code, sep=" "))

  # complete the WCS request with the country envelope (EEZ). 
  wcs.template$value[wcs.template$param == "bbox"] <- GetCountryEnvelopeEEZ(country.code)
  
  # the country.code bounding box is valid
  if(is.na(wcs.template$value[wcs.template$param == "bbox"])){
       rciop.log("DEBUG", paste("Country ISO code:", country.code, "wrong or no bbox associated to the Country ISO code",sep=" "))
       next;
  }
       
  # clipping using the country boundary
  split.country <- FALSE
  country.extent <- extent(GetCountryEEZ(country.code))

  # work around for data 
  if(country.extent@xmin<0 & country.extent@xmax>0) {
       split.country <- TRUE
  }
    
  coordinates <- unlist(strsplit(wcs.template$value[wcs.template$param == "bbox"], ","))
  country.polygon <- paste("POLYGON((",   coordinates[1], coordinates[2], ",", coordinates[1], coordinates[4], ",",
                                          coordinates[3], coordinates[4], ",", coordinates[3], coordinates[2], ",",
                                          coordinates[1], coordinates[2], "))" )
   
  json.list <- c()
  
  for (i in 1:length(coverages$online.resource)) {

    rciop.log("INFO", paste(i/length(coverages$online.resource)*100, "Processing date:",  format(as.Date(coverages$start[i]), format="%Y-%m-%d"), sep=" "))
    # NOTE: no split needed for  countries crossing the Greenwich meridian (check)
      
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

    tryCatch({
        # clip with the country confines
      r.mask <- mask(r, GetCountryEEZ(country.code) )
      },error=function(cond)
      {
        rciop.log("Debug", paste("Error:", cond, sep=" "))
        jump.to.next.country <- TRUE   
        break;       
    })  

    if(!jump.to.next.country){
      #json.list <- c(json.list, list(list(iso=country.code, var=series[1,"identifier"], time=paste(format(as.Date(coverages$start[i]), format="%Y-%m"), "15", sep="-"), value=cellStats(r.mask, stat="mean"))))
      json.list <- c(json.list, list(list(iso=country.code, var=var.series, time=paste(format(as.Date(coverages$start[i]), format="%Y-%m-%d"), sep="-"), value=cellStats(r.mask, stat="mean"))))
      
      # delete the WCS downloaded raster (the other raster are in memory)
      file.remove(r@file@name)  
    }else{
      rciop.log("ERROR", paste("Error while computing ", country.code, ". No data computed.", sep=" "))
      # compute data for the next country
      next;
    }
    
  }

  # removing object from memory
  rm(r)
  rm(r.mask)

  json.filename <- paste(TMPDIR, "/", country.code, ".json", sep="")

  writeLines(toJSON(list(items=json.list), pretty=TRUE), json.filename)

  res <- rciop.publish(json.filename, metalink=TRUE, recursive=FALSE)
   
  if (res$exit.code==0) { published <- res$output }
  file.remove(json.filename)

  # post json to datastore
  response <- POSTRequest(access.point=data.api, content=toJSON(list(items=json.list)), content.type="application/json")
  if(response$status!=200)
  {
    rciop.log("ERROR", paste("Error while sanding", json.filename, "to the catalogue", sep=" "))  
    rciop.log("ERROR", paste("Message:", response$message, sep=" "))  
  }else {
    rciop.log("DEBUG", paste(json.filename, "correctly sent to the catalogue", sep=" "))  
  }
}



