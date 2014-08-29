#!/usr/bin/Rscript --vanilla --slave --quiet
 
library("rciop")
library("raster")
library("rOpenSearch")

library("ReoWBcckp", lib.loc="/application/share/R/library/")

osd.url <- rciop.getparam("catalogue")
start.date <- rciop.getparam("start.date")
end.date <- rciop.getparam("end.date")
response.type <- rciop.getparam("response.type")
count <- rciop.getparam("count")

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

while(length(country.code <- readLines(f, n=1)) > 0) {
  
  rciop.log("DEBUG", paste("Country ISO code:", country.code, sep=" "))
  
  # complete the WCS request with the country envelope (MBR) 
  wcs.template$value[wcs.template$param == "bbox"] <- GetCountryEnvelope(country.code)
  
  json.list <- c()
  
  for (i in 1:length(coverages$online.resource)) {
  
    rciop.log("INFO", paste(i/length(coverages$online.resource)*100, "Processing date:",  format(as.Date(coverages$start[i]), format="%Y-%m"), sep=" "))
  
    # get the coverage 
    r <- GetWCSCoverage(coverages$online.resource[i], wcs.template, by.ref=FALSE)
    
    # issue on georef for contries with longitudes<0
    r.shift <- shift(r, x=-360,y=0)
    
    # clip with the country EEZ
    r.mask <- mask(r.shift, GetCountryEEZ(country.code))
  
    json.list <- c(json.list, list(list(iso=country.code, var=series[1,"identifier"], time=paste(format(as.Date(coverages$start[i]), format="%Y-%m"), "15", sep="-"), value=cellStats(r.mask, stat="mean"))))
    
    # delete the WCS downloaded raster (the other raster are in memory)
    file.remove(r@file@name)
  
  }


json.filename <- paste(TMPDIR, "/", country.code, ".json", sep="")

writeLines(toJSON(list(items=json.list), pretty=TRUE), json.filename)

res <- rciop.publish(json.filename, metalink=TRUE, recursive=FALSE)
 
if (res$exit.code==0) { published <- res$output }

file.remove(json.filename)

}
