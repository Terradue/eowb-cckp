#cci-sst 

This application computes, for each country the mean of the sea temperature for a given day.

##Application

You can check the precision of the result using the following procedure. You need to store in a folder the json file computed by the cci-sst application 

```coffee
library(devtools)
library(raster)
library(ReoWBcckp)
library(rgeos)
library(rjson)
library(rasterVis)       
library(rOpenSearch)

# The search.date value  in the application.xml file
search.date <- "2013-01-01"

# A list of countries you want to check
countries <-c("PRT", "GBR", "ITA", "FRA", "AUS", "USA", "ARG", "SPA", "MAR")

# The folder where the json files are stored
jsonPath <- "/Users/myUserFolder/sst" 

# the following information must be the same present in the application.xml
osd.url <- "http://catalogue.eowb-cckp.terradue.int/catalogue/search/UKMO_OSTIA/description"
start.date <- search.date 
end.date <- search.date 
response.type <- "application/rdf+xml"
count <- 300

# prepare the catalogue request
df.params <- GetOSQueryables(osd.url, response.type)

df.params$value[df.params$type == "count"] <- count 
df.params$value[df.params$type == "time:start"] <- start.date
df.params$value[df.params$type == "time:end"] <- end.date 
# submit the query
res <- Query(osd.url, response.type, df.params)
coverages <- list(online.resource=rev(xpathSApply(xmlParse(res), "//dclite4g:DataSet/dclite4g:onlineResource/ws:WCS/@rdf:about")), 
                  start=rev(xpathSApply(xmlParse(res), "//dclite4g:DataSet/ical:dtstart", xmlValue)))


# prepare the WCS request
wcs.template <- GetWCSTemplate()
wcs.template$value[wcs.template$param == "service"] <- "WCS" 
wcs.template$value[wcs.template$param == "version"] <- "1.0.0"
wcs.template$value[wcs.template$param == "request"] <- "GetCoverage"
wcs.template$value[wcs.template$param == "coverage"] <- "analysed_sst"
wcs.template$value[wcs.template$param == "format"] <- "NetCDF3"


jfiles <- list.files(path = jsonPath)
results<-c()
for(i in seq(along=countries)) { 
     
     wcs.template$value[wcs.template$param == "bbox"] <- GetCountryEnvelopeEEZ(countries[i])
     r <- GetWCSCoverage(coverages$online.resource[1], wcs.template, by.ref=FALSE)
     r.mask <- mask(r, GetCountryEEZ(countries[i]) )
     check.value <- cellStats(r.mask, stat="mean")
     results<-c(results, check.value)

}

# Print the results on screen
header<-paste("Country", "Computed Value", "Read from File", sep='\t\t')
cat(header, "\n")
for(i in seq(along=countries)) { 
     jsonFile <- fromJSON(file = paste(jsonPath,"/",countries[i],".json",sep=""))
     row.computed<-paste(countries[i], results[i], jsonFile$items[[1]]$value, sep="\t\t")
     cat(row.computed, "\n")
}

```

##Print Example
We choose to print the data for the following countries: Portugal (PRT), United Kingdom (GBR), Italy (ITA), France (FRA), Australia (AUS), United State of America (USA), Argentina (ARG).
This is the output:

```coffee
Country   Computed Value    Read from File 
GBR       281.923267924124  281.92 
ITA       288.515458184502  288.52 
FRA       285.83173175      285.83 
AUS       295.405436349265  295.41 
USA       282.967733367273  282.97 
ARG       284.897846103793  284.9 
```