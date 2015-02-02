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
library(XML)

# number of dates to check
checks <- 4

thirtyone <- c(1, 3, 5, 7, 8, 10, 12)
thirty <- c(4, 6, 9, 11)

dates <- c()
results <- c()
values.fromCatalogue <-c()
date.extraction <- c()
the.country <- c()
ok <- c()
json.urls <- c()


osd.url <- "http://catalogue.eowb-cckp.terradue.int/catalogue/search/UKMO_OSTIA/description"
response.type <- "application/rdf+xml"

# prepare the catalogue request
df.params <- GetOSQueryables(osd.url, response.type)

# prepare the WCS request
wcs.template <- GetWCSTemplate()
wcs.template$value[wcs.template$param == "service"] <- "WCS" 
wcs.template$value[wcs.template$param == "version"] <- "1.0.0"
wcs.template$value[wcs.template$param == "request"] <- "GetCoverage"
wcs.template$value[wcs.template$param == "coverage"] <- "analysed_sst"
wcs.template$value[wcs.template$param == "format"] <- "NetCDF3"


# build random dates between 2006-04-01 and 2014-12-18
while(checks>0){     
     year <- sample(2006:2014,1,replace=T)
     if(year == 2006){
          month <- sample(4:12,1, replace=T)          
     }else{
          month <- sample(1:12,1, replace=T)          
     }
     
     if(month == 12 & year == 2014){
          day <-sample(1:18,1, replace=T)
     }else if(month %in% thirtyone){
          day <-sample(1:31,1, replace=T)   
     }else if(month %in% thirty){
          day <-sample(1:30,1, replace=T)   
     }else{
          # february
          if(year%%4 == 0){
               day <-sample(1:29,1, replace=T)   
          }else{
               day <-sample(1:28,1, replace=T)   
          }
     }
     
     if(month %in% 1:9){
          month <- paste("0", month, sep="")
     }
     if(day %in% 1:9){
          day <- paste("0", day, sep="")
     }
     new.date <- paste(year, month, day, sep="-")
     
     if(!(new.date %in% dates)){
     	  # only differents dates
          dates <- c(dates, new.date)
          checks <- checks -1
     }
}


# execute validation test
for(j in seq(along=dates)){
    # builds urls 
     json.url.request <- paste('http://data.terradue.int:9200/eowb/cci-v1/_search?q=date:"',dates[j],'"',sep="")
     json.data <- fromJSON(readLines(json.url.request)[1])     
     
     for(i in 1:length(json.data$hits$hits)){
          
          research.date <- substring(json.data$hits$hits[[i]]$`_source`$date, 0, 10)
          country <- json.data$hits$hits[[i]]$`_source`$iso
          
          df.params$value[df.params$type == "time:start"] <- research.date
          df.params$value[df.params$type == "time:end"] <- research.date 
          
          # submit the query
          res <- Query(osd.url, response.type, df.params)
          coverages <- list(online.resource=rev(xpathSApply(xmlParse(res), "//dclite4g:DataSet/dclite4g:onlineResource/ws:WCS/@rdf:about")), 
                            start=rev(xpathSApply(xmlParse(res), "//dclite4g:DataSet/ical:dtstart", xmlValue)))
          
          wcs.template$value[wcs.template$param == "bbox"] <- GetCountryEnvelopeEEZ(country)
          
          r <- GetWCSCoverage(coverages$online.resource[1], wcs.template, by.ref=FALSE)
          r.mask <- mask(r, GetCountryEEZ(country) )
          check.value <- round(cellStats(r.mask, stat="mean"), digits = 2)
          
          # save data
          results <- c(results, check.value)
          the.country <- c(the.country, country)
          values.fromCatalogue <- c(values.fromCatalogue, json.data$hits$hits[[i]]$`_source`$value)
          date.extraction <- c(date.extraction, research.date)
          if(check.value == json.data$hits$hits[[i]]$`_source`$value)
               ok <- c(ok,"OK")
          else
               ok <- c(ok,"KO")
     }
     
}

# put data on a dataframe for visualization
df <- data.frame(Country = the.country, Date = date.extraction, ValueFromCatalogue = values.fromCatalogue, ComputedValue = results, Verified = ok )
print(df)

```

##Print Example
We choose to print data for 3 random generated dates. The 3 random generated dates are: "2013-04-26", "2006-11-03", "2011-09-23".
This is the output:

```coffee
Country Date 		ValueFromCatalogue 	ComputedValue 	Verified
PRT 	2013-04-26 	289.86 			289.86       	OK
PRT 	2006-11-03 	293.67        		293.67       	OK
ASM 	2011-09-23 	300.86        		300.86       	OK
BRB 	2011-09-23 	301.82        		301.82       	OK
BVT 	2011-09-23 	272.44        		272.44       	OK
CAN 	2011-09-23 	276.22        		276.22       	OK
CMR 	2011-09-23 	299.78        		299.78       	OK
COG 	2011-09-23 	297.78        		297.78       	OK
COL 	2011-09-23 	301.24        		301.24       	OK
DNK 	2011-09-23 	287.53        		287.53       	OK
DOM 	2011-09-23 	302.25        		302.25       	OK
FRA 	2011-09-23 	291.84        		291.84       	OK
```