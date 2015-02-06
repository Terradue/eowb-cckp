#!/usr/bin/Rscript --vanilla --slave --quiet

library("rciop")
library("raster")
library("rOpenSearch")
library("rgeos")
library("ReoWBcckp", lib.loc="/application/share/R/library/")

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
#routing variable 
var.series <- "ghrsst-ukmo"   
# base polygon (global world bbox)
basePolygon <- readWKT("POLYGON((-180 -90, -180 90, 0 90, 0 -90,-180 -90))")

osd.url <- rciop.getparam("catalogue")
response.type <- rciop.getparam("responsetype")
data.api <- rciop.getparam("dataapi")

# prepare the catalogue request
df.params <- GetOSQueryables(osd.url, response.type)

df.params$value[df.params$type == "count"] <- 20

# read the stdin into a file
f <- file("stdin")
open(f)

# error log file
t <- unlist(strsplit(TMPDIR,"/"))
base.name <- t[length(t) - 2]
log.file <-paste(TMPDIR, paste(base.name, "log", sep="."), sep="/")
print(paste("Log file is", log.file, sep=" "))
sink(log.file, append=TRUE)
cat(paste("**********", base.name, "**********", sep=" "))
cat("\n")
cat(paste("Proces started at", as.character(Sys.time()), sep=" "))
cat("\n")
sink()

tryCatch({
  while(length(input <- readLines(f, n=1)) > 0) {
    tokens<-unlist(strsplit(input, ";"))
    country.code <- tokens[1]
    df.params$value[df.params$type == "time:start"] <- substring(tokens[2], 0, 10)
    df.params$value[df.params$type == "time:end"] <- substring(tokens[2], 0, 10)

    # submit the query
    retry <- 0
    do.next.country <- FALSE
    while(!do.next.country){
      
      tryCatch({
        res <- Query(osd.url, response.type, df.params)
        # create a named list with the WCS online resources and associated start date 
        coverages <- list(online.resource=rev(xpathSApply(xmlParse(res), "//dclite4g:DataSet/dclite4g:onlineResource/ws:WCS/@rdf:about")), 
                          start=rev(xpathSApply(xmlParse(res), "//dclite4g:DataSet/ical:dtstart", xmlValue)))
        break;
      },error=function(cond)
      {
        message.error <- paste("Error while submitting query to", osd.url, "for", country.code, "[", substring(tokens[2], 0, 10), "]", "New try in", wait.time, "seconds", sep=" ")
        print(message.error)
        rciop.log("DEBUG", message.error)
        Sys.sleep(wait.time)
      })  
      
      # check exit from loop 
      if(retry > retries){
        message.fail <- paste("FAILED. Submitting query to", osd.url, "for", country.code, "[", substring(tokens[2], 0, 10), sep=" ")
        print(message.fail)
        rciop.log("DEBUG", message.fail)
        do.next.country <- TRUE
        log.datetime <- paste0("[", as.character(Sys.time()), "]")
        sink(log.file, append=TRUE)
        cat(paste(log.datetime, "FAIL", country.code, substring(tokens[2], 0, 10), "error while executing query to osd", sep=" -- "))
        cat("\n")
        sink()
        break;
      }

      retry <- retry + 1
    }
    if(do.next.country)
      next;
    
    # country code is valid
    if(IsISOCodeInvalid(country.code)){
         rciop.log("DEBUG", paste("Country ISO code:", country.code, "is not valid",sep=" "))
         log.datetime <- paste0("[", as.character(Sys.time()), "]")
         sink(log.file, append=TRUE)
         cat(paste(log.datetime, "WARNING", country.code, substring(tokens[2], 0, 10), paste("Country ISO code:", country.code, "is not valid",sep=" "), sep=" -- "))
         cat("\n")
         sink()
         next;
    }

    rciop.log("DEBUG", paste("Country ISO code:", country.code, sep=" "))

    # complete the WCS request with the country envelope (EEZ). 
    wcs.template$value[wcs.template$param == "bbox"] <- GetCountryEnvelopeEEZ(country.code)
    
    # the country.code bounding box is valid
    if(is.na(wcs.template$value[wcs.template$param == "bbox"])){
         rciop.log("DEBUG", paste("Country ISO code:", country.code, "wrong or no bbox associated to the Country ISO code",sep=" "))
         log.datetime <- paste0("[", as.character(Sys.time()), "]")
         sink(log.file, append=TRUE)
         cat(paste(log.datetime, "WARNING", country.code, substring(tokens[2], 0, 10), "wrong or no bbox associated to the Country ISO code", sep=" -- "))
         cat("\n")
         sink()
         next;
    }
            
    coordinates <- unlist(strsplit(wcs.template$value[wcs.template$param == "bbox"], ","))
    country.polygon <- paste("POLYGON((",   coordinates[1], coordinates[2], ",", coordinates[1], coordinates[4], ",",
                                            coordinates[3], coordinates[4], ",", coordinates[3], coordinates[2], ",",
                                            coordinates[1], coordinates[2], "))" )
     
    json.list <- c()
    
    rciop.log("INFO", paste("Processing",country.code, "in  date:",  substring(tokens[2],0,10), sep=" "))
      
    # get the coverage 
    retry <- 0
    do.next.country <- FALSE
    while(!do.next.country){
      
      tryCatch({
        r <-GetWCSCoverage(coverages$online.resource[1], wcs.template, by.ref=FALSE)
        break;
      },error=function(cond)
      {
        rciop.log("DEBUG", paste("Error:", cond, sep=" "))
        rciop.log("DEBUG", paste("New try in", wait.time, "seconds", sep=" "))
        Sys.sleep(wait.time)
      })  
      
      # check exit from loop 
      if(retry > retries){
        print(paste("[", substring(tokens[2], 0, 10), "]", country.code, "element not computed", sep=" "))
        rciop.log("DEBUG", paste("[", substring(tokens[2], 0, 10), "]", country.code, "element not computed", sep=" "))
        do.next.country <- TRUE
        log.datetime <- paste0("[", as.character(Sys.time()), "]")
        sink(log.file, append=TRUE)
        cat(paste(log.datetime, "FAIL", country.code, substring(tokens[2], 0, 10), "error while getting coverage" ,sep=" -- "))
        cat("\n")
        sink()
        break;
      }

      retry <- retry + 1
    }
    if(do.next.country)
      next;
      
    tryCatch({
        # clip with the country confines
      r.mask <- mask(r, GetCountryEEZ(country.code) )
    },error=function(cond)
    {
      print(paste("[", substring(tokens[2], 0, 10), "]", "Error while computing ", country.code, ". No data computed.", sep=" "))
      rciop.log("Debug", paste("Error:", cond, sep=" "))
      log.datetime <- paste0("[", as.character(Sys.time()), "]")
      sink(log.file, append=TRUE)
      cat(paste(log.datetime, "FAIL", country.code, substring(tokens[2], 0, 10), "error while masking the country", sep=" -- "))
      cat("\n")
      sink()
      next;
    })  

    json.list <- c(json.list, list(list(iso=country.code, var=var.series, time=substring(tokens[2],0,10), value=cellStats(r.mask, stat="mean"))))
    
    # delete the WCS downloaded raster (the other raster are in memory)
    file.remove(r@file@name)  
    
    # removing object from memory
    rm(r)
    rm(r.mask)
    
    json.filename <- paste(TMPDIR, "/", country.code, "_", substring(tokens[2], 0, 10), ".json", sep="")

    writeLines(toJSON(list(items=json.list), pretty=TRUE), json.filename)

    res <- rciop.publish(json.filename, metalink=TRUE, recursive=FALSE)
    
    print(paste("[", substring(tokens[2], 0, 10), "]", country.code, "element computed", sep=" "))

    if (res$exit.code==0) { published <- res$output }
    file.remove(json.filename)

    # post json to datastore
    response <- POSTRequest(access.point=data.api, content=toJSON(list(items=json.list)), content.type="application/json")
    if(response$status!=200)
    {
      error.message <- paste("Error while sending", json.filename, "to the catalogue", sep=" ")
      print(error.message)
      rciop.log("ERROR", error.message)  
      rciop.log("ERROR", paste("Message:", response$message, sep=" "))  
      log.datetime <- paste0("[", as.character(Sys.time()), "]")
      jfn <- paste("error while sending json to the server. File ", country.code, "_", substring(tokens[2], 0, 10), ".json ", "created", sep="")
      sink(log.file, append=TRUE)
      cat(paste(log.datetime, "ERROR", country.code, substring(tokens[2], 0, 10), jfn, sep=" -- "))
      cat("\n")
      sink()
    }else {
      rciop.log("DEBUG", paste(json.filename, "correctly sent to the catalogue", sep=" "))  
      log.datetime <- paste0("[", as.character(Sys.time()), "]")
      jfn <- paste("Json file ", country.code, "_", substring(tokens[2], 0, 10), ".json ", "succesfully sent", sep="")
      sink(log.file, append=TRUE)
      cat(paste(log.datetime, "SUCCESS", country.code, substring(tokens[2], 0, 10), jfn, sep=" -- "))
      cat("\n")
      sink()
    }
  }
},error=function(cond)
{   
    message.error <- paste("Unexpected error", country.code, substring(tokens[2], 0, 10), sep=" ")
    print(message.error)
    rciop.log("DEBUG", message.error)
    rciop.log("ERROR", message.error)
    log.datetime <- paste0("[", as.character(Sys.time()), "]")
    sink(log.file, append=TRUE)
    cat(paste(log.datetime, "ERROR", country.code, substring(tokens[2], 0, 10), "Unexpected error", sep=" -- "))
    cat("\n")
    sink()
},finally={
    sink(log.file, append=TRUE)
    cat(paste("Proces stoped at", as.character(Sys.time()), sep=" "))
    cat("\n")
    sink()
})  
res <- rciop.publish(log.file, metalink=TRUE, recursive=FALSE)
  if (res$exit.code==0) { published <- res$output }
    file.remove(log.file)

