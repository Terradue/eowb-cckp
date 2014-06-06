#!/usr/bin/Rscript --vanilla --slave --quiet
 
library("rciop")
library("rgeos")

rciop.log("DEBUG", paste("track", os.track, "start", os.start, "stop", os.stop, sep=" "))

# read the stdin into a file
f <- file("stdin")
open(f)

while(length(input <- readLines(f, n=1)) > 0) {
  
  rciop.log("DEBUG", paste("Country ISO code:", input, sep=" "))
  
}
  
