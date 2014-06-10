# World Exclusive Economic Zone (EEZ)

## What is an EEZ

An exclusive economic zone (EEZ) is a seazone prescribed by the United Nations Convention on the Law of the Sea over which state has special rights over the exploration and use of marine resources, including energy production from water and wind.

It stretches from the baseline out to 200 nautical miles from its coast. In colloquial usage, the term may include the continental shelf. The term does not include either the territorial sea or the continental shelf beyond the 200 n.m. limit. 

The difference between the territorial sea and the exclusive economic zone is that the first confers full sovereignty over the waters, whereas the second is merely a "sovereign right" which refers to the coastal state's rights below the surface of the sea. 

Source: [Wikipedia](http://en.wikipedia.org/wiki/Exclusive_economic_zone)

## Data source

The data was downloaded from the [Marine Regions](http://www.marineregions.org/) website.

Marine Regions is an integration of the VLIMAR Gazetteer and the VLIZ Maritime Boundaries Geodatabase. The VLIMAR Gazetteer is a database with geographic, mainly marine names such as seas, sandbanks, seamounts, ridges, bays or even standard sampling stations used in marine research. The geographic cover of the VLIMAR gazetteer is global but initially focused on the Belgian Continental Shelf and the Scheldt Estuary and the Southern Bight of the North Sea. Gradually more regional and global geographic information was added to VLIMAR and combining this information with the Maritime Boundaries database, representing the Exclusive Economic Zone (EEZ) of the world, led to the creation of marineregions.org. 

## Data description

The Marine Regions provides several versions of the EEZ geodatabase version and for each two resolutions.

The EEZ geodatabase used in this package is:

World EEZ v8 of 2014-02-28, low resolution.

The download required filing a form with the Name, Organisation, Email, Country and purpose of download.

## Data format

The data is provided as a Shapefile.

## Data pre-processing

The data was imported into R and saved as an R object to a file:

```R
library(rgdal)
world_EEZ_V8_2014 <- readOGR("~/Downloads/World_EEZ_v8_20140228_LR/", "World_EEZ_v8_2014")
save(world_EEZ_V8_2014, file="~/Downloads/world_EEZ_V8_2014.rda")
```

The file world_EEZ_V8_2014.rda was then pushed to https://github.com/Terradue/eowb-cckp/blob/master/src/main/R/ReoWBcckp/data/world_EEZ_V8_2014.rda

