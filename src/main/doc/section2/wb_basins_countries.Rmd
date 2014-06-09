# World Bank basins and countries

The file maps.rda contains the World Banck basins and countries ISO 3166-1 alpha-3 codes.

## Data source

The data was taken from the [R Open Science](http://ropensci.org/) [rWBClimate](https://github.com/ropensci/rWBclimate) package: a Programmatic interface to the World Bank climate data used in the World Bank climate knowledge portal.

## Data description

The maps Rda file contains the World Bank basins and countries grouped by continent:

```R
 [1] "Africa_basin"   "Africa_country" "Asia_basin"     "Asia_country"   "Eur_basin"      "Eur_country"    "NoAm_basin"     "NoAm_country"   "Oceana_basin"  
[10] "Oceana_country" "SoAm_basin"     "SoAm_country" 
```

The African_basin integer vector contains the World Bank basin codes:

```R
> class(e$Africa_basin)
[1] "integer"
> head(e$Africa_basin)
[1] 379 380 381 382 383 384
```

The Africa_country character vector contains the countries' [ISO 3166-1 alpha-3](http://en.wikipedia.org/wiki/ISO_3166-1_alpha-3) code:

```R
> class(e$Africa_country)
[1] "character"
> head(e$Africa_country)
[1] "AGO" "BFA" "BDI" "BEN" "BWA" "COD"
```

