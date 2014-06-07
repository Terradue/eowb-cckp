\name{GetWCSCoverage}
\alias{GetWCSCoverage}
\title{A function to define an OGC Web Coverage Service GetCoverage request URL or return the raster associated to the request.}
\usage{
GetWCSCoverage(WCS.access.point, df.params, by.ref = TRUE)
}
\arguments{
  \item{WCS.access.point}{URL of the OGC Web Coverage
  Service access point. The URL can be passed as a
  GetCoverage request, all parameters will be stripped and
  replaced with the contents of df.params}

  \item{df.params}{data frame with the Web Coverage Service
  parameters (column param) and values (column value)}

  \item{by.ref}{boolean to define the retruned object: if
  TRUE (default) a character URL to the Web Coverage
  Service is returned if FALSE, a raster object is
  returned}
}
\value{
WCS request character vector or a raster depending on the
by.ref parameter value
}
\description{
A function to define an OGC Web Coverage Service
GetCoverage request URL or return the raster associated to
the request.
}
\examples{
\dontrun{
wcs.template <- GetWCSTemplate()

wcs.template$value[wcs.template$param == "service"] <- "WCS"
wcs.template$value[wcs.template$param == "version"] <- "1.0.0"
wcs.template$value[wcs.template$param == "request"] <- "GetCoverage"
wcs.template$value[wcs.template$param == "coverage"] <- "sla"
wcs.template$value[wcs.template$param == "format"] <- "NetCDF3"
wcs.template$value[wcs.template$param == "bbox"] <- GetCountryEnvelope("USA")

r <- GetWCSCoverage("http://catalogue.eowb-cckp.terradue.int/thredds/wcs/SeaLevel-ECV/V1.1_20131220/ESACCI-SEALEVEL-L4-MSLA-MERGED-20100815000000-fv01.nc?service=WCS&version=1.0.0&request=GetCoverage&coverage=sla&format=NetCDF3&bbox=0.1,-90,360,90",
  wcs.template, by.ref=FALSE)
}
\keyword{utilities}

