\name{GetCountryEEZ}
\alias{GetCountryEEZ}
\title{provides the country EEZ spatial polygon(s)}
\usage{
GetCountryEEZ(ISO.Code)
}
\arguments{
  \item{ISO.code}{The 3 letter country code based on ISO3
  Country abbreviations
  (http://unstats.un.org/unsd/methods/m49/m49alpha.htm)}
}
\value{
SpatialPolygons
}
\description{
Checks if the country code entered is valid and provides
the country exclusive economic zone (EEZ) spatial
polygon(s)
}
\examples{
\dontrun{
GetCountryEEZ("PRT")
}
}

