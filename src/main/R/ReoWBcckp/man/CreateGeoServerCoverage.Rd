\name{CreateGeoServerCoverage}
\alias{CreateGeoServerCoverage}
\title{creates a coverage in a GeoServer instance}
\usage{
CreateGeoServerCoverage(geoserver.access.point, workspace, coveragestore, name,
  title, abstract = "This is the abstract", native.crs = "EPSG:4326",
  srs = "EPSG:4326", enabled = TRUE, native.bbox, latlon.bbox)
}
\arguments{
  \item{geoserver.access.point}{GeoServer REST API access
  point}

  \item{workspace}{character vector of the GeoServer
  workspace (must exist)}

  \item{coveragestore}{character vector of the GeoServer
  coveragestore (must exist)}

  \item{name}{character vector with the coverage store
  name}

  \item{title}{character vector with the coverage store
  title}

  \item{abstract}{character vector with the coverage store
  abstract}

  \item{enabled}{boolean to define if the coverage store is
  enabled, defaults to TRUE}

  \item{native.crs}{native CRS of the coverage (defaults to
  EPSG:4326)}

  \item{latlon.crs}{Lat/Lon CRS of the coverage (defaults
  to EPSG:4326)}
}
\value{
boolean TRUE when the coverage was created, FALSE otherwise
(check the message returned)
}
\description{
Creates a GeoServer coverage store in a GeoServer instance.
}
\examples{
\dontrun{
CreateGeoServerCoverage("localhost:8080/geoserver", "acme", "cs", "mycoverage", "this is a title")
}
}

