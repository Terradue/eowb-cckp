\name{CreateGeoServerCoverageStore}
\alias{CreateGeoServerCoverageStore}
\title{creates a coverage store in a GeoServer instance}
\usage{
CreateGeoServerCoverageStore(geoserver.access.point, workspace, name,
  enabled = TRUE, type = "GeoTIFF", url)
}
\arguments{
  \item{geoserver.access.point}{GeoServer REST API access
  point}

  \item{workspace}{character vector of the GeoServer
  workspace}

  \item{name}{character vector with the coverage store
  name}

  \item{enabled}{boolean to define if the coverage store is
  enabled, defaults to TRUE}

  \item{type}{character vector with the coverage type,
  defaults to GeoTIFF}

  \item{url}{URL of a coverage previously uploaded}
}
\value{
boolean TRUE when the coverage store was created, FALSE
otherwise (check message returned)
}
\description{
Creates a GeoServer coverage store in a GeoServer instance.
This function requires the URL of a coverage previously
uploaded to GeoServer
}
\examples{
\dontrun{
CreateGeoServerCoverageStore("localhost:8080/geoserver", "acme", "cs", "file:data/folder/coverage")
}
}

