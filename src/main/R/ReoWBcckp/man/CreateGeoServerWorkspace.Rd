\name{CreateGeoServerWorkspace}
\alias{CreateGeoServerWorkspace}
\title{creates a workspace in a GeoServer instance}
\usage{
CreateGeoServerWorkspace(geoserver.access.point, workspace)
}
\arguments{
  \item{geoserver.access.point}{GeoServer REST API access
  point}

  \item{workspace}{workspace name to be created}
}
\value{
boolean TRUE when the workspace was created, FALSE
otherwise (check message returned)
}
\description{
Creates a GeoServer workspace in a GeoServer instance.
}
\examples{
\dontrun{
CreateGeoServerWorkspace("PRT")
}
}

