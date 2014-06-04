\name{GetWCSAccessPoint}
\alias{GetWCSAccessPoint}
\title{A function to return the OGC Web Coverage Request access point}
\usage{
GetWCSAccessPoint(WCS.url)
}
\arguments{
  \item{WCS.url}{URL of the WCS service}
}
\value{
a data frame with three columns: param, type, value (NAs)
containing the OGC WCS parameters
}
\description{
A function to return the OGC Web Coverage Request access
point
}
\examples{
\dontrun{
GetWCSTemplate()
}
\keyword{utilities}

