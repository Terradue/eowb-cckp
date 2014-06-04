\name{GetCountryEnvelope}
\alias{GetCountryEnvelope}
\title{provides the country EEZ envelope}
\usage{
GetCountryEnvelope(ISO.Code)
}
\arguments{
  \item{iso}{The 3 letter country code based on ISO3
  Country abbreviations
  (http://unstats.un.org/unsd/methods/m49/m49alpha.htm)}
}
\value{
character representation of the geographical envelope as
xmin, ymin, xmax, ymax
}
\description{
Checks if the country code entered is valid and provides
the geographical envelope of the country exclusive economic
zone (EEZ)
}
\examples{
\dontrun{
GetCountryEnvelope("PRT")
}
}

