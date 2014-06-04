\name{IsISOCodeInvalid}
\alias{IsISOCodeInvalid}
\title{check country code validity}
\usage{
IsISOCodeInvalid(ISO.code)
}
\arguments{
  \item{iso}{The 3 letter country code based on ISO3
  Country abbreviations
  (http://unstats.un.org/unsd/methods/m49/m49alpha.htm)}
}
\value{
TRUE if an invalid code and an error message is returned,
FALSE if the code is valid
}
\description{
Checks if the country code entered is invalid
}
\examples{
\dontrun{
IsISOCodeInvalid("PRT")
}
}

