\name{IsISOCodeInvalid}
\alias{IsISOCodeInvalid}
\title{check country codes}
\usage{
IsISOCodeInvalid(ISO.code)
}
\arguments{
  \item{iso}{The 3 letter country code based on ISO3
  Country abbreviations
  (http://unstats.un.org/unsd/methods/m49/m49alpha.htm)}
}
\value{
TRUE if a valid code, otherwise an error is returned
}
\description{
Checks if the country code entered is a valid country code
that data exists for
}
\examples{
\dontrun{
IsISOCodeInvalid("PRT")
}
}

