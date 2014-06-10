#' A function to return the OGC Web Coverage Request parameter template as a data frame
#'
#' @return a data frame with three columns: param, type, value (NAs) containing the OGC WCS parameters
#' @keywords utilities
#' @examples \dontrun{
#' GetWCSTemplate()
#' }
#'
#' @export
#' @import httr stringr 

GetWCSTemplate <- function() {

  l <- parse_url(wcs.url.template)$query
  df.full.template <- do.call(rbind.data.frame,l)

  # get a column with the named list name
  df.full.template$param <- rownames(df.full.template)
  
  # cleanup the rownames
  rownames(df.full.template) <- NULL
  
  # there are invalid templates out there!
  # e.g. ?}&loc={geo:name&}&startdate={time:start?}&
  df.full.template <- df.full.template[!(is.na(df.full.template[,1]) | df.full.template[,1]==""), ]

  # remove the {, }, ? from the type
  df.template <- as.data.frame(sapply(df.full.template, function(x) {
    x <- str_replace_all(x, "([\\{\\}\\?])", "")
  }))

  # add a third column with NAs, this column can be filled with query values
  df.template[, 3] <- NA

  # set the column names to type/value, it will be very useful for the Query function params argument
  colnames(df.template) <- c("type", "param", "value")

  return(df.template)

}
