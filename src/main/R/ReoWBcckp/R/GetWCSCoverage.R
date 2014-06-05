#' A function to return ,
#' a response type and a data frame with queryables' type and values (NAs are be removed)
#'
#' @param opensearch.description URL pointing to the OpenSearch decription document
#' @param response.type OpenSearch response type
#' @return the OpenSearch response
#' @keywords utilities
#' @examples \dontrun{
#' osd.url <- "http://eo-virtual-archive4.esa.int/search/ASA_IM__0P/description"
#' df.params <- GetOSQueryables(osd.url, "application/rdf+xml")
#' df.params$value[df.params$type == "count"] <- 30
#' df.params$value[df.params$type == "time:start"] <- "2010-01-10"
#' df.params$value[df.params$type == "time:end"] <- "2010-01-31"
#' res <- Query(osd.url, "application/rdf+xml", df.params)
#' @export

GetWCSCoverage <- function(WCS.access.point, df.params, by.ref=TRUE) {

  if(IsURLInvalid(WCS.access.point)) { stop("Invalid WCS access point") }
 
  # remove the NAs if any and keep columns type and value
  df.params <- subset(df.params[complete.cases(df.params),], select=c("type", "value"))

  # avoid factors
  # TODO: is this really needed after all?
  # df.params <- CastCharacter(df.params)

  # get the queryables template, drop the value column
  # since the value column will come from the df.params when doing the merge
  df.template <- subset(GetWCSTemplate(), select = c("type", "param"))

  # merge the template and the parameters
  df.query <- subset(merge(df.template, df.params, by.y=c("type")), select = c("param", "value"))

  # create a named list
  params <- as.list(df.query$value)
  names(params) <- df.query$param
  
  url <- parse_url(WCS.access.point)

  url$query <- params
  
  if (by.ref) return(getURL(build_url(url)))
  
  tmp.file <- tempfile()
  download.file(getURL(build_url(url)), tmp.file)
  
  return(raster(tmp.file))

}
