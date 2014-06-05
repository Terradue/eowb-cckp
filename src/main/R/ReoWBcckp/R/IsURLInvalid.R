#' A function to check if URL is invalid
#'
#' @param URL URL to be tested
#' @return boolean
#' @keywords utilities
IsURLInvalid <- function(URL) {

  return(inherits(try(url(URL)), "try-error"))
  
}
