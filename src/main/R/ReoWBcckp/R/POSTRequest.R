
POSTRequest <- function(access.point, content.type, content) {
  
  myheader=c(Connection='close', 'Content-Type' = content.type)
  
  reader = basicTextGatherer()
  header = basicTextGatherer()
  # removed userpwd = geoserver.authn,
  data <- curlPerform(url = access.point,
    postfields = content,
    httpheader = myheader,
    verbose = FALSE,
    ssl.verifypeer = FALSE,
    writefunction = reader$update,
    headerfunction = header$update
  )
  
  h = parseHTTPHeader( header$value() )
  
  return(list(status=capture.output(cat(h["status"])),
              message=capture.output(cat(h["statusMessage"])),
              location=capture.output(cat(h["Location"]))
              )
          )
  
}
