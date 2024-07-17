

iintrp <- 'http://fews.oakridgeswater.ca:8080/dymetp/%f/%f'


qInterp <- function(longitude,latitude){
  if (is.null(longitude) | is.null(longitude)) return(NULL)
  url <- sprintf(iintrp,latitude,longitude)
  print(url)
  out <- tryCatch(
    {
      fromJSON(print(url))
    },
    error=function(cond) {
      return(NULL)
    },
    warning=function(cond) {
      return(NULL)
    },
    finally={}
  )
  if (length(out)==1 && out=="NA") return(NULL)
  out[out==-999] <- NA
  out$Date <- as.Date(out$Date)
  # out$Date <- as.POSIXct(out$Date, tz="America/New_York")
  return(out)  
}