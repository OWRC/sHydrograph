

iintrp <- 'http://golang.oakridgeswater.ca:8080/pmet/%f/%f'


qInterp <- function(longitude,latitude){
  out <- tryCatch(
    {
      print(sprintf(iintrp,latitude,longitude))
      fromJSON(sprintf(iintrp,latitude,longitude))
    },
    error=function(cond) {
      return(NULL)
    },
    warning=function(cond) {
      return(NULL)
    },
    finally={}
  )
  if (out=="NA") return(NULL)
  out[out==-999] <- NA
  out$Date <- as.Date(out$Date)
  # out$Date <- as.POSIXct(out$Date, tz="America/New_York")
  return(out)  
}