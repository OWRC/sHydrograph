

iintrpd <- 'http://fews.oakridgeswater.ca:8080/dymetp/%f/%f'
iintrph <- 'http://fews.oakridgeswater.ca:8080/h6metp/%f/%f'

getCDS <- function(url){
  out <- tryCatch(
    {
      fromJSON(url)
    },
    error=function(cond) {
      return(NULL)
    },
    warning=function(cond) {
      return(NULL)
    },
    finally={}
  )
  out[out==-999] <- NA
  return(out)
}


qInterp <- function(longitude,latitude){
  if (is.null(longitude) | is.null(longitude)) return(NULL)
  url <- sprintf(iintrpd,latitude,longitude)
  print(url)
  dfd <- getCDS(url)
  if (length(dfd)==1 && dfd=="NA") return(NULL)
  dfd$Date <- as.Date(dfd$Date)

  url <- sprintf(iintrph,latitude,longitude)
  print(url) 
  dfh <- getCDS(url)
  dfh$Date <- as.POSIXct(dfh$Date, format="%Y-%m-%dT%H:%M:%S")
  dfh <- dfh %>%
    dplyr::select(c(Date,Pa)) %>%
    mutate(Date = as.Date(floor_date(Date, "day"))) %>%
    group_by(Date) %>%
    dplyr::summarize(Pa = mean(Pa))
  
  out <- left_join(dfd, dfh, by="Date")
  return(out)  
}

latitude<-43.514463
longitude<- -79.944608
