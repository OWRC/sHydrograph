

v <- reactiveValues(title=NULL,df=NULL,scrn=NULL,DTb=NULL,DTe=NULL)


##############################################################
### collect data from API
##############################################################
collect_interval <- function(INT_ID,vTemporal=2) {
  # print(paste0(' -> INT_ID: ', INT_ID))
  isolate(withProgress(message = 'querying station data..', value = 0.1, {
    setProgress(0.2,"location name..")
    inam <- qIntName(INT_ID)
    setProgress(0.3,"location info..")
    v$scrn <- qIntScreen(INT_ID)
    v$title <- inam[[1]]
    setProgress(0.4,"location coordinates..")
    icrd <- qIntCoord(INT_ID)
    setProgress(0.5,"querying observations..")
    v$df <- qTemporal(INT_ID,vTemporal)
    v$DTb <- min(v$df$orig$Date)
    v$DTe <- max(v$df$orig$Date)
    
    # v$lat <- icrd$LAT
    # v$lng <- icrd$LONG
    setProgress(0.6,"interpolating to location..")
    dfinterp <- qInterp(icrd$LONG,icrd$LAT)
    v$df$plt <- v$df$plt %>% inner_join(dfinterp %>% dplyr::select(-c("Tx","Tn","Sf","Pa")), by="Date")
  }))
  shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")
  shinyjs::show("app-content")
}


print("collect_interval loaded")