
v <- reactiveValues(title=NULL,df=NULL,scrn=NULL,DTb=NULL,DTe=NULL)


##############################################################
### collect data from API
##############################################################
collect_interval <- function(INT_ID,vTemporal=2) {
  print(paste0(' -> INT_ID: ', INT_ID))
  isolate(withProgress(message = 'querying station data..', value = 0.1, {
    inam <- qIntName(INT_ID)
    v$scrn <- qIntScreen(INT_ID)
    v$title <- inam[[1]]
    v$df <- qTemporal(INT_ID,vTemporal)
    v$DTb <- min(v$df$orig$Date)
    v$DTe <- max(v$df$orig$Date)
  }))
  shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")
  shinyjs::show("app-content")
}


print("collect_interval loaded")