
v <- reactiveValues(title=NULL,df=NULL,DTb=NULL,DTe=NULL)


##############################################################
### collect data from API
##############################################################
collect_interval <- function(INT_ID) {
  print(paste0(' -> INT_ID: ', INT_ID))
  isolate(withProgress(message = 'querying station data..', value = 0.1, {
    inam <- qIntName(INT_ID)
    v$title <- inam[[1]]
    v$df <- qTemporal(INT_ID)
    v$DTb <- min(v$df$orig$Date)
    v$DTe <- max(v$df$orig$Date)
  }))
  shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")
  shinyjs::show("app-content")
}
