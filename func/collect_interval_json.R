
v <- reactiveValues(title=NULL,df=NULL,scrn=NULL,DTb=NULL,DTe=NULL)





##############################################################
### collect data from file
##############################################################
collect_interval <- function(jsonfp) {
  INT_ID <- strtoi(sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(jsonfp))) # filename no extension
  print(paste0(' -> INT_ID: ', INT_ID))
  isolate(withProgress(message = 'querying station data..', value = 0.1, {
    inam <- qIntName(INT_ID)
    v$scrn <- qIntScreen(INT_ID)
    v$title <- inam[[1]]
    v$df <- qTemporal_json(jsonfp)
    v$DTb <- min(v$df$orig$Date)
    v$DTe <- max(v$df$orig$Date)
  }))
  shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")
  shinyjs::show("app-content")
}


print("collect_interval loaded")