


qTemporal_json <- function(fp) {
  print(fp)
  df <- tryCatch(
    {
      fromJSON(fp)
    },
    error=function(cond) {
      showNotification(paste0("Error: invalid interval ID"))
      return(NULL)
    },
    warning=function(cond) {
      showNotification(paste0("Error: invalid interval ID"))
      return(NULL)
    },
    finally={}
  )
  if (is.null(df)) {
    showNotification(paste0("Error: invalid URL"))
    return(NULL)  
  }
  return(qTemporal_clean(df))
}