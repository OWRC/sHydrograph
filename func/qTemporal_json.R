


qTemporal_json <- function(fp) {
  print(fp)
  df <- tryCatch(
    {
      fromJSON(fp)
    },
    error=function(cond) {
      print(cond)
      showNotification("Error: invalid URL/interval ID")
      return(NULL)
    },
    warning=function(cond) {
      print(cond)
      # showNotification("Error: invalid interval ID")
      return(NULL)
    },
    finally={}
  )
  if (is.null(df)) {
    print("qTemporal_json returned null df (possibly too big)")
    # showNotification(paste0("query returned null (possibly too big)"))
    return(NULL)  
  }
  return(qTemporal_clean(df))
}