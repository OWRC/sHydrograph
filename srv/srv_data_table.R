

########################################################
#### data table
########################################################
df.filtered <- function(){
  dat.out <- v$df$orig[v$df$orig$Date >= input$tabRng[1] & v$df$orig$Date <= input$tabRng[2],] # %>%
    # spread(key=RDTC,value=colnames(v$df$orig)[4:ncol(v$df$orig)])
  # row.has.na <- apply(dat.out[-1], 1, function(x){all(is.na(x))})
  # return(dat.out[!row.has.na,])
  # dat.out$Date <- as.Date(dat.out$Date)
  # dat.out$Val <- as.numeric(dat.out$Val)
  dat.out$RDNC <- mapvalues(dat.out$RDNC,names(xr.RNDC),xr.NLong[xr.RNDC])
  dat.out$unit <- mapvalues(dat.out$unit,names(xr.unit),xr.unit)
  dat.out <- dat.out[,c(1,2,5,4)] %>% #excluding RDTC
    rename(
      value = Val,
      type = RDNC
    )
  return(dat.out)
}

observe(updateDateRangeInput(session, "tabRng", start = min(v$df$orig$Date), end = max(v$df$orig$Date), min = min(v$df$orig$Date), max = max(v$df$orig$Date)))

output$tabts <- renderDataTable({
    if (!is.null(v$df$orig)) df.filtered()
  }, 
  options = list(scrollY='100%', scrollX=TRUE,
            lengthMenu = c(30, 100, 365, 3652),
            pageLength = 100,
            searching=FALSE)
)

output$tabCsv <- downloadHandler(
  filename <- function() { paste0(path_sanitize(v$title), '.csv') },
  content <- function(file) {
    if (!is.null(v$df$orig)){ write.csv(df.filtered(), file, row.names = FALSE) }
  }
)