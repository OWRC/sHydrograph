

########################################################
#### data table
########################################################
df.filtered <- reactive({
  req(s <- input$tabRad )
  if (s == 1) {
    df <- v$df$orig[v$df$orig$Date >= input$tabRng[1] & v$df$orig$Date <= input$tabRng[2],]
    df$RDNC <- mapvalues(df$RDNC,names(xr.RNDC),xr.NLong[xr.RNDC])
    df$unit <- mapvalues(df$unit,names(xr.unit),xr.unit)
    df <- df[,c(1,2,5,4)] %>% #excluding RDTC
      rename(
        value = Val,
        type = RDNC
      )
    return(df)    
  } else if (s == 2) {
    col.from <- colnames(v$df$plt)
    col.from <- col.from[col.from != "Date"]
    return(
      v$df$plt[v$df$plt$Date >= input$tabRng[1] & v$df$plt$Date <= input$tabRng[2],] %>%
        rename_at(vars(col.from), function(x) xr.NLong[col.from])
    )
  }
})

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