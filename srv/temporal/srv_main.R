

# observe({
#   input$mouseup
#   isolate({
#     if (!is.null(v$df)){
#       rng <- input$plt.raw_date_window # dummy variable used to trigger below
#     }
#   })
# })

observe({
  y <- colnames(v$df$plt)[2:ncol(v$df$plt)]
  x <- unname(xr.NLong[y])
  s <- x[x != "Temperature (Water) - Logger (degC)"]
  if (anyNA(x)) { showNotification(paste0("unknown RDNC: ", paste(as.character(y[which(is.na(x))]), sep="' '", collapse=", ")), duration = 35) }
  updateCheckboxGroupInput(session, "chkData", choices=x, select=s) #tail(x,1))
  if (is.null(v$scrn)) hide("chkScrn")
})


output$info.main <- renderUI({
  DTb <- as.Date(strftime(req(input$plt.raw_date_window[[1]]), "%Y-%m-%d"))
  DTe <- as.Date(strftime(req(input$plt.raw_date_window[[2]]), "%Y-%m-%d"))
  vis <- input$chkData
  isolate({
    if (!is.null(v$df$plt)){
      df2 <- subset(v$df$plt, Date>=DTb & Date<=DTe)
      nam <- unname(xr.NLong[colnames(v$df$plt)[2:ncol(v$df$plt)]])
      stat <- colMeans(df2[2:ncol(v$df$plt)], na.rm = TRUE) #*(xr.step[xr.Nshrt[nam]]*364.24+1)
      
      df2 <- df2[c('Date',xr.Nshrt[vis])]
      df2 <- df2[rowSums(is.na(df2)) != ncol(df2)-1, ]
      ndat <- nrow(df2)

      shiny::HTML(paste0(
        '<body>',
        v$info.html, br(),

        loc.info('Visible data range:',ndat,DTb,DTe,stat,nam),
        '</body>'
      ))
    }
  })
})



#####################
## plots
#####################
output$plt.raw <- renderDygraph({
  req(input$chkData)
  if (!is.null(v$df$plt)){
    rf <- xr.NLong[["Rf"]]
    sm <- xr.NLong[["Sm"]]
    xl <- input$chkData
    xs <- as.character(xr.Nshrt[xl])
    qxts <- xts(v$df$plt[,xs], order.by = v$df$plt$Date)
    colnames(qxts) <- xs
    dg <- dygraph(qxts) %>%
      dyOptions(retainDateWindow = TRUE, axisLineWidth = 1.5) %>% #, fillGraph = TRUE, stepPlot = as.logical(xr.step[xs])) %>%
      dyAxis(name='y', label=as.character(xl[xl != rf & xl != sm]), axisLabelWidth=100) %>%
      dyRangeSelector(fillColor = '', height=80) %>%
      dyLegend(show = "follow")
    if (sm %in% xl) {
      dg <- dg %>% 
        dySeries("Sm", axis = 'y2', stepPlot = TRUE, label = 'snowmelt') %>% #, fillGraph = TRUE) %>% #"#008080"
        dyAxis('y2', label=sm, valueRange = c(200, 0))
    }      
    if (rf %in% xl) {
      dg <- dg %>% 
        dySeries("Rf", axis = 'y2', stepPlot = TRUE, label = 'rainfall') %>% #, fillGraph = TRUE) %>% #"#008080"
        dyAxis('y2', label=rf, valueRange = c(200, 0))
    }    
  
    if ( !is.null(v$scrn) && input$chkScrn ) {
      dd <- v$df$plt[,xs[xs!='AtmosYld']]
      nscr <- min(c(min(dd[dd>0],na.rm=TRUE),min(v$scrn)))
      xscr <- max(c(max(dd[dd>0],na.rm=TRUE),max(v$scrn)))
      buf <- .05*(xscr-nscr)
      dg <- dg %>% dyAxis("y", valueRange = c(nscr-buf, xscr+buf))
      
      spc <- strrep(" ",50)
      spcs <- ''
      for (p in names(v$scrn)) {
        dg <- dg %>% dyLimit(v$scrn[[p]], label=paste0(spcs,p), color = "grey22")
        spcs = paste0(spcs,spc)
      }      
    }

    return(dg)
  }
})