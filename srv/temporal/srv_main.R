
# observe({
#   input$mouseup
#   isolate({
#     if (!is.null(v$df)){
#       rng <- input$plt.raw_date_window # dummy variable used to trigger below
#     }
#   })
# })

observeEvent(input$plt.raw_date_window, { updated_date_window(input$plt.raw_date_window,"dt.rng") })

observeEvent(input$dt.rng, { updated_date_selector(input$dt.rng) })


observe({
  updateDateRangeInput(session, "dt.rng", start = v$DTb, end = v$DTe, min = v$DTb, max = v$DTe)
  r.rngselect = c(v$DTb, v$DTe)
})

observe({
  y <- colnames(v$df$plt)[2:ncol(v$df$plt)]
  x <- unname(xr.NLong[y])
  s <- x[x != "Temperature (Water) - Logger (degC)"]
  if (anyNA(x)) { showNotification(paste0("unknown RDNC: ", paste(as.character(y[which(is.na(x))]), sep="' '", collapse=", ")), duration = 35) }
  updateCheckboxGroupInput(session, "chkData", choices=x, select=s) #tail(x,1))
  if (is.null(v$scrn)) hide("chkScrn")
})


output$info.main <- renderUI({
  DTb <- as.Date(strftime(req(input$dt.rng[[1]]), "%Y-%m-%d"))
  DTe <- as.Date(strftime(req(input$dt.rng[[2]]), "%Y-%m-%d"))
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
        loc.info(ndat,DTb,DTe,stat,nam),
        '</body>'
      ))
    }
  })
})



#####################
## plots
#####################
qxts_series <- reactive({
  req(xl <- input$chkData)
  xs <- as.character(xr.Nshrt[xl])
  qxts <- xts(v$df$plt[,xs], order.by = v$df$plt$Date)
})



output$plt.raw <- renderDygraph({
  # dygraph(qxts_series()) %>%
  #   dyRangeSelector(dateWindow = r$rngselect+1)
  req(input$chkData)
  if (!is.null(v$df$plt)){
    rng <- r$rngselect+1
    print(rng)
    rf <- xr.NLong[["Rf"]]
    sm <- xr.NLong[["Sm"]]
    pp <- xr.NLong[["Pump"]]
    xl <- input$chkData
    xs <- as.character(xr.Nshrt[xl])
    qxts <- xts(v$df$plt[,xs], order.by = v$df$plt$Date)
    colnames(qxts) <- xs
    dg <- dygraph(qxts)

    y2max = 200
    if (pp %in% xl) {
      dg <- dg %>%
        dySeries("Pump", axis = 'y2', stepPlot = TRUE, fillGraph = TRUE, color = "#e41a1c", label = 'production') 
      y2max = max(v$df$plt[,xr.Nshrt[[pp]]], na.rm=TRUE) * 2
    }
    if (sm %in% xl) {
      dg <- dg %>%
        dySeries("Sm", axis = 'y2', stepPlot = TRUE, fillGraph = TRUE, color = "#4daf4a", label = 'snowmelt')
    }
    if (rf %in% xl) {
      dg <- dg %>%
        dySeries("Rf", axis = 'y2', stepPlot = TRUE, fillGraph = TRUE, color = "#377eb8", label = 'rainfall')
    }    
    dg <- dg %>% 
      dyAxis('y', label=as.character(xl[xl != rf & xl != sm & xl != pp]), axisLabelWidth=100) %>%
      dyAxis('y2', label=as.character(xl[xl == rf | xl == sm | xl == pp]), valueRange = c(y2max, 0)) %>%
      dyRangeSelector(fillColor = '', height=40, dateWindow = rng, retainDateWindow = TRUE) %>%
      dyOptions(axisLineWidth = 1.5, connectSeparatedPoints = TRUE) # %>%
      # dyLegend(show = "follow")      

    
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



output$plt.print <- renderPlot({
  req(input$chkData)
  if (!is.null(v$df$orig)){
    v$df$orig %>%
      ggplot(aes(Date,Val)) + geom_line(aes(colour=RDNC))
  }
})