

qxts_series <- reactive({
  showNotification("rendering..")
  req(xl <- input$chkData)
  req(iids <- input$pck.raw)
  xs <- as.character(xr.Nshrt[xl])

  # grab climate data 
  mdf <- v$df[v$df$RDNC %in% xs[xs == "iRainfall" | xs == "iSnowmelt" | xs == "iAirPressure"],] %>% # c('Rainfall','Snowmelt'),]
    dplyr::select(-one_of(c('IID','RDTC','unit','grp'))) %>%
    mutate(Val=round(Val,3)) %>%
    spread(key=RDNC, value=Val)
 
  if ( dim(mdf)[2] < 2 ) { mdf <- NULL }

  sdf <- v$df[v$df$RDNC %in% xs  &  v$df$IID %in% iids,] %>%
    dplyr::select(-one_of(c('RDTC','unit','grp'))) %>%
    group_by_at(vars(-Val)) %>%  # group by everything other than the value column. (from: https://github.com/tidyverse/tidyr/issues/426)
    dplyr::mutate(row_id=1:n()) %>% ungroup() %>% # build group index (from: https://github.com/tidyverse/tidyr/issues/426)
    spread(key=RDNC, value=Val) %>%
    dplyr::select(-one_of(c('row_id','iRainfall','iSnowmelt','iAirPressure')))

  if ( dim(sdf)[2] < 3 ) {
    sdf <- NULL
  } else {
    if (length(unique(v$df$IID)) > 1 ) {
      sdf <- sdf %>% # combine IID and Variable
        gather(variable, value, -(Date:IID)) %>%
        unite(temp, IID, variable) %>%
        group_by(Date, temp) %>%
        dplyr::summarise(value = mean(value)) %>% # grouping and summarizing needed to remove duplicate rows
        ungroup() %>%
        spread(temp, value) %>%
        dplyr::select(where(~!all(is.na(.x)))) # remove all-NA columns
    }
  }
  
  
  # reintroduce climate
  if (is.null(sdf) & is.null(mdf)) {
    return(NULL)
  } else if (is.null(mdf)) {
    x <- xts(sdf, order.by = sdf$Date)
  } else if (is.null(sdf)) {
    x <- xts(mdf, order.by = mdf$Date)
  } else {
    sdf <- sdf %>% left_join(mdf)
    x <- xts(sdf, order.by = sdf$Date)
  }
  
  storage.mode(x) <- "numeric"
  return(x)
})


y2.add <- function(dg, colnam, legendnam, colour) {
  if (length(colnam)>1) colnam=colnam[1] #paste(colnam, collapse = '')
  dg %>% dyBarSeries(colnam, axis = 'y2', color = colour, label = legendnam)
  # dySeries(colnam, axis = 'y2', stepPlot = TRUE, fillGraph = TRUE, color = "#4daf4a", label = legendnam)
}

# output$plt.raw <- renderDygraph({
output$plt.raw <- renderUI({
  req(xl <- input$chkData)
  req(iids <- input$pck.raw)
  if (!is.null(v$df)){
    xs <- as.character(xr.Nshrt[xl])
    qxts <- qxts_series()
    cn <- colnames(qxts)
    
    pp <- grep("Pump", cn, value = TRUE)
    
    # dg <- dygraph(qxts[, !(cn %in% c('iRainfall','iAirPressure','iSnowmelt',pp))], group="dymain", width='100%', height='300px') %>%
    dg <- dygraph(qxts[, !(cn %in% c('iAirPressure',pp))], group="dymain", width='100%', height='300px') %>%
      dyLegend(labelsDiv = "plt.raw.labelsDiv") %>%
      dyRangeSelector(height=30, retainDateWindow=TRUE) %>%
      dyOptions(axisLineWidth = 1.5, 
                connectSeparatedPoints = TRUE, 
                drawPoints = TRUE, 
                pointSize = 2)
    
    if ( !is.null(v$scrn) && input$chkScrn ) {
      dd <- v$df[v$df$RDNC %in% xs[xs!='Rain' & xs!='Snow' & xs!='Precip' & xs!='iRainfall' & xs!='iAirPressure' & xs!='iSnowmelt' & xs!='Pump'] & v$df$IID %in% iids,]$Val
      nscr <- min(c(min(dd[dd>0],na.rm=TRUE),min(v$scrn)))
      xscr <- max(c(max(dd[dd>0],na.rm=TRUE),max(v$scrn)))
      buf <- .05*(xscr-nscr)
      dg <- dg %>% dyAxis("y", valueRange = c(nscr-buf, xscr+buf))

      spc <- strrep(" ",50)
      spcs <- ''
      for (p in names(v$scrn)) {
        dg <- dg %>% dyLimit(v$scrn[[p]], label=paste0(spcs,p), color = "grey22", labelLoc="right")
        spcs = paste0(spcs,spc)
      }
    }
    if ( !is.null(v$v0) && input$chkWL0 ) {
      dg <- dg %>% dyLimit(v$v0, label=paste0('Original Water level (',format(as.POSIXct(v$t0),'%Y-%m-%d'),')'), color = "blue")
    }
    
    qp <- qxts[, cn %in% pp]
    qp$dummy<-NA
    dp <- NULL
    if (length(colnames(qp))>0) {
      dp <- dygraph(qp, group="dymain", width='100%', height='200px') %>% 
        dyBarChart() %>% 
        # dyRangeSelector(height=0, retainDateWindow=TRUE) %>%
        dyLegend(width=600)
      
      dp <- y2.add(dp,"dummy",'dummy',"#ffffff00") # needed to trick the dygraph to think theres a y2 axis, used to align with above polt
      dp <- dp %>%
        dyAxis('y2',
               independentTicks = TRUE,
               drawGrid = FALSE,
               valueRange = c(0, 0))
    }
    
    if ("iSnowmelt" %in% cn) { dg <- y2.add(dg,"iSnowmelt",'snowmelt',"#a6cee3") }
    if ("iRainfall" %in% cn) { dg <- y2.add(dg,"iRainfall",'rainfall',"#1f78b4") }
    dg <- dg %>%     
        dyAxis('y2',
               independentTicks = TRUE,
               drawGrid = FALSE,
               valueRange = c(150, 0))
    
    htmltools::browsable(htmltools::tagList(list(dg,dp)))

    
    # qm <- qxts[, cn %in% c('iRainfall','iAirPressure','iSnowmelt')]
    # if (length(colnames(qm))>0) {
    #   dm <- NULL
    #   if ("iRainfall" %in% cn & "iSnowmelt" %in% cn) {
    #     dm <- dygraph(qm, group="dymain", width='100%', height='100px') %>%
    #       dyStackedBarGroup(c('iRainfall','iSnowmelt'), color = c('#1f78b4','#a6cee3'), label=c('Rainfall', 'Snowmelt'))
    #   } else if ("iRainfall" %in% cn) {
    #     dm <- dygraph(qm, group="dymain", width='100%', height='100px') %>%
    #       dyBarSeries('iRainfall', color = '#1f78b4', label='Rainfall')
    #   } else if ("iSnowmelt" %in% cn) {
    #     dm <- dygraph(qm, group="dymain", width='100%', height='100px') %>%
    #       dyBarSeries('iSnowmelt', color = '#a6cee3', label='Snowmelt')
    #   }
    #   
    #   if ("iAirPressure" %in% cn) {
    #     if (is.null(dm)) {
    #       dm <- dygraph(qm, group="dymain", width='100%', height='100px') %>%
    #         dySeries('iAirPressure', color = 'darkred', label = 'Air Pressure')
    #     } else {
    #       dm <- dm %>% 
    #         dySeries('iAirPressure',axis='y2', color = 'darkred', label = 'Air Pressure') %>%
    #         dyAxis('y2', independentTicks = TRUE, drawGrid = FALSE, valueRange = c(90, 105))
    #     }
    #   }
    #   
    #   if (!is.null(dm)) {
    #     dm <- dm %>%
    #       dyRangeSelector(height=0, retainDateWindow=TRUE) %>%
    #       dyLegend(width=500)        
    #   }
    # } else {
    #   dm <- NULL
    # }
    # 
    # htmltools::browsable(htmltools::tagList(list(dg,dp,dm)))
    
    
    
    
    # rng <- r$rngselect+1
    # xs <- as.character(xr.Nshrt[xl])
    # qxts <- qxts_series()
    # cn <- colnames(qxts)
    # dg <- dygraph(qxts) %>% dyLegend(labelsDiv = "plt.raw.labelsDiv")
    # 
    # # axis 'y2' parameters
    # y2max = 150
    # pp <- grep("Pump", cn, value = TRUE)
    # if (length(pp) > 0) {
    #   # pumpcols <- c("#c57b3d","#946ec6","#76a44a","#ca5477","#3fadaf")
    #   pumpcols <- brewer.pal(n=8, name = "Pastel2")
    #   i<-0
    #   for (pw in pp) {
    #     i=i+1
    #     dg <- y2.add(dg,pw,paste0('production-',pw),pumpcols[i])
    #   }
    #   y2max = max(v$df[v$df$grp == "Production (m³/d)",]$Val, na.rm=TRUE) * 2
    # }
    # if ("iSnowmelt" %in% cn) { dg <- y2.add(dg,"iSnowmelt",'snowmelt',"#a6cee3") }
    # if ("Snow" %in% cn) { dg <- y2.add(dg,"Snow",'snowfall',"#b2df8a") }
    # if ("Rain" %in% cn) { dg <- y2.add(dg,"Rain",'rainfall',"#1f78b4") }
    # if ("iRainfall" %in% cn) { dg <- y2.add(dg,"iRainfall",'rainfall',"#1f78b4") }
    # if ("Precip" %in% cn) { dg <- y2.add(dg,"Precip",'precipiation',"#33a02c") }
    # 
    # dd <- v$df[v$df$RDNC %in% xs[xs!='Rain' & xs!='Snow' & xs!='Precip' & xs!='iRainfall' & xs!='iAirPressure' & xs!='iSnowmelt' & xs!='Pump'] & v$df$IID %in% iids,]$Val
    # 
    # for (cnam in colnames(dd)) {
    #   dg %>% dyBarSeries(cnam)
    # }
    # 
    # dg <- dg %>%
    #   dyAxis('y', label=NULL, valueRange = c(min(dd),max(dd))) %>% #as.character(xl[cn[cn != "Date" & cn != "Rain" & cn != "Snow" & cn != "Rainfall" & cn != "Snowmelt" & cn != "Pump"]])) %>% # , axisLabelWidth=100
    #   dyAxis('y2', 
    #          # label=unname(xr.NLong[as.character(cn[cn == "Rain" | cn == "Snow" | cn == "Precip" | cn == "iRainfall" | cn == "iSnowmelt" | cn == "Pump"])]), 
    #          # label=unname(cn[cn == "Rain" | cn == "Snow" | cn == "Precip" | cn == "iRainfall" | cn == "iSnowmelt" | cn == "Pump"]), 
    #          independentTicks = TRUE,
    #          drawGrid = FALSE,
    #          valueRange = c(y2max, 0)) %>%
    #   dyRangeSelector(fillColor = '', strokeColor = '', height=20, dateWindow = rng) %>% #, retainDateWindow = TRUE) %>%
    #   dyOptions(axisLineWidth = 1.5, connectSeparatedPoints = TRUE)
    # # dyLegend(show = "follow")
    # 
    # if ( !is.null(v$scrn) && input$chkScrn ) {
    #   # dd <- v$df[v$df$RDNC %in% xs[xs!='Rain' & xs!='Snow' & xs!='Precip' & xs!='iRainfall' & xs!='iSnowmelt' & xs!='Pump'],]$Val
    #   nscr <- min(c(min(dd[dd>0],na.rm=TRUE),min(v$scrn)))
    #   xscr <- max(c(max(dd[dd>0],na.rm=TRUE),max(v$scrn)))
    #   buf <- .05*(xscr-nscr)
    #   dg <- dg %>% dyAxis("y", valueRange = c(nscr-buf, xscr+buf))
    #   
    #   spc <- strrep(" ",50)
    #   spcs <- ''
    #   for (p in names(v$scrn)) {
    #     dg <- dg %>% dyLimit(v$scrn[[p]], label=paste0(spcs,p), color = "grey22", labelLoc="right")
    #     spcs = paste0(spcs,spc)
    #   }
    # }
    # if ( !is.null(v$v0) && input$chkWL0 ) {
    #   dg <- dg %>% dyLimit(v$v0, label=paste0('Original Water level (',format(as.POSIXct(v$t0),'%Y-%m-%d'),')'), color = "blue")
    # }
    # 
    # return(dg)
  }
})

