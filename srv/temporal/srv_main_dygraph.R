

qxts_series <- reactive({
  showNotification("rendering..")
  req(xl <- input$chkData)
  req(iids <- input$pck.raw)
  xs <- as.character(xr.Nshrt[xl])
  
  # grab climate data 
  mdf <- v$df[v$df$RDNC %in% xs[xs == "iRainfall" | xs == "iSnowmelt" | xs == "iAirPressure"],] %>% # c('Rainfall','Snowmelt'),]
    dplyr::select(-one_of(c('IID','RDTC','unit','grp'))) %>%
    spread(key=RDNC, value=Val)
  
  if ( dim(mdf)[2] < 2 ) { mdf <- NULL }
  
  sdf <- v$df[v$df$RDNC %in% xs  &  v$df$IID %in% iids,] %>%
    dplyr::select(-one_of(c('RDTC','unit','grp'))) %>%
    group_by_at(vars(-Val)) %>%  # group by everything other than the value column. (from: https://github.com/tidyverse/tidyr/issues/426)
    mutate(row_id=1:n()) %>% ungroup() %>% # build group index (from: https://github.com/tidyverse/tidyr/issues/426)
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
    xts(sdf, order.by = sdf$Date)
  } else if (is.null(sdf)) {
    xts(mdf, order.by = mdf$Date)
  } else {
    sdf <- sdf %>% inner_join(mdf)
    xts(sdf, order.by = sdf$Date)
  }
})


y2.add <- function(dg, colnam, legendnam, colour) {
  dg %>% dyBarSeries(colnam, axis = 'y2', color = colour, label = legendnam)
  # dySeries(colnam, axis = 'y2', stepPlot = TRUE, fillGraph = TRUE, color = "#4daf4a", label = legendnam)
}

output$plt.raw <- renderDygraph({
  req(xl <- input$chkData)
  req(iids <- input$pck.raw)
  if (!is.null(v$df)){
    rng <- r$rngselect+1
    xs <- as.character(xr.Nshrt[xl])
    qxts <- qxts_series()
    cn <- colnames(qxts)
    dg <- dygraph(qxts)
    
    y2max = 150
    pp <- grep("Pump", cn, value = TRUE)
    if (length(pp) > 0) {
      dg <- y2.add(dg,pp,'production',"#fb9a99")
      y2max = max(v$df[v$df$grp == "Production (m³/d)",]$Val, na.rm=TRUE) * 2
    }
    if ("iSnowmelt" %in% cn) { dg <- y2.add(dg,"iSnowmelt",'snowmelt',"#a6cee3") }
    if ("Snow" %in% cn) { dg <- y2.add(dg,"Snow",'snowfall',"#b2df8a") }
    if ("Rain" %in% cn) { dg <- y2.add(dg,"Rain",'rainfall',"#1f78b4") }
    if ("iRainfall" %in% cn) { dg <- y2.add(dg,"iRainfall",'rainfall',"#1f78b4") }
    if ("Precip" %in% cn) { dg <- y2.add(dg,"Precip",'precipiation',"#33a02c") }
    
    dd <- v$df[v$df$RDNC %in% xs[xs!='Rain' & xs!='Snow' & xs!='Precip' & xs!='iRainfall' & xs!='iSnowmelt' & xs!='Pump'] & v$df$IID %in% iids,]$Val
    dg <- dg %>%
      dyAxis('y', label=NULL, valueRange = c(min(dd),max(dd))) %>% #as.character(xl[cn[cn != "Date" & cn != "Rain" & cn != "Snow" & cn != "Rainfall" & cn != "Snowmelt" & cn != "Pump"]])) %>% # , axisLabelWidth=100
      dyAxis('y2', 
             # label=unname(xr.NLong[as.character(cn[cn == "Rain" | cn == "Snow" | cn == "Precip" | cn == "iRainfall" | cn == "iSnowmelt" | cn == "Pump"])]), 
             # label=unname(cn[cn == "Rain" | cn == "Snow" | cn == "Precip" | cn == "iRainfall" | cn == "iSnowmelt" | cn == "Pump"]), 
             independentTicks = TRUE,
             drawGrid = FALSE,
             valueRange = c(y2max, 0)) %>%
      dyRangeSelector(fillColor = '', strokeColor = '', height=20, dateWindow = rng, retainDateWindow = TRUE) %>%
      dyOptions(axisLineWidth = 1.5, connectSeparatedPoints = TRUE)
    # dyLegend(show = "follow")
    
    if ( !is.null(v$scrn) && input$chkScrn ) {
      # dd <- v$df[v$df$RDNC %in% xs[xs!='Rain' & xs!='Snow' & xs!='Precip' & xs!='iRainfall' & xs!='iSnowmelt' & xs!='Pump'],]$Val
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

