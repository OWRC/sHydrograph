
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
  x <- unname(unlist(v$typs))
  s <- x[x != "Temperature (Water) - Logger (°C)"] # default layers to uncheck
  if (anyNA(x)) { showNotification(paste0("unknown RDNC: ", paste(as.character(y[which(is.na(x))]), sep="' '", collapse=", ")), duration = 35) }
  updateCheckboxGroupInput(session, "chkData", choices=x, select=s) #tail(x,1))
  if (is.null(v$scrn)) hide("chkScrn")
})

observe({
  x <- unname(unlist(v$inam))
  updatePickerInput(session,"pck.raw", choices = x, selected = x)
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
## ggplot
# https://stackoverflow.com/questions/35806310/ggplot-plotting-layers-only-if-certain-criteria-are-met
pick <- function(condition){ function(d) d %>% filter(!!enquo(condition)) }

output$plt.print <- renderPlot({
  req(rng <- input$dt.rng)
  req(xl <- input$chkData)
  if (!is.null(v$df)){
    withProgress(message = 'building plot..', value = 0.1, {
      xs <- as.character(xr.Nshrt[xl])
      v$df[v$df$RDNC %in% xs & v$df$IID %in% input$pck.raw,] %>% 
        subset( Date >= rng[[1]]  &  Date <= rng[[2]] ) %>%
        ggplot(aes(Date,Val)) +
        theme_bw() + theme(
          axis.title=element_blank(),
          legend.title=element_blank(),
          strip.placement = "outside",
          strip.background = element_blank()
        ) +
        geom_step(data = pick(grp == "Temperature (°C)"), aes(colour=IID)) +
        geom_line(data = pick(RDNC == "WtrLvl"),aes(colour=IID)) +
        geom_line(data = pick(grp == "Stream flow (m³/s)"), aes(colour=IID)) +
        geom_point(data = pick(RDNC == "WtrLvl.s"), aes(colour=IID)) +
        geom_col(data = pick(grp == "Precipitation (mm)"), aes(fill=RDNC), position=position_stack()) +
        geom_col(data = pick(grp == "Production (m³/d)"), aes(fill=IID), position=position_dodge()) +
        facet_wrap(~grp, ncol=1, scales = "free_y", strip.position = "left")
    })
  }
})


## Dygraph
qxts_series <- reactive({
  showNotification("rendering..")
  req(xl <- input$chkData)
  req(iids <- input$pck.raw)
  xs <- as.character(xr.Nshrt[xl])
  
  # grab climate data 
  mdf <- v$df[v$df$RDNC %in% xs[xs == "Rainfall" | xs == "Snowmelt"],] %>% # c('Rainfall','Snowmelt'),]
    dplyr::select(-one_of(c('IID','RDTC','unit','grp'))) %>%
    spread(key=RDNC, value=Val)
  
  if ( dim(mdf)[2] < 2 ) { mdf <- NULL }

  sdf <- v$df[v$df$RDNC %in% xs  &  v$df$IID %in% iids,] %>%
    dplyr::select(-one_of(c('RDTC','unit','grp'))) %>%
    group_by_at(vars(-Val)) %>%  # group by everything other than the value column. (from: https://github.com/tidyverse/tidyr/issues/426)
    mutate(row_id=1:n()) %>% ungroup() %>% # build group index (from: https://github.com/tidyverse/tidyr/issues/426)
    spread(key=RDNC, value=Val) %>%
    dplyr::select(-one_of(c('row_id','Rainfall','Snowmelt')))
  
  if ( dim(sdf)[2] < 3 ) {
    sdf <- NULL
  } else {
    sdf <- sdf %>% # combine IID and Variable
      gather(variable, value, -(Date:IID)) %>%
      unite(temp, IID, variable) %>%
      group_by(Date, temp) %>%
      dplyr::summarise(value = mean(value)) %>% # grouping and summarizing needed to remove duplicate rows
      ungroup() %>%
      spread(temp, value) %>%
      dplyr::select(where(~!all(is.na(.x)))) # remove all-NA columns    
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


output$plt.raw <- renderDygraph({
  req(xl <- input$chkData)
  if (!is.null(v$df)){
    rng <- r$rngselect+1
    xs <- as.character(xr.Nshrt[xl])
    qxts <- qxts_series()
    cn <- colnames(qxts)
    dg <- dygraph(qxts)
    
    y2max = 150
    pp <- grep("Pump", cn, value = TRUE)
    if (length(pp) > 0) {
      dg <- dg %>%
        # dySeries("Pump", axis = 'y2', stepPlot = TRUE, fillGraph = TRUE, color = "#e41a1c", label = 'production')
        dyBarSeries(pp, axis = 'y2', color = "#e41a1cBF", label = 'production')
      y2max = max(v$df[v$df$grp == "Production (m³/d)",]$Val, na.rm=TRUE) * 2
    }
    if ("Snowmelt" %in% cn) {
      dg <- dg %>%
        # dySeries("Sm", axis = 'y2', stepPlot = TRUE, fillGraph = TRUE, color = "#4daf4a", label = 'snowmelt')
        dyBarSeries("Snowmelt", axis = 'y2', color = "#4daf4aBF", label = 'snowmelt')
    }
    if ("Rainfall" %in% cn) {
      dg <- dg %>%
        # dySeries("Rf", axis = 'y2', stepPlot = TRUE, fillGraph = TRUE, color = "#377eb8", label = 'rainfall')
        dyBarSeries("Rainfall", axis = 'y2', color = "#377eb8BF", label = 'rainfall')
    }

    dg <- dg %>%
      dyAxis('y', label=as.character(xl[cn[cn != "Date" & cn != "Rainfall" & cn != "Snowmelt" & cn != "Pump"]]), axisLabelWidth=100) %>%
      dyAxis('y2', label=as.character(cn[cn == "Rainfall" | cn == "Snowmelt" | cn == "Pump"]), valueRange = c(y2max, 0)) %>%
      dyRangeSelector(fillColor = '', height=40, dateWindow = rng, retainDateWindow = TRUE) %>%
      dyOptions(axisLineWidth = 1.5, connectSeparatedPoints = TRUE) # %>%  # 
      # dyLegend(show = "follow")

    if ( !is.null(v$scrn) && input$chkScrn ) {
      dd <- v$df[v$df$RDNC %in% xs[xs!='Rainfall' & xs!='Snowmelt' & xs!='Pump'],]$Val
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
 
