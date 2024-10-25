
observe({
  req(i <- input$int.bko)
  x <- unname(xr.NLong[unique(v$df[v$df$IID==i,]$RDNC)])
  updateRadioButtons(session, "radio.bko", choiceNames=x, choiceValues=x)
})

observe({
  typs <- unique(v$df$IID)
  updateSelectInput(session,"int.bko", choices = typs) #, selected = typs)
})


output$plt.bko <- renderPlot({
  req(xl <- input$radio.bko)
  req(iid <- input$int.bko)
  if (!is.null(v$df)){
    xs <- as.character(xr.Nshrt[xl])
    ylab <- xr.NLong[[xs]]
    
    # summarize by month
    df <- v$df[v$df$RDNC==xs & v$df$IID==iid,] %>%
      dplyr::select(Date,Val) %>%
      complete(Date = seq.Date(min(Date), max(Date) , by= "day"))
      

    # following https://thecodeforest.github.io/post/time_series_outlier_detection.html
    dts = ts(df$Val, frequency = 365)
    decomp_ts = stl(ts(df$Val, frequency = 365),
                    na.action=na.approx,
                    s.window = "periodic",
                    robust = TRUE
    )
    
    
    
    ts_decomposition = data.frame(decomp_ts$time.series) %>%
      mutate(hydrograph=df$Val) %>%
      dplyr::select(hydrograph, seasonal, trend, remainder) %>%
      melt() %>%
      mutate(date = rep(df$Date, 4)) %>%
      dplyr::rename(component = variable)

    p <- ggplot(ts_decomposition, aes(x=date, y=value, color=component)) +
      theme_bw() +
      theme(legend.position = "none") +
      geom_line(stat = "identity") +
      facet_wrap(component~ ., ncol=1, scales = "free_y", strip.position = "left") +
      scale_color_manual(values = c('#4987d0','#83ab55','#a14f9d','#c05f43')) +
      labs(x="Date",y=NULL,title=iid, subtitle = 'Seasonal decomposition')
    
    return(p)
  }
}, res=ggres)