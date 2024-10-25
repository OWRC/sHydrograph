
observe({
  req(i <- input$int.se)
  x <- unname(xr.NLong[unique(v$df[v$df$IID==i,]$RDNC)])
  updateRadioButtons(session, "radio.se", choiceNames=x, choiceValues=x)
})

# observe(updateDateRangeInput(session, "se.rng", start = v$DTb, end = v$DTe, min = v$DTb, max = v$DTe))

observe({
  typs <- unique(v$df$IID)
  updateSelectInput(session,"int.se", choices = typs) #, selected = typs)
})


output$plt.se <- renderPlot({
  req(xl <- input$radio.se)
  # req(rng <- input$se.rng)
  req(iid <- input$int.se)
  if (!is.null(v$df)){
    xs <- as.character(xr.Nshrt[xl])
    ylab <- xr.NLong[[xs]]
    
    # summarize by month
    # df <- v$df[v$df$Date >= rng[[1]] & v$df$Date <= rng[[2]] & v$df$RDNC==xs & v$df$IID==iid,] %>%
    df <- v$df[v$df$RDNC==xs & v$df$IID==iid,] %>%
      drop_na(Val) %>%
      mutate(year = year(Date), month = month(Date)) %>%
      subset(year>min(year) & year<max(year))

    df$wy = wtr_yr(df$Date)
    df$se <- ''
    df[df$month<3 | df$month>11,]$se = 'DJF'
    df[df$month<6 & df$month>2,]$se = 'MAM'
    df[df$month<9 & df$month>5,]$se = 'JJA'
    df[df$month<12 & df$month>8,]$se = 'SON'
    df$se_f <- factor(df$se,levels=c('DJF','MAM','JJA','SON'))
    
    df <- df %>% group_by(wy,se_f)
    if ( xr.step[xs] ) {
      df <- df %>% dplyr::summarise(stat = sum(Val, na.rm = TRUE), n = sum(!is.na(Val)))
    } else {
      df <- df %>% dplyr::summarise(stat = mean(Val, na.rm = TRUE), n = sum(!is.na(Val)))
    }
    if (nrow(df[df$n==0,])>0) df[df$n==0,]$stat <- NA
    
    p <- ggplot(df, aes(wy,stat)) + 
      theme_bw() +
      geom_step(na.rm = TRUE) + 
      geom_smooth(na.rm=TRUE) +
      facet_grid(rows = vars(se_f), scales = "free") +
      ggtitle(iid) + 
      ylab(ylab) + xlab('water year (oct-sept)') + 
      scale_x_continuous(breaks= pretty_breaks())
    
    return(p)
  }
}, res=ggres)