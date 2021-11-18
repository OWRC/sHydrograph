
observe({
  x <- unname(unlist(v$typs)) #unname(xr.NLong[colnames(v$df$plt[-c(1)])])
  # ss <- substring(x,1,regexpr("\\([^\\(]*$", x)-1)
  updateRadioButtons(session, "radio.se", choiceNames=x, choiceValues=x)
})

observe(updateDateRangeInput(session, "se.rng", start = v$DTb, end = v$DTe, min = v$DTb, max = v$DTe))

output$plt.se <- renderPlot({
  req(xl <- input$radio.se)
  input$se.rng
  if (!is.null(v$df$plt)){
    xs <- as.character(xr.Nshrt[xl])
    ylab <- xr.NLong[[xs]]
    
    # summarize by month
    df <- v$df$plt[v$df$plt$Date >= input$se.rng[1] & v$df$plt$Date <= input$se.rng[2],] %>%
      mutate(month = month(Date))
    df$wy = wtr_yr(df$Date)
    df$se <- ''
    df[df$month<3 | df$month>11,]$se = 'DJF'
    df[df$month<6 & df$month>2,]$se = 'MAM'
    df[df$month<9 & df$month>5,]$se = 'JJA'
    df[df$month<12 & df$month>8,]$se = 'SON'
    df$se_f <- factor(df$se,levels=c('DJF','MAM','JJA','SON'))
    
    df <- df %>% group_by(wy,se_f)
    if ( xr.step[xs] ) {
      df <- df %>% dplyr::summarise(stat = sum(!!ensym(xs), na.rm = TRUE), n = sum(!is.na(!!ensym(xs))))
    } else {
      df <- df %>% dplyr::summarise(stat = mean(!!ensym(xs), na.rm = TRUE), n = sum(!is.na(!!ensym(xs))))
    }
    if (nrow(df[df$n==0,])>0) df[df$n==0,]$stat <- NA
    
    p <- ggplot(df, aes(wy,stat)) + 
      theme_bw() +
      geom_step(na.rm = TRUE) + 
      geom_smooth(na.rm=TRUE) +
      facet_grid(rows = vars(se_f), scales = "free") +
      ggtitle(v$title) + 
      ylab(ylab) + xlab('water year (oct-sept)') + 
      scale_x_continuous(breaks= pretty_breaks())
    
    return(p)
  }
})