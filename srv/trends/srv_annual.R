

observe({
  x <- unname(xr.NLong[colnames(v$df$plt[-c(1)])])
  # ss <- substring(x,1,regexpr("\\([^\\(]*$", x)-1)
  updateRadioButtons(session, "radio.an", choiceNames=x, choiceValues=x)
})

observe(updateDateRangeInput(session, "an.rng", start = v$DTb, end = v$DTe, min = v$DTb, max = v$DTe))

########################################################
# annual timeseries summary
########################################################
summary_annual <- function(df,relative=FALSE){
  xl <- input$radio.an
  xs <- as.character(xr.Nshrt[xl])
  ylab <- xr.NLong[[xs]]
  
  # summarize by year
  df <- df %>%
    mutate(year = year(Date)) %>%
    group_by(year)
  
  if ( xr.step[xs] ) {
    df <- df %>% dplyr::summarise(stat = sum(!!ensym(xs), na.rm = TRUE), n = sum(!is.na(!!ensym(xs))))
  } else {
    df <- df %>% dplyr::summarise(stat = mean(!!ensym(xs), na.rm = TRUE), n = sum(!is.na(!!ensym(xs))))
  }
  
  
  if (nrow(df[df$n==0,])>0) df[df$n==0,]$stat <- NA
  mP <- mean(df$stat, na.rm=TRUE)
  
  if(!relative){
    p <- ggplot(df, aes(year,stat)) +
      theme_bw() +
      geom_step(direction = "mid") + 
      geom_hline(yintercept = mP, size=1, linetype='dotted') +
      geom_label(aes(x = min(year), y=mP, label=paste0("mean ",ylab," = ",round(mP,0))), hjust=0,vjust=-.5,fill = "white") +
      labs(y = ylab, x=NULL)
  }else{
    df$stat <- df$stat - mP
    p <- ggplot(df, aes(year,stat)) +
      theme_bw() +
      theme(axis.text.x=element_text(angle=90,vjust=0),axis.title.x=element_blank()) +
      geom_bar(stat="identity") +
      geom_smooth(na.rm=TRUE) +
      labs(y = paste0("Relative ", ylab), x=NULL)
  }
  return(p + ggtitle(v$title))
}

######################
### plots
######################
output$plt.an.tot <- renderPlot({
  req(input$radio.an)
  if (!is.null(v$df$plt)){
    summary_annual(v$df$plt[v$df$plt$Date >= input$an.rng[1] & v$df$plt$Date <= input$an.rng[2],])
  }
})

output$plt.an.diff <- renderPlot({
  req(input$radio.an)
  if (!is.null(v$df$plt)){
    summary_annual(v$df$plt[v$df$plt$Date >= input$an.rng[1] & v$df$plt$Date <= input$an.rng[2],],TRUE)
  }
})
