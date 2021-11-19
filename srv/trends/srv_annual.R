

observe({
  req(i <- input$int.an)
  x <- unname(xr.NLong[unique(v$df[v$df$IID==i,]$RDNC)])
  updateRadioButtons(session, "radio.an", choiceNames=x, choiceValues=x)
})

observe(updateDateRangeInput(session, "an.rng", start = v$DTb, end = v$DTe, min = v$DTb, max = v$DTe))

observe({
  typs <- unique(v$df$IID)
  updateSelectInput(session,"int.an", choices = typs, selected = typs)
})


########################################################
# annual time series summary
########################################################
summary_annual <- function(df,relative=FALSE){
  iid <- input$int.an
  xl <- input$radio.an
  xs <- as.character(xr.Nshrt[xl])
  ylab <- xr.NLong[[xs]]
  
  # summarize by year
  df1 <- df[df$RDNC==xs & df$IID==iid,] %>%
    mutate(year = year(Date)) %>%
    group_by(year)

  if ( xr.step[xs] ) {
    df1 <- df1 %>% dplyr::summarise(stat = sum(Val, na.rm = TRUE), n = sum(!is.na(Val)))
  } else {
    df1 <- df1 %>% dplyr::summarise(stat = mean(Val, na.rm = TRUE), n = sum(!is.na(Val)))
  }

  if (nrow(df1[df1$n==0,])>0) df1[df1$n==0,]$stat <- NA
  mP <- mean(df1$stat, na.rm=TRUE)

  if(!relative){
    p <- ggplot(df1, aes(year,stat)) +
      theme_bw() +
      geom_step(direction = "mid") +
      geom_hline(yintercept = mP, size=1, linetype='dotted') +
      geom_label(aes(x = min(year), y=mP, label=paste0("mean ",ylab," = ",round(mP,0))), hjust=0,vjust=-.5,fill = "white") +
      labs(y = ylab, x=NULL)
  }else{
    df1$stat <- df1$stat - mP
    p <- ggplot(df1, aes(year,stat)) +
      theme_bw() +
      theme(axis.text.x=element_text(angle=90,vjust=0),axis.title.x=element_blank()) +
      geom_bar(stat="identity") +
      geom_smooth(na.rm=TRUE) +
      labs(y = paste0("Relative ", ylab), x=NULL)
  }
  return(p + ggtitle(iid))
}


######################
### plots
######################
output$plt.an.tot <- renderPlot({
  req(rng <- input$an.rng)
  if (!is.null(v$df)){
    summary_annual(v$df[v$df$Date >= rng[[1]] & v$df$Date <= rng[[2]],])
  }
})

output$plt.an.diff <- renderPlot({
  req(rng <- input$an.rng)
  if (!is.null(v$df)){
    summary_annual(v$df[v$df$Date >= rng[[1]] & v$df$Date <= rng[[2]],],TRUE)
  }
})
