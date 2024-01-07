

observe({
  x <- unname(xr.NLong[unique(v$df[v$df$IID %in% input$pck.an,]$RDNC)])
  updateCheckboxGroupInput(session, "chk.an", choices=x, select=x)
})

# observe(updateDateRangeInput(session, "an.rng", start = v$DTb, end = v$DTe, min = v$DTb, max = v$DTe))

observe({
  x <- unique(v$df$IID)
  updatePickerInput(session,"pck.an", choices = x, selected = x)
})


########################################################
# annual time series summary
########################################################
summary_annual <- function(df,relative=FALSE){
  req(xl <- input$chk.an)
  req(iids <- input$pck.an)
  req(typ <- input$sel.an)
  xs <- as.character(xr.Nshrt[xl])

  # summarize by year
  df1 <- df[df$RDNC %in% xs & df$IID %in% iids,] %>%
    drop_na(Val) %>%
    mutate(year = year(Date)) %>%
    subset(year>min(year) & year<max(year)) # cropping first and last years
  
  df1$m1 <- xr.step[df1$RDNC]

  if (typ=='Maximum') {
    df1 <- df1 %>%
      group_by(IID,grp,RDNC,m1,year) %>% 
      dplyr::summarise(stat = max(Val, na.rm = TRUE), n = sum(!is.na(Val))) 
  } else if (typ=='Minimum') {
    df1 <- df1 %>%
      group_by(IID,grp,RDNC,m1,year) %>% 
      dplyr::summarise(stat = min(Val, na.rm = TRUE), n = sum(!is.na(Val))) 
  } else if (typ=='Median') {
    df1 <- df1 %>%
      group_by(IID,grp,RDNC,m1,year) %>% 
      dplyr::summarise(stat = median(Val, na.rm = TRUE), n = sum(!is.na(Val)))       
  # } else if (typ='Sum') {
  #   df1 <- df1 %>%
  #     group_by(IID,grp,RDNC,m1,year) %>% 
  #     dplyr::summarise(stat = sum(Val, na.rm = TRUE), n = sum(!is.na(Val)))             
  } else { #  (typ=="Mean")
    df1 <- df1 %>%
      group_by(IID,grp,RDNC,m1,year) %>% 
      dplyr::summarise(stat = sum(Val, na.rm = TRUE), n = sum(!is.na(Val))) %>%
      # mutate(m1 = recode(RDNC, !!!xr.step, .default = NA)) %>%
      mutate(stat = case_when(!m1 ~ stat/n, TRUE ~ stat))
  }

  if (nrow(df1[df1$n==0,])>0) df1[df1$n==0,]$stat <- NA
  df1 <- df1 %>% mutate(statmean = mean(stat, na.rm=TRUE))

  if(!relative){
    p <- ggplot(df1, aes(year,stat, color=IID, linetype=RDNC)) +
      theme_bw() + theme(legend.position="bottom", legend.title= element_blank()) +
      geom_step(direction = "mid") +
      # geom_hline(yintercept = statmean, size=1, linetype='dotted') +
      # geom_label(aes(x = min(year), y=statmean, label=paste0("mean ",ylab," = ",round(1,0))), hjust=0,vjust=-.5,fill = "white") +
      labs(y = NULL, x=NULL)
  }else{
    df1$diff <- df1$stat - df1$statmean
    p <- ggplot(df1, aes(year,diff, color=IID, fill=RDNC)) +
      theme_bw() + theme(legend.position="bottom", legend.title= element_blank()) +
      theme(axis.text.x=element_text(angle=90,vjust=0),axis.title.x=element_blank()) +
      geom_bar(stat="identity", position = "dodge", alpha=.25) +
      geom_smooth(na.rm=TRUE) +
      labs(y = NULL, x=NULL)
  }
  return(p + ggtitle(iids) + facet_wrap(~grp, ncol=1, scales = "free_y", strip.position = "left"))
}


######################
### plots
######################
output$plt.an.tot <- renderPlot({
  if (!is.null(v$df)) summary_annual(v$df)
  # req(rng <- input$an.rng)
  # if (!is.null(v$df)){
  #   summary_annual(v$df[v$df$Date >= rng[[1]] & v$df$Date <= rng[[2]],])
  # }
})

output$plt.an.diff <- renderPlot({
  if (!is.null(v$df)) summary_annual(v$df,TRUE)
  # req(rng <- input$an.rng)
  # if (!is.null(v$df)){
  #   summary_annual(v$df[v$df$Date >= rng[[1]] & v$df$Date <= rng[[2]],],TRUE)
  # }
})
