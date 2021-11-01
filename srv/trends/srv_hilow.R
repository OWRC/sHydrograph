

observe({
  x <- unname(xr.NLong[colnames(v$df$plt[-c(1)])])
  # ss <- substring(x,1,regexpr("\\([^\\(]*$", x)-1)
  updateRadioButtons(session, "radio.hl", choiceNames=x, choiceValues=x)
})

observe(updateDateRangeInput(session, "hl.rng", start = v$DTb, end = v$DTe, min = v$DTb, max = v$DTe))

######################
### plots
######################
output$plt.hl.all <- renderPlot({
  req(input$radio.hl)
  if (!is.null(v$df$orig)){
    xl <- input$radio.hl
    xs <- as.character(xr.Nshrt[xl])
    xi <- as.numeric(xr.Nindx[xs])
    ylab <- xr.NLong[[xs]]
    
    df1 <- remove.outliers(v$df$orig, xi)
    df1 <- df1[df1$Date >= input$hl.rng[1] & df1$Date <= input$hl.rng[2],]
    
    # ggplot(df1, aes(Date,Val)) +
    #   theme_bw() +
    #   geom_point() +
    #   labs(y = ylab, x=NULL) + 
    #   ggtitle(v$title)
    
    df366 <- df1 %>%
      mutate(year = year(Date), doy = yday(Date))
    df366$doym <- as.Date(df366$doy, origin = "2000-01-01")
    
    df366$year <- as.factor(df366$year)
    mP <- mean(df366$Val,na.rm=TRUE)
    
    ggplot(df366, aes(doym,Val)) +
      theme_bw() +
      geom_path(aes(colour=year)) +
      geom_smooth(na.rm=TRUE) +
      geom_hline(yintercept = mP,linetype='dashed') +
      # geom_label(aes(x = as.Date('2000-02-01'), y=mP, label=paste0("mean ",ylab," = ",round(mP,1))), hjust=0,vjust=-.5,fill = "white") +
      scale_x_date(breaks = date_breaks("months"),labels = date_format("%b")) +
      labs(y = ylab, x=NULL) +
      ggtitle(v$title)
  }
})

output$plt.hl.dens <- renderPlot({
  req(input$radio.hl)
  if (!is.null(v$df$orig)){
    xl <- input$radio.hl
    xs <- as.character(xr.Nshrt[xl])
    xi <- as.numeric(xr.Nindx[xs])
    ylab <- paste0(xr.NLong[[xs]],'\npeak density')
    
    df1 <- remove.outliers(v$df$orig, xi)
    df1 <- df1[df1$Date >= input$hl.rng[1] & df1$Date <= input$hl.rng[2],]
    
    dfhl <- df1 %>%
      drop_na() %>%
      mutate(year = year(Date), doy = yday(Date)) %>%
      group_by(year) %>%
      dplyr::summarise(low = doy[which.min(Val)], high = doy[which.max(Val)]) %>% 
      slice(rep(row_number(), 3))
    
    dfhl <- melt(dfhl[,c('year','low','high')],id.vars = 1)
    # dfhl$value <- as.Date(dfhl$value, origin = "2000-01-01")
    # 
    # ggplot(dfhl,aes(value,fill=variable)) +
    #   theme_bw() + 
    #   geom_density(alpha=.3) +
    #   scale_x_date(breaks = date_breaks("months"),labels = date_format("%b")) +
    #   labs(y = ylab, x=NULL) +
    #   ggtitle(v$title)
    
    n <- nrow(dfhl)/6
    dfhl[1:n,]$value <- dfhl[1:n,]$value - 365
    dfhl[(2*n+1):(3*n),]$value <- dfhl[(2*n+1):(3*n),]$value + 365
    dfhl[(3*n+1):(3*n+n),]$value <- dfhl[(3*n+1):(3*n+n),]$value - 365
    dfhl[(3*n+2*n+1):(3*n+3*n),]$value <- dfhl[(3*n+2*n+1):(3*n+3*n),]$value + 365
    
    dfhl$value <- as.Date(dfhl$value, origin = "2000-01-01")
    
    ggplot(dfhl,aes(value,fill=variable,colour=variable)) +
      theme_bw() + 
      geom_density(alpha=.3,adjust=1/6) +
      geom_rug() +
      scale_x_date(breaks = date_breaks("months"),labels = date_format("%b"),name=element_blank()) +
      coord_cartesian(xlim=as.Date(c('2000-01-01','2000-12-31')))
  }
})
