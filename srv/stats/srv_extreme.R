
observe({
  req(i <- input$int.ax)
  x <- unname(xr.NLong[unique(v$df[v$df$IID==i,]$RDNC)])
  updateRadioButtons(session, "radio.ax", choiceNames=x, choiceValues=x)
})

observe({
  typs <- unique(v$df$IID)
  updateSelectInput(session,"int.ax", choices = typs, selected = typs)
})

#########################################################################

frequencyPlot <- function(series, years, ci, ylab=NULL, inverted=FALSE) {
  
  # determine plotting positions
  if(inverted) {
    bwpeaks <- data.frame(PROB = 1-pp(series, sort = FALSE), Val = series)
    nep <- 1-ci$nonexceed_prob
    lorg <- c(0, 0)
    lpos <- c(.01, .01)    
  } else {
    bwpeaks <- data.frame(PROB = pp(series, sort = FALSE), Val = series)
    nep <- ci$nonexceed_prob
    lorg <- c(1, 0)
    lpos <- c(.99, .01)    
  }
  bwpeaks$year = years
  
  xbreaks <- c(0.002,0.01,0.1,0.25,0.5,0.8,0.9,0.95,0.975,0.99,0.995,0.998)
  rnge <- range(series, ci[,ncol(ci)], na.rm=TRUE)
  # srng <- log10(rnge[2])-log10(rnge[2]-rnge[1]) # range index
  # print(srng)
  # ybreaks <- NULL
  # if (log10(rnge[2])-log10(rnge[2]-rnge[1])<0.73) { # <1.7) {
  #   log.range <- log10(rnge) #ci[,1]
  #   lower <- 10^floor(log.range[1])
  #   upper <- 10^ceiling(log.range[2])
  #   cap <- lower
  #   while(cap < upper) {
  #     ybreaks <- c(ybreaks, seq(cap, cap*9, by = cap))
  #     cap <- cap * 10
  #   }    
  # }
  
  # now plot
  p <- ggplot(bwpeaks) + 
    geom_point(aes(x=PROB, y=Val, colour=year)) + 
    scale_colour_binned(type = "viridis") +
    theme_bw() + theme(panel.grid.major = element_line(colour = "#808080"), 
                       panel.grid.minor = element_line(colour = "#808080"),
                       legend.justification = lorg, legend.position = lpos) +
    scale_y_continuous(trans="log10", name=ylab) +
    scale_x_continuous(trans=probability_trans(distribution="norm"),
                       breaks=xbreaks, labels=signif(prob2T(xbreaks), digits=3),
                       name="Return period (years)") +
    geom_line(data=ci, aes(x=nep, y=true), color="red") +
    geom_line(data=ci, aes(x=nep, y=lower), color="red", lty=2) +
    geom_line(data=ci, aes(x=nep, y=upper), color="red", lty=2) + 
    ggtitle(v$title)
  
  # if(!is.null(ybreaks)) {
  #   p <- p + scale_y_continuous(trans="log10", name=ylab, breaks=ybreaks)
  # } else {
  #   p <- p + ylab(ylab)
  # }
  # if(!is.null(title)) p <- p + ggtitle(title)
  
  return(p)
}


########################################################
# frequency of annual extremes
########################################################
extreme_frequency <- function(hds, xlab, dist='lp3', n = 2.5E4, ci = 0.90, ismn=FALSE) {
  hds$yr <- as.numeric(format(hds$Date, "%Y"))
  if (ismn) {
    agg <- aggregate(Val ~ yr, hds, min)
  } else {
    agg <- aggregate(Val ~ yr, hds, max) 
  }
  input_data <- agg[,2]
  
  if (length(input_data)<5) {
    p <- ggplot() + theme_void() + xlim(0, 10) + ylim(0, 100) + annotate("text", x=1, y=85, hjust=0, size=6,
                                                                         label = "WARNING: Annual extreme statistics requires the selected data to have measurements in at least 5 years")
    return(p)
  } else {
    ci <- BootstrapCI(series=input_data, # input data
                      distribution=dist, # distribution
                      n.resamples = n,   # number of re-samples to conduct
                      ci = ci)           # confidence interval level
    
    # generate frequency plot
    return(frequencyPlot(input_data, agg[,1], ci$ci, xlab, inverted=ismn))    
  }
}

extreme_density <- function(hds, xlab, ismn=FALSE){
  hds$yr <- as.numeric(format(hds$Date, "%Y"))
  if (ismn) {
    df <- data.frame(peak=aggregate(Val ~ yr, hds, min)[,2])
  } else {
    df <- data.frame(peak=aggregate(Val ~ yr, hds, max)[,2]) 
  }
  
  p <- ggplot(df,aes(peak)) +
    theme_bw() +
    geom_density(colour='blue', size=1, fill='blue', alpha=0.2) +
    geom_rug() +
    labs(x=xlab, title=NULL)
  
  return(p)
}

extreme_histogram <- function(hds, ismn=FALSE){
  hds$yr <- as.numeric(format(hds$Date, "%Y"))
  
  if (ismn) {
    df <- hds %>% 
      dplyr::select(c(Date,yr,Val)) %>%
      drop_na() %>%
      group_by(yr) %>% 
      summarise(v = min(Val,na.rm=TRUE), date = Date[which.min(Val)]) %>%
      ungroup()
  } else {
    df <- hds %>% 
      dplyr::select(c(Date,yr,Val)) %>%
      drop_na() %>%
      group_by(yr) %>% 
      summarise(v = max(Val,na.rm=TRUE), date = Date[which.max(Val)]) %>%
      ungroup() 
  }
  
  df$mo <- as.numeric(format(df$date, "%m"))
  df$mnt <- format(df$date, "%b")
  df$mnt <- ordered(df$mnt, levels = c('Oct','Nov','Dec','Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep'))
  
  p <- ggplot(df,aes(mnt)) +
    theme_bw() + theme(panel.grid.major = element_line(colour = "#808080"), panel.grid.minor = element_line(colour = "#808080")) +
    geom_histogram(stat='count') +
    scale_x_discrete(drop = FALSE) +
    labs(x=NULL, title=NULL)
  
  return(p)
}


######################
### plots
######################
output$ax.h <- renderPlot({
  req(xl <- input$radio.ax)
  req(iid <- input$int.ax)
  input$ax.regen
  isolate({
    if (!is.null(v$df)){
      xs <- as.character(xr.Nshrt[xl])
      xlab <- paste0(xr.NLong[[xs]],'\nfrequency of annual extremes')
      
      df1 <- remove.outliers(v$df[v$df$RDNC==xs & v$df$IID==iid,])
      
      ismn <- input$ax.mnmx=='min'
      mdl <- input$ax.freq
      nrsm <- input$ax.rsmpl
      ci <- input$ax.ci
      withProgress(message = 'rendering plots..', value = 0.1, {extreme_frequency(df1, xlab, mdl, nrsm, ci, ismn)})
    }
  })
})

output$ax.dist <- renderPlot({
  req(xl <- input$radio.ax)
  req(iid <- input$int.ax)
  input$ax.regen
  isolate(
    if (!is.null(v$df$orig)){
      xs <- as.character(xr.Nshrt[xl])
      xlab <- paste0(xr.NLong[[xs]],'\ndistribution of annual extremes')
      
      df1 <- remove.outliers(v$df[v$df$RDNC==xs & v$df$IID==iid,])
      
      ismn <- input$ax.mnmx=='min'
      withProgress(message = 'rendering distribution..', value = 0.8, {extreme_density(df1, xlab, ismn)})
    }
  )
})

output$ax.hist <- renderPlot({
  req(input$radio.ax)
  input$ax.regen
  isolate(
    if (!is.null(v$df$orig)){
      xl <- input$radio.ax
      xs <- as.character(xr.Nshrt[xl])
      ismn <- input$ax.mnmx=='min'
      withProgress(message = 'rendering distribution..', value = 0.8, {extreme_histogram(remove.outliers(v$df[v$df$RDNC==xs & v$df$IID==iid,]), ismn)})
    }
  )
})