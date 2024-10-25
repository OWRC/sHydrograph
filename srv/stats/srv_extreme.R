
observe({
  req(i <- input$int.ax)
  x <- unname(xr.NLong[unique(v$df[v$df$IID==i,]$RDNC)])
  updateRadioButtons(session, "radio.ax", choiceNames=x, choiceValues=x)
})

observe({
  typs <- unique(v$df$IID)
  updateSelectInput(session,"int.ax", choices = typs) #, selected = typs)
})



########################################################
# frequency of annual extremes
########################################################
extreme_frequency <- function(hds, xlab, dist='lp3', n = 2.5E4, ci = 0.90, mnx='max') {
  hds$yr <- as.numeric(format(hds$Date, "%Y"))
  ismn <- FALSE
  if (mnx=='min') {
    agg <- aggregate(Val ~ yr, hds, min)
    ismn=TRUE
  } else if (mnx=='max') {
    agg <- aggregate(Val ~ yr, hds, max) 
  } else if (mnx=='mean') {
    agg <- aggregate(Val ~ yr, hds, mean) 
  } else{
    agg <- aggregate(Val ~ yr, hds, median) 
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
    
    if (is.null(ci)) {
      p <- ggplot() + theme_void() + xlim(0, 10) + ylim(0, 100) + annotate("text", x=1, y=85, hjust=0, size=6,
                                                                           label = "data invalid for plotting, please select another")
      return(p)      
    } else {
      # generate frequency plot
      return(frequencyPlot(input_data, agg[,1], ci$ci, xlab, inverted=ismn))       
    }
  }
}

extreme_density <- function(hds, xlab, mnx='max'){
  hds$yr <- as.numeric(format(hds$Date, "%Y"))
  if (mnx=='min') {
    df <- data.frame(peak=aggregate(Val ~ yr, hds, min)[,2])
  } else if (mnx=='max') {
    df <- data.frame(peak=aggregate(Val ~ yr, hds, max)[,2])
  } else if (mnx=='mean') {
    df <- data.frame(peak=aggregate(Val ~ yr, hds, mean)[,2])
  } else{
    df <- data.frame(peak=aggregate(Val ~ yr, hds, median)[,2]) 
  }
  
  p <- ggplot(df,aes(peak)) +
    theme_bw() + theme(legend.justification = c(1, 1),legend.position = c(.99, .99), 
                       legend.title = element_blank(),
                       legend.background=element_rect(fill="transparent")) + 
    geom_density(colour='blue', size=1, fill='blue', alpha=0.2) +
    geom_rug() +
    labs(x=xlab, title=NULL)
  
  return(p)
}

extreme_histogram <- function(hds, mnx='max'){
  hds$yr <- as.numeric(format(hds$Date, "%Y"))
  
  if (mnx=='min') {
    df <- hds %>% 
      dplyr::select(c(Date,yr,Val)) %>%
      drop_na() %>%
      group_by(yr) %>% 
      summarise(v = min(Val,na.rm=TRUE), date = Date[which.min(Val)]) %>%
      ungroup()
  } else if (mnx=='max') {
    df <- hds %>% 
      dplyr::select(c(Date,yr,Val)) %>%
      drop_na() %>%
      group_by(yr) %>% 
      summarise(v = max(Val,na.rm=TRUE), date = Date[which.max(Val)]) %>%
      ungroup() 
  } else {
    df <- hds %>% 
      dplyr::select(c(Date,yr,Val)) %>%
      drop_na() %>%
      group_by(yr) %>% 
      summarise(v = median(Val,na.rm=TRUE), date = Date[which.quantile(Val,.5,na.rm=TRUE)]) %>%
      ungroup() 
  }
  
  df$mo <- as.numeric(format(df$date, "%m"))
  df$mnt <- format(df$date, "%b")
  df$mnt <- ordered(df$mnt, levels = c('Oct','Nov','Dec','Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep'))
  
  ggplot(df,aes(mnt)) +
    theme_bw() + theme(panel.grid.major = element_line(colour = "#808080"), panel.grid.minor = element_line(colour = "#808080")) +
    geom_histogram(stat='count') +
    scale_x_discrete(drop = FALSE) +
    labs(x='monthly distribution of annual extremes', title=NULL)
}

extreme_rank <- function(df,iid,ylab, mnx='max'){
  df <- df %>% drop_na(Val) %>%
    mutate(year = year(Date)) %>%
    group_by(year) 
  
  if (mnx=='min') {
    df <- df %>% dplyr::summarise(stat = min(Val, na.rm = TRUE), n = sum(!is.na(Val))) %>% arrange(year)
    sttl <- 'ranked anomalies of extreme annual minima'
  } else if (mnx=='max') {
    df <- df %>% dplyr::summarise(stat = max(Val, na.rm = TRUE), n = sum(!is.na(Val))) %>% arrange(year)
    sttl <- 'ranked anomalies of extreme annual maxima'
  } else if (mnx=='mean') {
    df <- df %>% dplyr::summarise(stat = max(Val, na.rm = TRUE), n = sum(!is.na(Val))) %>% arrange(year)
    sttl <- 'ranked anomalies of annual means'
  } else{
    df <- df %>% dplyr::summarise(stat = max(Val, na.rm = TRUE), n = sum(!is.na(Val))) %>% arrange(year)
    sttl <- 'ranked anomalies of annual medians'
  }
  
  # Mann-Kendall test for trend
  MK = MannKendall(df$stat)
  if (MK$sl[1] < 0.001) {
    sttl <- paste0(sttl,' - Mann-Kendall tau: ',round(MK$tau[1],3),' (p < 0.001)')
  } else if (MK$sl[1] < 0.05) {
    trnd <- paste0(sttl,' - Mann-Kendall tau: ',round(MK$tau[1],3),' (p = ',round(MK$sl[1],3),')')
  } else {
    trnd <- paste0(sttl,' - no significant trend')
  }
  
  if (nrow(df[df$n==0,])>0) df[df$n==0,]$stat <- NA # clean missing
  df$stat <- df$stat - mean(df$stat, na.rm = TRUE)  # get anomalies      
  
  ggplot(df, aes(reorder(year, -stat),stat,fill=year)) +
    theme_bw() +
    theme(legend.justification = c(0, 0),legend.position = c(.01, .01), 
          legend.title = element_blank(),
          legend.background=element_rect(fill="transparent")) +
    theme(axis.text.x=element_blank(),axis.title.x=element_blank()) +
    geom_bar(stat="identity") + 
    geom_text(aes(label=year),angle = 90,hjust=1.1) +
    scale_fill_binned(type = "viridis") + 
    labs(y=ylab, title=iid, subtitle=sttl) +
    ylim(c(1.1*min(df$stat),NA))
}


######################
### plots
######################
output$ax.h <- renderPlot({
  req(xl <- input$radio.ax)
  req(iid <- input$int.ax)
  req(mnx <- input$ax.mnmx)
  req(mdl <- input$ax.freq)
  # input$ax.regen
  isolate({
    if (!is.null(v$df)){
      xs <- as.character(xr.Nshrt[xl])
      xlab <- paste0(xr.NLong[[xs]],'\nfrequency of annual extremes')
      # nrsm <- input$ax.rsmpl
      # ci <- input$ax.ci
      df <- remove.outliers(v$df[v$df$RDNC==xs & v$df$IID==iid,]) %>% drop_na(Val)
      withProgress(message = 'rendering plots..', value = 0.1, {extreme_frequency(df, xlab, dist=mdl, mnx=mnx) + ggtitle(iid)})
    }
  })
}, res=ggres)

output$ax.rnk <- renderPlot({
  req(xl <- input$radio.ax)
  req(iid <- input$int.ax)
  req(mnx <- input$ax.mnmx)
  # input$ax.regen
  withProgress(message = 'rendering rank..', value = 0.8, isolate(
    if (!is.null(v$df)){
      xs <- as.character(xr.Nshrt[xl])
      xlab <- paste0(xr.NLong[[xs]],'\nrank of annual extremes')
      extreme_rank(remove.outliers(v$df[v$df$RDNC==xs & v$df$IID==iid,]), iid, xlab, mnx=mnx)
    }
  ))
}, res=ggres)

output$ax.dist <- renderPlot({
  req(xl <- input$radio.ax)
  req(iid <- input$int.ax)
  req(mnx <- input$ax.mnmx)
  isolate(
    if (!is.null(v$df)){
      xs <- as.character(xr.Nshrt[xl])
      xlab <- paste0(xr.NLong[[xs]],'\ndistribution of annual extremes')
      withProgress(message = 'rendering distribution..', value = 0.8, {extreme_density(remove.outliers(v$df[v$df$RDNC==xs & v$df$IID==iid,]), xlab, mnx=mnx)})
    }
  )
}, res=ggres)

output$ax.hist <- renderPlot({
  req(iid <- input$int.ax)
  req(xl <- input$radio.ax)
  req(mnx <- input$ax.mnmx)
  # input$ax.regen
  isolate(
    if (!is.null(v$df)){
      xs <- as.character(xr.Nshrt[xl])
      # ismn <- input$ax.mnmx=='min'
      withProgress(message = 'rendering distribution..', value = 0.8, {extreme_histogram(remove.outliers(v$df[v$df$RDNC==xs & v$df$IID==iid,]), mnx=mnx)})
    }
  )
}, res=ggres)
