
observe({
  x <- unname(xr.NLong[colnames(v$df$plt[-c(1)])])
  # ss <- substring(x,1,regexpr("\\([^\\(]*$", x)-1)
  updateRadioButtons(session, "radio.ax", choiceNames=x, choiceValues=x)
})


########################################################
# frequency of annual extremes
########################################################
extreme_frequency <- function(hds, xlab, dist='lp3', n = 2.5E4, ci = 0.90, ismn=FALSE, title=NULL) {
  hds$yr <- as.numeric(format(hds$Date, "%Y"))
  if (ismn) {
    input_data <- aggregate(Val ~ yr, hds, min)[,2]
  } else {
    input_data <- aggregate(Val ~ yr, hds, max)[,2]  
  }
  
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
    return(frequencyPlot(input_data, ci$ci, title, xlab, inverted=ismn))    
  }
}

extreme_histogram <- function(hds, xlab, ismn=FALSE, title=NULL){
  hds$yr <- as.numeric(format(hds$Date, "%Y"))
  if (ismn) {
    df <- data.frame(peak=aggregate(Val ~ yr, hds, min)[,2])
  } else {
    df <- data.frame(peak=aggregate(Val ~ yr, hds, max)[,2]) 
  }
  
  p <- ggplot(df,aes(peak)) +
    theme_bw() +
    geom_density(colour='blue', size=1, fill='blue', alpha=0.2) +
    labs(x=xlab, title=NULL)
  
  if(!is.null(title)) p <- p + ggtitle(title)
  
  return(p)
}


ax.screen <- function(df,col) {
  df1 <- df[which(df$RDNC == col),]
  # outlier removal
  med <- median(df1$Val)
  iqr <- IQR(df1$Val)*100
  df1$Val[df1$Val<(med-iqr)]=NA
  df1$Val[df1$Val>(med+iqr)]=NA
  return(df1[c('Date','Val')])
}


######################
### plots
######################
output$ax.h <- renderPlot({
  req(input$radio.ax)
  input$ax.regen
  isolate({
    if (!is.null(v$df$orig)){
      xl <- input$radio.ax
      xs <- as.character(xr.Nshrt[xl])
      xi <- as.numeric(xr.Nindx[xs])
      xlab <- paste0(xr.NLong[[xs]],'\nfrequency of annual extremes')
      df1 <- ax.screen(v$df$orig, xi)
      
      ismn <- input$ax.mnmx=='min'
      mdl <- input$ax.freq
      nrsm <- input$ax.rsmpl
      ci <- input$ax.ci
      withProgress(message = 'rendering plots..', value = 0.1, {extreme_frequency(df1, xlab, mdl, nrsm, ci, ismn)})
    }
  })
})

output$ax.dist <- renderPlot({
  req(input$radio.ax)
  input$ax.regen
  isolate(
    if (!is.null(v$df$orig)){
      xl <- input$radio.ax
      xs <- as.character(xr.Nshrt[xl])
      xi <- as.numeric(xr.Nindx[xs])
      xlab <- paste0(xr.NLong[[xs]],'\ndistribution of annual extremes')
      df1 <- ax.screen(v$df$orig, xi)
      
      ismn <- input$ax.mnmx=='min'
      withProgress(message = 'rendering distribution..', value = 0.8, {extreme_histogram(df1, xlab, ismn)})
    }
  )
})