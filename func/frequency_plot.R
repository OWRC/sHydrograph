
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
    geom_line(data=ci, aes(x=nep, y=upper), color="red", lty=2)
  
  # if(!is.null(ybreaks)) {
  #   p <- p + scale_y_continuous(trans="log10", name=ylab, breaks=ybreaks)
  # } else {
  #   p <- p + ylab(ylab)
  # }
  # if(!is.null(title)) p <- p + ggtitle(title)
  
  return(p)
}