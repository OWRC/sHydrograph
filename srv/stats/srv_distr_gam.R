
observe({
  req(i <- input$int.distr.gam)
  x <- unname(xr.NLong[unique(v$df[v$df$IID==i,]$RDNC)])
  updateRadioButtons(session, "radio.distr.gam", choiceNames=x, choiceValues=x)
})

observe({
  typs <- unique(v$df$IID)
  updateSelectInput(session,"int.distr.gam", choices = typs, selected = typs)
})


applyGAM <- function(df,k,asPoints=FALSE){
  
  
  p <- df %>%
    mutate(doy=as.numeric(strftime(Date, format="%j"))) %>%
    dplyr::select(doy,Val) %>%
    drop_na() %>% 
    GAM(k=k)

  
  df <- df %>% 
    dplyr::select(c(Date,Val)) %>%
    drop_na() %>%        
    mutate(doy=as.numeric(strftime(Date, format="%j")),
           year=as.factor(strftime(Date, format="%Y")),
           dateday=as.Date(doy, origin = "2016-01-01"))

  # p + geom_line(data=df, aes(dateday,Val,group=year,color=year)) +
  #   coord_cartesian(xlim=as.Date(c('2016-01-01','2016-12-31')))
  
  if (asPoints) {
    df[df==0] <- NA
    p <- p + geom_point(data=df,aes(dateday,Val,group=year,color=year),size=1, position = "jitter")
  } else {
    p <- p + geom_line(data=df, aes(dateday,Val,group=year,color=year))
  }
  p + coord_cartesian(xlim=as.Date(c('2016-01-01','2016-12-31')))
}

gghighlow <- function(df1) {
  xl <- input$radio.distr.gam
  xs <- as.character(xr.Nshrt[xl])
  ylab <- paste0(xr.NLong[[xs]],'\npeak density')
  
  dfhl <- df1 %>%
    drop_na() %>%
    mutate(year = year(Date), doy = yday(Date)) %>%
    group_by(year) %>%
    dplyr::summarise(low = doy[which.min(Val)], high = doy[which.max(Val)]) %>% 
    slice(rep(row_number(), 3))
  
  dfhl <- melt(dfhl[,c('year','low','high')],id.vars = 1)
  n <- nrow(dfhl)/6
  dfhl[1:n,]$value <- dfhl[1:n,]$value - 365
  dfhl[(2*n+1):(3*n),]$value <- dfhl[(2*n+1):(3*n),]$value + 365
  dfhl[(3*n+1):(3*n+n),]$value <- dfhl[(3*n+1):(3*n+n),]$value - 365
  dfhl[(3*n+2*n+1):(3*n+3*n),]$value <- dfhl[(3*n+2*n+1):(3*n+3*n),]$value + 365
  
  dfhl$value <- as.Date(dfhl$value, origin = "2016-01-01")
  
  ggplot(dfhl, aes(value,fill=variable,colour=variable)) +
    theme_bw() + 
    theme(text = element_text(size = 15),
          axis.ticks.length.x = unit(0.5, "cm"),
          axis.text.x = element_text(vjust = 5.5, hjust = -0.2),
          axis.title.x = element_blank(),
          axis.text.y = element_blank()) +
    geom_density(alpha=.3,adjust=1/6) +
    geom_rug(length = unit(0.1, "npc"), size=1, na.rm=TRUE) +
    labs(fill = "annual", colour = "annual") +
    scale_x_date(date_labels = "%b", date_minor_breaks = "1 month") +
    # scale_x_date(breaks = date_breaks("months"),labels = date_format("%b"),name=element_blank()) +
    coord_cartesian(xlim=as.Date(c('2016-01-01','2016-12-31')))
}


output$distr.gam <- renderPlot({
  req(xl <- input$radio.distr.gam)
  req(iid <- input$int.distr.gam)
  req(k <- input$distr.gam.k)
  if (!is.null(v$df)) {
    xs <- as.character(xr.Nshrt[xl])
    df <- remove.outliers(v$df[v$df$RDNC==xs & v$df$IID==iid,])
    if ( nrow(df)<=12 ) {
      ggplot() + 
        annotate("text", x = as.Date('2016-07-01'), y = 0, size=6, label = 'Not enough data, please select another parameter') + 
        theme_void() 
    } else {
      plts <- list( applyGAM(df, k) + labs(title=iid,y=xl), gghighlow(df) )
      cowplot::plot_grid(plotlist=plts, ncol=1, align='v', rel_heights = c(5,2))      
    }
  }
})


# output$distr.naive <- renderPlot({
#   req(input$radio.distr.gam)
#   if (!is.null(v$df$orig)) {
#     xl <- input$radio.distr.gam
#     xs <- as.character(xr.Nshrt[xl])
#     k <- input$distr.gam.k # rolling mean length (days)
#     chkP <- input$distr.gam.pnts
#     
#     # (naive) point wise distribution
#     p <- v$df$plt %>%
#       dplyr::select(c(Date,!!ensym(xs))) %>%
#       drop_na() %>%
#       mutate(meanVal = rollmean(x=!!ensym(xs), k, fill = NA),
#              doy=as.numeric(strftime(Date, format="%j")),
#              year=as.factor(strftime(Date, format="%Y")),
#              date2 = as.Date(doy, origin = "2016-01-01")) %>%
#       
#       ggplot(aes(date2,!!ensym(xs),group=1)) +
#       theme_bw() + #theme(panel.grid.major = element_line(colour = "#808080"), panel.grid.minor = element_line(colour = "#808080")) +
#       theme(axis.title.x = element_blank()) +
#       theme(text = element_text(size = 15),
#             axis.ticks.length.x = unit(0.5, "cm"),
#             axis.text.x = element_text(vjust = 5.5, hjust = -0.2)) +
#       
#       stat_summary(aes(y=meanVal), fun.data = 'mean_sdl',
#                    fun.args = list(mult = 2),
#                    geom = 'smooth', se = TRUE, fill='#f1a340',alpha=.75) +
#       stat_summary(aes(y=meanVal), fun.data = 'mean_sdl',
#                    fun.args = list(mult = 1),
#                    geom = 'smooth', se = TRUE, color='#f7f7f7', fill='#998ec3',alpha=.75) +
#       
#       # geom_smooth(method = "gam", formula = y ~s(x, bs = "ps"), na.rm=TRUE,fill='black',level=.95)  +
#       
#       scale_x_date(date_labels = "%b", date_minor_breaks = "1 month") +
#       labs(title=v$title,y=xl)
#     
#     applyColour(chkP,p,xs)
#   }
# })