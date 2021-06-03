
observe({
  x <- unname(xr.NLong[colnames(v$df$plt[-c(1)])])
  updateRadioButtons(session, "radio.distr.gam", choiceNames=x, choiceValues=x)
})


applyColour <- function(chkP,p,xs){
  df <- v$df$plt %>% 
    select(c(Date,!!ensym(xs))) %>%
    drop_na() %>%        
    mutate(doy=as.numeric(strftime(Date, format="%j")),
           year=as.factor(strftime(Date, format="%Y")),
           dateday=as.Date(doy, origin = "2016-01-01"))
  
  if (chkP) {
    p + geom_line(data=df, aes(dateday,!!ensym(xs),group=year,color=year))
  } else {
    p + geom_point(data=df,aes(dateday,!!ensym(xs)),size=1, position = "jitter", alpha=0.2)
  }
}


output$distr.naive <- renderPlot({
  req(input$radio.distr.gam)
  if (!is.null(v$df$orig)) {
    xl <- input$radio.distr.gam
    xs <- as.character(xr.Nshrt[xl])
    k <- input$distr.gam.k # rolling mean length (days)
    chkP <- input$distr.gam.pnts
    
    # (naive) point wise distribution
    p <- v$df$plt %>%
      select(c(Date,!!ensym(xs))) %>%
      drop_na() %>%
      mutate(meanVal = rollmean(x=!!ensym(xs), k, fill = NA),
             doy=as.numeric(strftime(Date, format="%j")),
             year=as.factor(strftime(Date, format="%Y")),
             date2 = as.Date(doy, origin = "2016-01-01")) %>%
      
      ggplot(aes(date2,!!ensym(xs),group=1)) +
      theme_bw() + #theme(panel.grid.major = element_line(colour = "#808080"), panel.grid.minor = element_line(colour = "#808080")) +
      theme(axis.title.x = element_blank()) +
      theme(text = element_text(size = 15),
            axis.ticks.length.x = unit(0.5, "cm"),
            axis.text.x = element_text(vjust = 5.5, hjust = -0.2)) +
      
      stat_summary(aes(y=meanVal), fun.data = 'mean_sdl',
                   fun.args = list(mult = 2),
                   geom = 'smooth', se = TRUE, fill='#f1a340',alpha=.75) +
      stat_summary(aes(y=meanVal), fun.data = 'mean_sdl',
                   fun.args = list(mult = 1),
                   geom = 'smooth', se = TRUE, color='#f7f7f7', fill='#998ec3',alpha=.75) +
      
      # geom_smooth(method = "gam", formula = y ~s(x, bs = "ps"), na.rm=TRUE,fill='black',level=.95)  +
      
      scale_x_date(date_labels = "%b", date_minor_breaks = "1 month") +
      labs(title=v$title,y=xl)
    
    applyColour(chkP,p,xs)
  }
})

output$distr.gam <- renderPlot({
  req(input$radio.distr.gam)
  if (!is.null(v$df$orig)) {
    xl <- input$radio.distr.gam
    xs <- as.character(xr.Nshrt[xl])
    k <- input$distr.gam.k
    chkP <- input$distr.gam.pnts
    
    p <- v$df$plt %>%
      mutate(doy=as.numeric(strftime(Date, format="%j")), val=!!ensym(xs)) %>%
      select(doy,val) %>%
      GAM(k=k) + labs(title=v$title,y=xl)
    
    applyColour(chkP,p,xs)
  }
})