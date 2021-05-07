
observe({
  x <- unname(xr.NLong[colnames(v$df$plt[-c(1)])])
  updateRadioButtons(session, "radio.distr.d", choiceNames=x, choiceValues=x)
})


output$distr.d.h <- renderPlot({
  req(input$radio.distr.d)
  if (!is.null(v$df$orig)){
    xl <- input$radio.distr.d
    xs <- as.character(xr.Nshrt[xl])
    
    # https://www.datanovia.com/en/blog/elegant-visualization-of-density-distribution-in-r-using-ridgeline/
    v$df$plt %>%
      select(c(Date,!!ensym(xs))) %>%
      drop_na() %>%
      mutate(meanVal = rollmean(x=!!ensym(xs), 30, fill = NA),
             doy=as.numeric(strftime(Date, format="%j")),
             date2 = as.Date(doy, origin = "2016-01-01")) %>%
      
      ggplot(aes(date2,!!ensym(xs),group=1)) +
        theme_bw() + #theme(panel.grid.major = element_line(colour = "#808080"), panel.grid.minor = element_line(colour = "#808080")) +
        theme(text = element_text(size = 15),
              axis.ticks.length.x = unit(0.5, "cm"),
              axis.text.x = element_text(vjust = 5.5, hjust = -0.2))+
        stat_summary(aes(y=meanVal), fun.data = 'mean_sdl',
                     fun.args = list(mult = 2),
                     geom = 'smooth', se = TRUE, fill='#f1a340',alpha=1) +
        stat_summary(aes(y=meanVal), fun.data = 'mean_sdl',
                     fun.args = list(mult = 1),
                     geom = 'smooth', se = TRUE, color='#f7f7f7', fill='#998ec3',alpha=1) +
        geom_point(size=1, position = "jitter", alpha=0.1) +
        geom_smooth(method = "gam", formula = y ~s(x, bs = "ps"), na.rm=TRUE,fill='black',level=.95)  +
        scale_x_date(date_labels = "%b", date_minor_breaks = "1 month")
    
    
    # # actual GAM
    # library(mgcv)
    # 
    # ggplot(tdf, aes(date2,RD_VALUE)) +
    #   geom_point(size=1, position = "jitter", alpha=0.005) + 
    #   geom_smooth(method = "gam", formula = y ~s(x, bs = "ps"), na.rm=TRUE,fill='black',level=.95) 
    
  }
})