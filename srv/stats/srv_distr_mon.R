
observe({
  x <- unname(xr.NLong[colnames(v$df$plt[-c(1)])])
  updateRadioButtons(session, "radio.distr.m", choiceNames=x, choiceValues=x)
})


output$distr.m.h <- renderPlot({
  req(input$radio.distr.m)
  if (!is.null(v$df$orig)){
    xl <- input$radio.distr.m
    xs <- as.character(xr.Nshrt[xl])
    
    # https://www.datanovia.com/en/blog/elegant-visualization-of-density-distribution-in-r-using-ridgeline/
    v$df$plt %>%
      select(c(Date,!!ensym(xs))) %>%
      drop_na() %>%
      mutate(smonth=factor(strftime(Date, format="%b"),levels=rev(c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')))) %>%
      
      # ggplot(aes(x=!!ensym(xs),y=smonth, fill = stat(x))) +
      #   theme_bw() + #theme(panel.grid.major = element_line(colour = "#808080"), panel.grid.minor = element_line(colour = "#808080")) +
      #   theme(axis.title.y = element_blank()) +
      #   geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01, quantile_lines = TRUE) +
      #   scale_fill_viridis_c(name = "Water Level", option = "C") +
      #   # coord_flip() +
      #   labs(title = 'Water levels')
      
      ggplot(aes(x=!!ensym(xs),y=smonth, fill = factor(stat(quantile)))) +
        theme_bw() + #theme(panel.grid.major = element_line(colour = "#808080"), panel.grid.minor = element_line(colour = "#808080")) +
        theme(axis.title.y = element_blank(), legend.position = "none") +
        stat_density_ridges(
          geom = "density_ridges_gradient",
          calc_ecdf = TRUE, rel_min_height = 0.01,
          quantiles = c(0.025, 0.5, 0.975),
          quantile_lines = TRUE
        ) +
        scale_fill_manual(
          name = "Probability", values = c("#FF0000A0", "#A0A0A0A0", "#A0A0A0A0", "#0000FFA0"),
          labels = c("(0, 0.025]", "(0.025, 0.5]", "(0.5, 0.975]", "(0.975, 1]")
        ) +
        labs(title=v$title,x=xl) 
  }
})