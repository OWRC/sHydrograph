
observe({
  req(i <- input$int.distr.m)
  x <- unname(xr.NLong[unique(v$df[v$df$IID==i,]$RDNC)])
  updateRadioButtons(session, "radio.distr.m", choiceNames=x, choiceValues=x)
})

observe({
  typs <- unique(v$df$IID)
  updateSelectInput(session,"int.distr.m", choices = typs) #, selected = typs)
})


output$distr.m.h <- renderPlot({
  req(xl <- input$radio.distr.m)
  req(iid <- input$int.distr.m)
  if (!is.null(v$df)){
    xs <- as.character(xr.Nshrt[xl])
    
    # https://www.datanovia.com/en/blog/elegant-visualization-of-density-distribution-in-r-using-ridgeline/
    remove.outliers(v$df[v$df$RDNC==xs & v$df$IID==iid,]) %>%
      dplyr::select(c(Date,Val)) %>%
      drop_na() %>%
      mutate(smonth=factor(strftime(Date, format="%b"),levels=rev(c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')))) %>%
      
      # ggplot(aes(x=Val,y=smonth, fill = stat(x))) +
      #   theme_bw() + #theme(panel.grid.major = element_line(colour = "#808080"), panel.grid.minor = element_line(colour = "#808080")) +
      #   theme(axis.title.y = element_blank()) +
      #   geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01, quantile_lines = TRUE) +
      #   scale_fill_viridis_c(name = "Water Level", option = "C") +
      #   # coord_flip() +
      #   labs(title = 'Water levels')
      
      ggplot(aes(x=Val,y=smonth, fill = factor(stat(quantile)))) +
        theme_bw() + #theme(panel.grid.major = element_line(colour = "#808080"), panel.grid.minor = element_line(colour = "#808080")) +
        theme(axis.title.y = element_blank(), legend.position = "none") +
        stat_density_ridges(
          geom = "density_ridges_gradient",
          calc_ecdf = TRUE, rel_min_height = 0.01,
          quantiles = c(0.025, 0.5, 0.975),
          quantile_lines = TRUE
        ) +
        scale_fill_manual(
          name = "Probability", values = c("#FF0000A0", "#A0A0A0A0", "#A0A0A0A0", "#FF0000A0"),
          labels = c("(0, 0.025]", "(0.025, 0.5]", "(0.5, 0.975]", "(0.975, 1]")
        ) +
        labs(title=iid,x=xl) 
  }
}, res=ggres)