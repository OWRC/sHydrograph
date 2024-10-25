

observe({
  x <- unname(xr.NLong[unique(v$df[v$df$IID %in% input$pck.raw,]$RDNC)])
  updateCheckboxGroupInput(session, "chk.cdf", choices=x, select=x) #tail(x,1))
})

observe({
  x <- unique(v$df$IID)
  updatePickerInput(session,"pck.cdf", choices = x, selected = x)
})



output$plt.cdf <- renderPlot({
  req(xl <- input$chk.cdf)
  req(iids <- input$pck.cdf)
  if (!is.null(v$df)){
    withProgress(message = 'building plot..', value = 0.1, {
      xs <- as.character(xr.Nshrt[xl])
      p <- v$df[v$df$RDNC %in% xs & v$df$IID %in% iids & v$df$Val != 0,] %>% 
        mutate(RDNC=xr.NLong[RDNC]) %>%
        ggplot(aes(Val, color=IID, shape=RDNC)) + 
        theme_bw() + theme_bw() + theme(
          axis.title=element_blank(),
          legend.title=element_blank(),
          strip.placement = "outside",
          strip.background = element_blank()
        ) +
        geom_point(stat = "ecdf") 
      
      if (input$chklog.cdf) p <- p + scale_x_log10(
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))
      )
      
      p + facet_wrap(~grp, ncol=2, scales = "free", strip.position = "left") + coord_flip()
    })
  }
}, res=ggres)