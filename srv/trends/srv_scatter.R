
crng.sc <- reactiveValues(x = NULL, y = NULL)

observeEvent(input$plt.sc_dblclick, {
  brush <- input$plt.sc_brush
  if (!is.null(brush)) {
    crng.sc$x <- c(brush$xmin, brush$xmax)
    crng.sc$y <- c(brush$ymin, brush$ymax)
  } else {
    crng.sc$x <- NULL
    crng.sc$y <- NULL
  }
})

observe({
  updateDateRangeInput(session, "rng.sc", start = v$DTb, end = v$DTe, min = v$DTb, max = v$DTe)
})

observe({
  if (!is.null(v$df$plt)){
    x <- unname(xr.NLong[colnames(v$df$plt[-c(1)])])
    updateSelectInput(session,"cmbX.sc", choices = x)
    updateSelectInput(session,"cmbY.sc", choices = x)
  }
})


output$plt.sc <- renderPlot ({
  if (!is.null(v$df$plt)) {
    xsel <- as.character(xr.Nshrt[input$cmbX.sc])
    ysel <- as.character(xr.Nshrt[input$cmbY.sc])
    v$df$plt %>%
      ggplot(aes(!!ensym(xsel),!!ensym(ysel), color=factor(year(Date)))) +
      # theme(legend.position="bottom") +
      scale_color_discrete(name = "Year") +
      geom_point(size=2) +
      coord_cartesian(xlim = crng.sc$x, ylim = crng.sc$y, expand = FALSE) +
      labs(title=v$title,x=input$cmbX.sc,y=input$cmbY.sc)
  }
})