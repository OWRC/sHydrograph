

crng.sca <- reactiveValues(x = NULL, y = NULL)

observeEvent(input$plt.sca_dblclick, {
  brush <- input$plt.sca_brush
  if (!is.null(brush)) {
    crng.sca$x <- c(brush$xmin, brush$xmax)
    crng.sca$y <- c(brush$ymin, brush$ymax)
    
  } else {
    crng.sca$x <- NULL
    crng.sca$y <- NULL
  }
})

observe({ updateDateRangeInput(session, "rng.sca", start = v$DTb, end = v$DTe, min = v$DTb, max = v$DTe) })

observe({
  if (!is.null(v$df)) {
    typs <- unique(v$df %>% arrange(RDNC,IID) %>% mutate(sel=paste0(IID,"-",RDNC)) %>% pull(sel))
    updateSelectInput(session,"cmbX.sca", choices = typs, selected = typs)
    updateSelectInput(session,"cmbY.sca", choices = typs, selected = typs)    
  }
})


output$plt.sca <- renderPlot ({
  xsel <- input$cmbX.sca
  ysel <- input$cmbY.sca
  df <- v$df %>% mutate(sel=paste0(IID,"-",RDNC))
  if (xsel==ysel) {
    p <- df[df$sel==xsel,] %>%
          spread(sel,Val) %>%
          ggplot(aes(!!ensym(xsel),!!ensym(ysel), color=year(Date))) +
            theme_bw() + theme(legend.justification = c(1, 0),legend.position = c(.99, .01), 
                  legend.title = element_blank(),
                  legend.background=element_rect(fill="transparent")) +
            scale_color_binned(type = "viridis") + 
            geom_point(size=3) +
            coord_cartesian(xlim = crng.sca$x, ylim = crng.sca$y, expand = FALSE) +
            ggtitle(v$title)
  } else {
    p <- df[df$sel==xsel | df$sel==ysel,] %>%
      # mutate(date=as.Date(SAMPLE_DATE), p2=paste0(PARAMETER," (",UNIT,")")) %>%
      # mutate(p2=paste0(PARAMETER," (",UNIT,")")) %>%
      dplyr::select(Date,Val,sel) %>%
      group_by(Date,sel) %>%
      mutate(Val=mean(Val)) %>%
      ungroup() %>%
      distinct() %>%
      spread(sel,Val) %>%
      ggplot(aes(!!ensym(xsel),!!ensym(ysel), color=year(Date))) +
        theme_bw() + theme(legend.justification = c(1, 0),legend.position = c(.99, .01), 
              legend.title = element_blank(),
              legend.background=element_rect(fill="transparent")) +
      scale_color_binned(type = "viridis") +       
      geom_point(size=3) +
            coord_cartesian(xlim = crng.sca$x, ylim = crng.sca$y, expand = FALSE) +
            ggtitle(v$title)
  }
  if (input$chk.11.sca) {
    p + geom_abline(slope=1,intercept=0,linetype='dotted')
  } else {
    p
  }
})
