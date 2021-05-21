
observe({
  x <- unname(xr.NLong[colnames(v$df$plt[-c(1)])])
  updateRadioButtons(session, "radio.qq", choiceNames=x, choiceValues=x)
})

observe({
  switch (input$freq.qq,
    case = action
  )
})

qdistr <- reactive({
  switch (input$freq.qq,
    'Uniform' = stats::qunif,
    'Normal' = stats::qnorm
  )
})

ddistr <- reactive({
  switch (input$freq.qq,
          'Uniform' = stats::dunif,
          'Normal' = stats::dnorm
  )
})



output$distr.qq.distr <- renderPlot({
  req(input$radio.qq)
  if (!is.null(v$df$plt)){
    xl <- input$radio.qq
    xs <- as.character(xr.Nshrt[xl])
    m <- v$df$plt %>% select(!!ensym(xs))
    
    v$df$plt %>%
      ggplot(aes(!!ensym(xs))) +
      theme_bw() +
      geom_density(size=1) +
      stat_function(fun = ddistr(), args = list(mean = mean(m[[1]], na.rm = TRUE), sd = sd(m[[1]], na.rm = TRUE)), color='red') +
      geom_rug() +
      labs(title=v$title,x=xl)
  }
})

output$distr.qq <- renderPlot({
  req(input$radio.qq)
  if (!is.null(v$df$plt)){
    xl <- input$radio.qq
    xs <- as.character(xr.Nshrt[xl])
    
    ggplot(v$df$plt, aes(sample = !!ensym(xs))) +
      theme_bw() +
      stat_qq(distribution = qdistr()) + 
      stat_qq_line(distribution = qdistr()) +
      labs(title=paste0(v$title,": Q-Q plot"),y=xl)
  }
})