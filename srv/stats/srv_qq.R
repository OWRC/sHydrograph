
observe({
  x <- unname(unlist(v$typs)) #unname(xr.NLong[colnames(v$df[-c(1)])])
  updateRadioButtons(session, "radio.qq", choiceNames=x, choiceValues=x)
})

observe({
  switch (input$freq.qq,
    case = action
  )
})

qdistr <- reactive({
  switch (input$freq.qq,
          'Exponential' = stats::qexp,
          'Uniform' = stats::qunif,
          'Normal' = stats::qnorm
  )
})

ddistr <- reactive({
  switch (input$freq.qq,
          'Exponential' = stats::dexp,
          'Uniform' = stats::dunif,
          'Normal' = stats::dnorm
  )
})

# qq.info <- reactive(info=NULL)
qq.info <- reactiveVal('')
output$info.qq <- renderUI(
    shiny::HTML(paste0(
      '<body>',
      qq.info(),
      '</div></body>'
    ))
)

prettyFmt <- function(f,sf=5) { formatC(signif(f,digits=sf), digits=sf, format="fg", flag="#") }


output$distr.qq.distr <- renderPlot({
  req(xl <- input$radio.qq)
  if (!is.null(v$df$plt)){
    xs <- as.character(xr.Nshrt[xl])
    m <- v$df$plt %>% dplyr::select(!!ensym(xs))
    
    p <- v$df$plt %>%
      ggplot(aes(!!ensym(xs))) +
      theme_bw() +
      geom_density(size=1)
    
    if ( input$freq.qq=='Exponential' ) {
      fit1 <- MASS::fitdistr(as.numeric(unlist(m[[1]])), "exponential") 
      qq.info(paste0('<b>Exponential fit</b></div><div> rate = ', prettyFmt(fit1$estimate)))
      args <- list(rate = fit1$estimate)
    } else if ( input$freq.qq=='Normal' ) {
      qq.info(paste0('<b>Gaussian fit</b></div><div> mean = ', prettyFmt(mean(m[[1]], na.rm = TRUE)), '</div><div> standard deviation = ', prettyFmt(sd(m[[1]], na.rm = TRUE))))
      args <- list(mean = mean(m[[1]], na.rm = TRUE), sd = sd(m[[1]], na.rm = TRUE))
    } else if ( input$freq.qq=='Uniform' ) {
      qq.info(paste0('<b>Uniform fit</b></div><div> mean = ', prettyFmt(mean(m[[1]], na.rm = TRUE))))
      args <- list(min = min(m[[1]], na.rm = TRUE), max = max(m[[1]], na.rm = TRUE))
    }
    
    p + stat_function(fun = ddistr(), args = args, color='red') +
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