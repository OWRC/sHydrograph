
## NOTE: headers must be given 1 per page/tab, or the app will hang ##

output$hdr.raw <- renderUI({shiny::HTML(paste0("<h1>",v$title,"</h1>"))})

output$hdr.an <- renderUI({shiny::HTML("<h1>Annual summary</h1>")})

output$hdr.se <- renderUI({shiny::HTML("<h1>Seasonal summary</h1>")})

output$hdr.bko <- renderUI({shiny::HTML("<h1>Seasonal Decomposition</h1>")})

output$hdr.sca <- renderUI({shiny::HTML("<h1>Scatter plot</h1>")})

output$hdr.cdf <- renderUI({shiny::HTML("<h1>Cumulative Distribution Function</h1>")})

# output$hdr.hl <- renderUI({shiny::HTML("<h1>High-Low recurrence</h1>")})

output$hdr.ax <- renderUI({shiny::HTML("<h1>Annual extreme</h1>")})

output$hdr.distr.m <- renderUI({shiny::HTML("<h1>Monthly Densities</h1>")})

output$hdr.distr.gam <- renderUI({shiny::HTML("<h1>Seasonal variability/High-Low Occurence</h1>")})

output$hdr.qq <- renderUI({shiny::HTML("<h1>Quantile-Quantile plot</h1>")})

