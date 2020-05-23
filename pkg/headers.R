
## NOTE: headers must be given 1 per page/tab, or the app will hang ##

output$hdr.raw <- renderUI({shiny::HTML(paste0("<h1>",v$title,"</h1>"))})

output$hdr.an <- renderUI({shiny::HTML("<h1>Annual summary</h1>")})

output$hdr.hl <- renderUI({shiny::HTML("<h1>High-Low recurrence</h1>")})

output$hdr.ax <- renderUI({shiny::HTML("<h1>Annual extreme</h1>")})