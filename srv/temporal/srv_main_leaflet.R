

points <- eventReactive(input$recalc, {
  if (!is.null(v$meta)) cbind(v$meta$LONG, v$meta$LAT)
}, ignoreNULL = FALSE)


output$main.map <- renderLeaflet({
  if (!is.null(v$meta)) {
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = points())    
  }
})