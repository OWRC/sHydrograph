

points <- eventReactive(input$recalc, {
  if (!is.null(v$meta)) cbind(v$meta$LONG, v$meta$LAT)
}, ignoreNULL = FALSE)


output$main.map <- renderLeaflet({
  if (!is.null(v$meta)) {
    leaflet() %>%
      addTiles(attribution = '<a href="https://owrc.github.io/interpolants/#data-sources" target="_blank" rel="noopener noreferrer"><b>Source Data</b></a> © Oak Ridges Moraine Groundwater Program') %>%
      addTiles(group='OpenStreetMap') %>%
      addTiles("https://tile.oakridgeswater.ca/basemap/{z}/{x}/{y}", group = "ORMGP basemap", options = providerTileOptions(attribution=" © Oak Ridges Moraine Groundwater Programx", maxNativeZoom = 17)) %>%
      
      addLogo(
        img="ORMGP_logo_vsmall.png",
        src= "remote",
        position="bottomleft",
        offset.x = 10,
        offset.y = 10,
        width = 294
      ) %>%
      
      addMarkers(data = points()) %>%
      
      addLayersControl (
        overlayGroups = "ORMGP basemap",
        options = layersControlOptions(position = "topleft")
      )   
  }
})