
# https://stackoverflow.com/questions/35806310/ggplot-plotting-layers-only-if-certain-criteria-are-met
pick <- function(condition){ function(d) d %>% filter(!!enquo(condition)) }

output$plt.print <- renderPlot({
  req(rng <- input$dt.rng)
  req(xl <- input$chkData)
  if (!is.null(v$df)){
    withProgress(message = 'building plot..', value = 0.1, {
      xs <- as.character(xr.Nshrt[xl])
      v$df[v$df$RDNC %in% xs & v$df$IID %in% input$pck.raw,] %>% 
        subset( Date >= rng[[1]]  &  Date <= rng[[2]] ) %>%
        ggplot(aes(Date,Val)) +
        theme_bw() + theme(
          axis.title=element_blank(),
          legend.title=element_blank(),
          strip.placement = "outside",
          strip.background = element_blank()
        ) +
        geom_step(data = pick(grp == "Temperature (°C)"), aes(colour=IID)) +
        geom_line(data = pick(RDNC == "WtrLvl"),aes(colour=IID)) +
        geom_line(data = pick(grp == "Atmospheric Pressure (kPa)"),aes(colour=IID)) +
        geom_area(data = pick(RDNC == "PackDepth"),aes(colour=IID,fill=RDNC)) +
        geom_line(data = pick(grp == "Stream flow (m³/s)"), aes(colour=IID)) +
        geom_point(data = pick(RDNC == "WtrLvl.s"), aes(colour=IID)) +
        geom_col(data = pick(grp == "Precipitation (mm)"), aes(fill=RDNC), position=position_stack()) +
        geom_col(data = pick(grp == "Production (m³/d)"), aes(colour=IID,fill=RDNC), position=position_dodge()) +
        scale_fill_manual(breaks = c("PackDepth", "Precip", "Rain", "iRainfall", "Snow", "iSnowmelt", "Pump"), 
                          values=c("#b2b2b2b2", "#33a02c", "#1f78b4", "#1f78b4", "#b2df8a", "#a6cee3", "#fb9a99")) +
        facet_wrap(~grp, ncol=1, scales = "free_y", strip.position = "left")
    })
  }
})
