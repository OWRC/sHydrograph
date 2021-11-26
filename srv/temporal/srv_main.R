
# observe({
#   input$mouseup
#   isolate({
#     if (!is.null(v$df)){
#       rng <- input$plt.raw_date_window # dummy variable used to trigger below
#     }
#   })
# })

observeEvent(input$plt.raw_date_window, { updated_date_window(input$plt.raw_date_window,"dt.rng") })

observeEvent(input$dt.rng, { updated_date_selector(input$dt.rng) })

observe({
  updateDateRangeInput(session, "dt.rng", start = v$DTb, end = v$DTe, min = v$DTb, max = v$DTe)
  r.rngselect = c(v$DTb, v$DTe)
})

observe({
  # x <- unname(unlist(v$typs))
  x <- unname(xr.NLong[unique(v$df[v$df$IID %in% input$pck.raw,]$RDNC)])
  s <- x[x != "Temperature (Water) - Logger (°C)" & x != "AirPressure¹ (kPa)"] # default layers to uncheck
  # if (anyNA(x)) { showNotification(paste0("unknown RDNC: ", paste(as.character(y[which(is.na(x))]), sep="' '", collapse=", ")), duration = 35) }
  updateCheckboxGroupInput(session, "chkData", choices=x, select=s) #tail(x,1))
  if (is.null(v$scrn)) hide("chkScrn")
})

observe({
  # x <- unname(unlist(v$nam))
  x <- unique(v$df$IID)
  updatePickerInput(session,"pck.raw", choices = x, selected = x)
})

output$info.main <- renderUI({
  req(rng <- input$dt.rng)
  DTb <- as.Date(strftime(rng[[1]], "%Y-%m-%d"))
  DTe <- as.Date(strftime(rng[[2]], "%Y-%m-%d"))
  isolate({
    por <- as.integer(difftime(DTe, DTb, units = "days"))
    shiny::HTML(paste0(
      '<body>',
      paste0(
        '<div><h4>Data summary:</h4></div>',
        strftime(DTb, "%b %Y"),' to ',strftime(DTe, "%b %Y"),' (',por+1,' days)</div>'
      ),
      '</body>'
    ))
  })
})

output$tabsum <- renderFormattable({
  req(rng <- input$dt.rng)
  req(xl <- input$chkData)
  req(iids <- input$pck.raw)
  if (!is.null(v$df)){
    xs <- as.character(xr.Nshrt[xl])
    v$df[v$df$RDNC %in% xs & v$df$Date >= rng[[1]] & v$df$Date <= rng[[2]] & v$df$IID %in% iids,] %>%
      dplyr::select(-one_of(c('RDTC','grp'))) %>%
      mutate(RDNC = xr.NLong[RDNC]) %>%
      group_by(IID,RDNC) %>%
      dplyr::summarise(mean = mean(Val,na.rm=TRUE), 
                       st.Dev = sd(Val,na.rm=TRUE), 
                       p5 = quantile(Val,.05,na.rm=TRUE), 
                       median = median(Val,na.rm=TRUE), 
                       p95 = quantile(Val,.95,na.rm=TRUE), 
                       n = sum(!is.na(Val)),
                       .groups = "keep") %>%
      ungroup() %>%
      mutate_at(vars(-c(IID,RDNC,n)), funs(round(., 3))) %>%
      formattable()
  }
})

