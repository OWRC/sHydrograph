

observe(updateDateRangeInput(session, "tab.rng", start = v$DTb, end = v$DTe, min = v$DTb, max = v$DTe))

observe({
  x <- unname(unlist(v$nam))
  updatePickerInput(session,"pck.tab", choices = x, selected = x)
})

observe({
  x <- unname(xr.NLong[unique(v$df[v$df$IID %in% input$pck.raw,]$RDNC)])
  updateCheckboxGroupInput(session, "chk.tab", choices=x, select=x) #tail(x,1))
})

########################################################
#### data table
########################################################
df.filtered <- reactive({
  req(xl <- input$chk.tab)
  req(s <- input$tab.spread)
  req(rng <- input$tab.rng)
  req(iids <- input$pck.tab) 
  xs <- as.character(xr.Nshrt[xl])
  if (s == 1) {
    # v$raw[v$raw$Date >= rng[[1]] & v$raw$Date <= rng[[2]] & v$raw$IID %in% iids,] %>%
    #   dplyr::select(-one_of(c('RDTC','grp')))
    v$raw[v$raw$RDNC %in% xs & v$raw$IID %in% iids,] %>%
      mutate(RDNC = xr.NLong[as.character(RDNC)]) %>%
      dplyr::select(-one_of(c('RDTC','grp'))) 
  } else if (s == 2) { # spread data
    showNotification("rendering..")
    
    # # grab climate data
    # mdf <- v$raw[v$raw$RDNC %in% c('Rainfall','Snowmelt'),] 
    # if ( nrow(mdf)>0 ) {
    #   mdf <- mdf %>%
    #     dplyr::select(-one_of(c('IID','RDTC','unit','grp'))) %>%
    #     spread(key=RDNC, value=Val)
    # }

    sdf <- v$raw[v$raw$RDNC %in% xs & v$raw$IID %in% iids,] %>%
      mutate(RDNC = xr.NLong[as.character(RDNC)]) %>%
      dplyr::select(-one_of(c('RDTC','unit','grp'))) %>%
      group_by_at(vars(-Val)) %>%  # group by everything other than the value column. (from: https://github.com/tidyverse/tidyr/issues/426)
      mutate(row_id=1:n()) %>% ungroup() %>% # build group index (from: https://github.com/tidyverse/tidyr/issues/426)
      spread(key=RDNC, value=Val) %>%
      dplyr::select(-one_of(c('row_id'))) #,'Rainfall','Snowmelt')))

    # combine IID and Variable
    if ( length(unique(v$raw$IID)) > 1 ) {
      sdf <- sdf %>%
        gather(variable, value, -(Date:IID)) %>%
        unite(temp, IID, variable) %>%
        group_by(Date, temp) %>%
        dplyr::summarise(value = mean(value)) %>% # grouping and summarizing needed to remove duplicate rows
        ungroup() %>%
        spread(temp, value) %>%
        dplyr::select(where(~!all(is.na(.x)))) # remove all-NA columns      
    }

    # # reintroduce climate
    # if ( nrow(mdf)>0 ) {
    #   return(sdf %>% inner_join(mdf))
    # } else {
      return(sdf)
    # }
  } else if (s == 3) { # spread daily table
    sdf <- v$df[v$df$RDNC %in% xs & v$df$IID %in% c(iids, 'interpolated'),] %>%
      mutate(RDNC = xr.NLong[as.character(RDNC)]) %>%
      dplyr::select(-one_of(c('RDTC','unit','grp'))) %>%
      group_by_at(vars(-Val)) %>%  # group by everything other than the value column. (from: https://github.com/tidyverse/tidyr/issues/426)
      mutate(row_id=1:n()) %>% ungroup() %>% # build group index (from: https://github.com/tidyverse/tidyr/issues/426)
      spread(key=RDNC, value=Val) %>%
      dplyr::select(-one_of(c('row_id'))) #,'Rainfall','Snowmelt')))
    
    # combine IID and Variable
    if ( length(unique(v$df$IID)) > 1 ) {
      sdf <- sdf %>%
        gather(variable, value, -(Date:IID)) %>%
        unite(temp, IID, variable) %>%
        group_by(Date, temp) %>%
        dplyr::summarise(value = mean(value)) %>% # grouping and summarizing needed to remove duplicate rows
        ungroup() %>%
        spread(temp, value) %>%
        dplyr::select(where(~!all(is.na(.x)))) # remove all-NA columns      
    }    
  }
})

# df.spread <- function() {
#   # grab climate data
#   print('here')
#   mdf <- v$df[v$df$RDNC %in% c('Rainfall','Snowmelt'),] %>%
#     dplyr::select(-one_of(c('IID','RDTC','unit','grp'))) %>%
#     spread(key=RDNC, value=Val)
#   print('here')
#   print(mdf)
#   sdf <- v$df[v$df$RDNC %in% xs  &  v$df$IID %in% iids,] %>%
#     dplyr::select(-one_of(c('RDTC','unit','grp'))) %>%
#     group_by_at(vars(-Val)) %>%  # group by everything other than the value column. (from: https://github.com/tidyverse/tidyr/issues/426)
#     mutate(row_id=1:n()) %>% ungroup() %>% # build group index (from: https://github.com/tidyverse/tidyr/issues/426)
#     spread(key=RDNC, value=Val) %>%
#     dplyr::select(-one_of(c('row_id','Rainfall','Snowmelt')))
#     
#   # combine IID and Variable
#   if ( length(unique(v$df$IID)) > 1 ) {
#     sdf <- sdf %>%
#       gather(variable, value, -(Date:IID)) %>%
#       unite(temp, IID, variable) %>%
#       group_by(Date, temp) %>%
#       dplyr::summarise(value = mean(value)) %>% # grouping and summarizing needed to remove duplicate rows
#       ungroup() %>%
#       spread(temp, value) %>%
#       dplyr::select(where(~!all(is.na(.x)))) # remove all-NA columns      
#   }
#     
#   # reintroduce climate
#   sdf %>% inner_join(mdf)
# }


output$tabiids <- renderDataTable(
  {if (!is.null(v$meta)){
    v$meta #%>%
      # formatPercentage('Quality', 0) %>%
      # formatRound(c('latitude', 'longitude'), 3) %>%
      # formatRound('DrainageArea',1)
  }},
  options = list(scrollY='100%', scrollX=TRUE,
                 lengthChange=FALSE,
                 pageLength = 5,
                 searching=FALSE)
)

output$tabts <- renderDataTable(
  { if (!is.null(v$raw)) df.filtered() }, 
  options = list(scrollY='100%', scrollX=TRUE,
                 lengthMenu = c(5, 30, 100, 365, 3652),
                 pageLength = 30,
                 searching=FALSE)
)

output$tabCsv <- downloadHandler(
  filename <- function() { paste0(path_sanitize(v$title), '.csv') },
  content <- function(file) {
    if (!is.null(v$raw)){ write.csv(df.filtered(), file, row.names = FALSE, na = "") }
  }
)