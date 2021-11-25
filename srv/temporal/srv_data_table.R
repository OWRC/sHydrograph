

observe(updateDateRangeInput(session, "tab.rng", start = v$DTb, end = v$DTe, min = v$DTb, max = v$DTe))

observe({
  x <- unname(unlist(v$nam))
  updatePickerInput(session,"pck.tab", choices = x, selected = x)
})


########################################################
#### data table
########################################################
df.filtered <- reactive({
  req(s <- input$tabRad)
  rep(rng <- input$tab.rng)
  req(iids <- input$pck.tab)
  if (s == 1) {
    v$df[v$df$Date >= rng[[1]] & v$df$Date <= rng[[2]] & v$df$IID %in% iids,] %>%
      dplyr::select(-one_of(c('RDTC','grp')))
  } else if (s == 2) {
    
    # grab climate data
    mdf <- v$df[v$df$RDNC %in% c('Rainfall','Snowmelt'),] 
    if ( nrow(mdf)>0 ) {
      mdf <- mdf %>%
        dplyr::select(-one_of(c('IID','RDTC','unit','grp'))) %>%
        spread(key=RDNC, value=Val)  
    }

    sdf <- v$df %>%
      dplyr::select(-one_of(c('RDTC','unit','grp'))) %>%
      group_by_at(vars(-Val)) %>%  # group by everything other than the value column. (from: https://github.com/tidyverse/tidyr/issues/426)
      mutate(row_id=1:n()) %>% ungroup() %>% # build group index (from: https://github.com/tidyverse/tidyr/issues/426)
      spread(key=RDNC, value=Val) %>%
      dplyr::select(-one_of(c('row_id','Rainfall','Snowmelt')))

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

    # reintroduce climate
    if ( nrow(mdf)>0 ) {
      return(sdf %>% inner_join(mdf))
    } else {
      return(sdf)
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
  { if (!is.null(v$df)) df.filtered() }, 
  options = list(scrollY='100%', scrollX=TRUE,
                 lengthMenu = c(30, 100, 365, 3652),
                 pageLength = 100,
                 searching=FALSE)
)

output$tabCsv <- downloadHandler(
  filename <- function() { paste0(path_sanitize(v$title), '.csv') },
  content <- function(file) {
    if (!is.null(v$df)){ write.csv(df.filtered(), file, row.names = FALSE) }
  }
)