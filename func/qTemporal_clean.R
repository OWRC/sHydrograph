


qTemporal_clean <- function(df) {

  # remove unwanted intervals
  df <- df[!(df$RDNC %in% xr.Ignore),]

  # special case, remove initial values made long before monitoring occurred (YCDB fix)
  if ( nrow(df) > 10 ) {
    for(i in 1:10) {
      if  ( as.numeric(difftime(df[i+1,"Date"], df[i,"Date"], unit="days"))/365.24 > 1.5 ) { 
        df = df[-(1:i),] 
        break
      }        
    }
  }   
  
  # include required fields
  if (!('RDTC' %in% colnames(df))) { df['RDTC'] <- NA } # value qualifyer
  if (!('IID' %in% colnames(df))) { df['IID'] <- NA } # value location id

  # convert to dailies
  df %>% 
    mutate(Date = zoo::as.Date(Date)) %>%
    group_by(Date, IID, RDNC, RDTC, unit) %>%
    dplyr::summarise(Val = mean(Val)) %>% # grouping and summarizing needed to remove duplicate rows
    ungroup()
  
  
  # df.plt <- df %>%
  #   dplyr::select(-one_of(c('RDTC','unit'))) %>% # drop columns
  #   group_by_at(vars(-Val)) %>%  # group by everything other than the value column. (from: https://github.com/tidyverse/tidyr/issues/426) 
  #   mutate(row_id=1:n()) %>% ungroup() %>% # build group index (from: https://github.com/tidyverse/tidyr/issues/426)
  #   spread(key=RDNC, value=Val) %>%
  #   dplyr::select(-one_of(c('row_id'))) %>% # drop col 
  #   # mutate(Date = zoo::as.Date(Date)) %>%
  #   plyr::rename(xr.RNDC) #%>%
  # # complete(Date = seq.Date(min(Date),max(Date),by='day'))
  # # View(df.plt)
  # 
  # if (nested) {
  #   df.plt <- df.plt %>% 
  #     gather(variable, value, -(Date:IID)) %>%
  #     unite(temp, IID, variable) %>%
  #     group_by(Date, temp) %>%
  #     dplyr::summarise(value = mean(value)) %>% # grouping and summarizing needed to remove duplicate rows
  #     ungroup()%>%
  #     spread(temp, value)    
  # } else {
  #   df.plt <- df.plt %>% dplyr::select(-IID) # drop column
  # }
  # 
  # # if ("AtmosYld" %in% names(df.plt)) {
  # #   s <- min(which(!is.na(df.plt$AtmosYld))) # index of first non-missing value
  # #   f <- max(which(!is.na(df.plt$AtmosYld))) # index of last non-missing value
  # #   df.plt$AtmosYld[s:f][is.na(df.plt$AtmosYld[s:f])] <- 0
  # # }
  # 
  # return(list("orig" = df, "plt" = df.plt))
}



# qTemporal_clean <- function(df) {
#   df <- df[!(df$RDNC %in% xr.Ignore),]
#   # print(df)
# 
#   if ( nrow(df) > 10  &&  as.numeric(difftime(df[2,"Date"], df[1,"Date"], unit="days"))/365.24 > 1.5 ) { df = df[-1,] } # remove first row from database (YCDB fix)
#   
#   if (!('RDTC' %in% colnames(df))) { df['RDTC'] <- NA }
#   blNest <- TRUE
#   if (!('IID' %in% colnames(df))) { 
#     df['IID'] <- NA 
#     blNest <- FALSE
#   }
#   
#   # convert to dailies
#   df <- df %>% 
#     mutate(Date = zoo::as.Date(Date)) %>%
#     group_by(Date, IID, RDNC, RDTC, unit) %>%
#     dplyr::summarise(Val = mean(Val)) %>% # grouping and summarizing needed to remove duplicate rows
#     ungroup()
#   # View(df)
#   
#   df.plt <- df %>%
#     dplyr::select(-one_of(c('RDTC','unit'))) %>% # drop columns
#     group_by_at(vars(-Val)) %>%  # group by everything other than the value column. (from: https://github.com/tidyverse/tidyr/issues/426) 
#     mutate(row_id=1:n()) %>% ungroup() %>% # build group index (from: https://github.com/tidyverse/tidyr/issues/426)
#     spread(key=RDNC, value=Val) %>%
#     dplyr::select(-one_of(c('row_id'))) %>% # drop col 
#     # mutate(Date = zoo::as.Date(Date)) %>%
#     plyr::rename(xr.RNDC) #%>%
#   # complete(Date = seq.Date(min(Date),max(Date),by='day'))
#   # View(df.plt)
#   
#   if (blNest) {
#     df.plt <- df.plt %>% 
#       gather(variable, value, -(Date:IID)) %>%
#       unite(temp, IID, variable) %>%
#       group_by(Date, temp) %>%
#       dplyr::summarise(value = mean(value)) %>% # grouping and summarizing needed to remove duplicate rows
#       ungroup()%>%
#       spread(temp, value)    
#   } else {
#     df.plt <- df.plt %>% dplyr::select(-IID) # drop column
#   }
# 
#   # if ("AtmosYld" %in% names(df.plt)) {
#   #   s <- min(which(!is.na(df.plt$AtmosYld))) # index of first non-missing value
#   #   f <- max(which(!is.na(df.plt$AtmosYld))) # index of last non-missing value
#   #   df.plt$AtmosYld[s:f][is.na(df.plt$AtmosYld[s:f])] <- 0
#   # }
#   
#   return(list("orig" = df, "plt" = df.plt))
# }