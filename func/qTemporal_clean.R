
qTemporal_clean <- function(df) {
  df <- df[!(df$RDNC %in% xr.Ignore),]

  if ( as.numeric(difftime(df[2,"Date"], df[1,"Date"], unit="days"))/365.24 > 1.5 ) { df = df[-1,] } # remove first row from database (YCDB fix)
  
  if (!('RDTC' %in% colnames(df))) { df['RDTC'] <- NA }
  
  # convert to dailies
  df <- df %>% 
    mutate(Date = zoo::as.Date(Date)) %>%
    group_by(Date, RDNC, RDTC, unit) %>%
    dplyr::summarise(Val = mean(Val)) %>% # grouping and summarizing needed to remove duplicate rows
    ungroup()
  # View(df)
  
  df.plt <- df %>%
    dplyr::select(-one_of(c('RDTC','unit'))) %>% # drop columns
    group_by_at(vars(-Val)) %>%  # group by everything other than the value column. (from: https://github.com/tidyverse/tidyr/issues/426) 
    mutate(row_id=1:n()) %>% ungroup() %>% # build group index (from: https://github.com/tidyverse/tidyr/issues/426)
    spread(key=RDNC, value=Val) %>%
    dplyr::select(-one_of(c('row_id'))) %>% # drop col 
    # mutate(Date = zoo::as.Date(Date)) %>%
    plyr::rename(xr.RNDC) #%>%
  # complete(Date = seq.Date(min(Date),max(Date),by='day'))
  # View(df.plt)
  
  # if ("AtmosYld" %in% names(df.plt)) {
  #   s <- min(which(!is.na(df.plt$AtmosYld))) # index of first non-missing value
  #   f <- max(which(!is.na(df.plt$AtmosYld))) # index of last non-missing value
  #   df.plt$AtmosYld[s:f][is.na(df.plt$AtmosYld[s:f])] <- 0
  # }
  
  return(list("orig" = df, "plt" = df.plt))
}