

characterMap <- function(df,inam) {

  print(paste0(" available RDNC: ",paste(unique(df$RDNC),collapse="; ")))
  
  df %>% mutate(RDNC = xr.RDNC[as.character(RDNC)],
                unit = xr.unit[as.character(unit)],
                grp = xr.group[as.character(RDNC)],
                IID = inam[as.character(IID)]) #%>% 
    # dplyr::select(-one_of(c('RDTC','unit'))) %>% # drop columns
    # drop_na()
  
}