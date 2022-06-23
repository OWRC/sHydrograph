

characterMap <- function(df,inam) {
  
  df %>% mutate(RDNC = xr.RDNC[as.character(RDNC)],
                unit = xr.unit[as.character(unit)],
                grp = xr.group[as.character(RDNC)],
                IID = inam[as.character(IID)])
}