

collect_interval <- function(INT_ID,vTemporal=2) {
  jsonfp <- NULL
  if ( !is.numeric(INT_ID)) {
    jsonfp <- INT_ID # assuming a .json file for testing
    INT_ID <- strtoi(sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(jsonfp))) # filename no extension
  }
  
  print(paste0(' -> INT_ID: ', INT_ID))
  isolate(withProgress(message = 'querying station data..', value = 0.1, {
    nam <- qIntName(INT_ID)
    v$scrn <- qIntScreen(INT_ID)
    v$icoord <- qIntCoord(INT_ID)    

    nest <- qNest(INT_ID)
    setProgress(0.5,"querying observations..")
    if (length(nest) == 0) {
      if (length(nam)>1) { v$inam <- paste0(nam[[1]], ": ", nam[[2]]) }  else  {  v$inam <- nam[[1]]  }
      names(v$inam) <- INT_ID
      if (is.null(jsonfp)) {
        v$df <- characterMap(qTemporal(INT_ID, vTemporal) %>% mutate(IID=INT_ID),v$inam)
      } else {
        v$df <- characterMap(qTemporal_json(jsonfp) %>% mutate(IID=INT_ID),v$inam)
      }
      v$title <- v$inam[[1]]
    } else {
      showNotification("interval nest found, querying..")
      v$inam <- sapply(nest, function(x) qIntName(x)$INT_NAME)
      names(v$inam) <- nest
      if (is.null(jsonfp)) {
        qt <- qTemporal_nest(nest,vTemporal)
        v$inam <- v$inam[names(v$inam) %in% unique(qt$IID)]
        v$df <- characterMap(qt,v$inam)
      } else {
        qt <- qTemporal_json(jsonfp)
        v$inam <- v$inam[names(v$inam) %in% unique(qt$IID)]
        v$df <- characterMap(qt,v$inam)
      }
      v$title <- paste(unname(unlist(v$inam)), collapse = '; ')  
    }   
    
    v$DTb <- min(v$df$Date)
    v$DTe <- max(v$df$Date) 
   
    setProgress(0.6,"interpolating to location..")
    dfInterp <- qInterp(v$icoord$LONG,v$icoord$LAT) 
    if (!is.null(dfInterp)) {
      dfInterp <- dfInterp %>% 
        subset( Date >= v$DTb  &  Date <= v$DTe ) %>%
        dplyr::select(-one_of(c('Tn','Tx','Sf','Pa'))) %>% # drop columns
        gather(RDNC,Val,-Date) %>%
        drop_na() %>%
        mutate( IID = v$inam[as.character(INT_ID)],
                unit = xr.unit[RDNC],
                RDTC = "interpolated",
                grp = xr.group[RDNC],
                RDNC = xr.RDNC[RDNC] )
      
      # print(head(v$df, 3))
      # print(head(dfInterp, 3))
      v$df <- rbind(v$df, dfInterp) #%>% arrange(Date)
    }
    print(head(v$df, 3)) 
    v$typs <- xr.NLong[unique(v$df$RDNC)]
  }))
  shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")
  shinyjs::show("app-content")
  print("collect_interval complete!")
}


# ##############################################################
# ### collect data from API
# ##############################################################
# collect_interval <- function(INT_ID,vTemporal=2) {
#   isolate(withProgress(message = 'querying station data..', value = 0.1, {
#     setProgress(0.2,"location name..")
#     inam <- qIntName(INT_ID)
#     setProgress(0.3,"location info..")
#     v$scrn <- qIntScreen(INT_ID)
#     v$title <- inam[[1]]
#     setProgress(0.4,"location coordinates..")
#     icrd <- qIntCoord(INT_ID)
#     setProgress(0.5,"querying observations..")
#     
#     nest <- qNest(INT_ID)
#     if (length(nest) == 0) {
#       v$df <- qTemporal(INT_ID,vTemporal)
#     } else {
#       showNotification("interval nest found, querying..")
#       v$inam <- nest
#       names(v$inam) <- sapply(nest, function(x) qIntName(x)$INT_NAME)
#       v$title <- paste(names(v$inam), collapse = '; ')
#       v$df <- qTemporal_nest(nest,vTemporal)
#     }
#     
#     v$DTb <- min(v$df$orig$Date)
#     v$DTe <- max(v$df$orig$Date)
#     
#     # v$lat <- icrd$LAT
#     # v$lng <- icrd$LONG
#     setProgress(0.6,"interpolating to location..")
#     dfinterp <- qInterp(icrd$LONG,icrd$LAT)
#     v$df$plt <- v$df$plt %>% inner_join(dfinterp %>% dplyr::select(-c("Tx","Tn","Sf","Pa")), by="Date")
#   }))
#   shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")
#   shinyjs::show("app-content")
#   print("collect_interval complete!")
# }