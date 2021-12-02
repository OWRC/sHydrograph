

collect_interval_loc <- function(LOC_ID,vTemporal=2) {
  print(paste0(' -> LOC_ID: ', LOC_ID))
  qloc <- qLocInfo(LOC_ID)
  collect_interval(qloc$INT_ID[1],vTemporal)
}


collect_interval <- function(INT_ID,vTemporal=2) {
  jsonfp <- NULL
  if ( !is.numeric(INT_ID)) {
    jsonfp <- INT_ID # assuming a .json file for testing
    INT_ID <- strtoi(sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(jsonfp))) # filename no extension
  }
  
  print(paste0(' -> INT_ID: ', INT_ID))
  isolate(withProgress(message = 'querying station data..', value = 0.1, {
    v$meta <- qIntInfo(INT_ID) #%>%
      # dplyr::select(c(LOC_ID,INT_ID,INT_NAME,INT_NAME_ALT1,INT_TYPE_CODE,LAT,LONG,X,Y,Z))
    if (is.null(v$meta)) showNotification(paste0("Error: Interval ID not valid"))
    v$scrn <- qIntScreen(INT_ID)

    nest <- qNest(INT_ID)
    setProgress(0.5,"querying observations..")
    if (length(nest) <= 1) {
      # if (length(v$meta$INT_NAME_ALT1[1])>0) v$nam <- paste0(v$meta$INT_NAME[1], ": ", v$meta$INT_NAME_ALT1[1]) else v$nam <- v$meta$INT_NAME[1]
      v$nam <- paste0(v$meta$LOC_NAME[1], ": ", v$meta$INT_NAME[1])
      names(v$nam) <- INT_ID
      if (is.null(jsonfp)) {
        v$df <- characterMap(qTemporal(INT_ID, vTemporal) %>% mutate(IID=INT_ID),v$nam)
      } else {
        v$df <- characterMap(qTemporal_json(jsonfp) %>% mutate(IID=INT_ID),v$nam)
      }
      v$title <- v$nam[[1]]
    } else {
      showNotification("interval nest found, querying..") #paste0("interval nest found, querying..\n",paste(nest)))
      v$meta <- bind_rows(lapply(nest, function(x) qIntInfo(x)), .id = "column_label") %>%
        dplyr::select(c(LOC_ID,INT_ID,INT_NAME,INT_NAME_ALT1,INT_TYPE_CODE,LAT,LONG,X,Y,Z))
      v$nam <- sapply(nest, function(x) qIntInfo(x)$INT_NAME[1])
      names(v$nam) <- nest
      if (is.null(jsonfp)) {
        qt <- qTemporal_nest(nest,vTemporal)
        v$nam <- v$nam[names(v$nam) %in% unique(qt$IID)]
        v$df <- characterMap(qt,v$nam)
      } else {
        qt <- qTemporal_json(jsonfp)
        v$nam <- v$nam[names(v$nam) %in% unique(qt$IID)]
        v$df <- characterMap(qt,v$nam)
      }
      v$title <- paste(unname(unlist(v$nam)), collapse = '; ')  
    }   
    
    v$DTb <- min(v$df$Date)
    v$DTe <- max(v$df$Date) 
   
    if (vTemporal!=3) {
      setProgress(0.6,"interpolating to location..")
      dfInterp <- qInterp(v$meta$LONG[1],v$meta$LAT[1]) 
      if (!is.null(dfInterp)) {
        dfInterp <- dfInterp %>% 
          subset( Date >= v$DTb  &  Date <= v$DTe ) %>%
          dplyr::select(-one_of(c('Tn','Tx','Sf'))) %>% # drop columns
          mutate(Pa=Pa/1000) %>%
          gather(RDNC,Val,-Date) %>%
          drop_na() %>%
          mutate( IID = "interpolated", #v$nam[as.character(INT_ID)],
                  unit = xr.unit[RDNC],
                  RDTC = "interpolated",
                  grp = xr.group[RDNC],
                  RDNC = xr.RDNC[RDNC] )
        
        # print(head(v$df, 3))
        # print(head(dfInterp, 3))
        v$df <- rbind(v$df, dfInterp) #%>% arrange(Date)
      }      
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