

collect_interval_loc <- function(LOC_ID,vTemporal=2) {
  print(paste0(' -> LOC_ID: ', LOC_ID))
  qloc <- qLocInfo(LOC_ID)
  collect_interval(qloc$INT_ID[1],vTemporal)
}


collect_interval <- function(INT_ID,vTemporal=2) {
  jsonfp <- NULL
  if ( !is.numeric(INT_ID) & substr(INT_ID,1,1) != "[" ) {
    jsonfp <- INT_ID # assuming a .json file for testing
    INT_ID <- strtoi(sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(jsonfp))) # filename no extension
  }
     
  print(paste0('INT_ID: ', INT_ID))
  isolate(withProgress(message = 'querying station data..', value = 0.1, {
    v$meta <- qIntInfo(INT_ID) #%>%
      # dplyr::select(c(LOC_ID,LOC_NAME,LOC_NAME_ALT1,INT_ID,INT_NAME,INT_NAME_ALT1,INT_TYPE_CODE,LAT,LONG,X,Y,Z))
    if (is.null(v$meta)) showNotification(paste0("Error (meta): Interval ID not valid"))
    # print(v$meta)
    v$scrn <- qIntScreen(INT_ID)

    nest <- qNest(INT_ID)
    setProgress(0.5,"querying observations..")
    if ( length(nest) > 1 ) {
      showNotification("interval nest/group found, querying..") #paste0("interval nest found, querying..\n",paste(nest)))
      print(paste0("nest of ",length(nest)," being queried.."))
      v$meta <- bind_rows(lapply(nest, function(x) qIntInfo(x)), .id = "column_label") # %>%
      # # dplyr::select(c(LOC_ID,LOC_NAME,LOC_NAME_ALT1,INT_ID,INT_NAME,INT_NAME_ALT1,INT_TYPE_CODE,LAT,LONG,X,Y,Z))
      # dplyr::select(c(LOC_ID,LOC_NAME,LOC_NAME_ALT1,INT_ID,INT_NAME,INT_TYPE_CODE,LAT,LONG,X,Y,Z))
      # print(v$meta)
      # v$nam <- sapply(nest, function(x) qIntInfo(x)$INT_NAME[1])
      v$nam <- v$meta$INT_NAME
      names(v$nam) <- nest
      if (is.null(jsonfp)) {
        qt <- qTemporal_nest(nest,vTemporal)
      } else {
        qt <- qTemporal_json(jsonfp)
      }
      if (is.null(qt)) {
        showNotification("interval nest too large, querying only selected interval")
        nest <- c(INT_ID)
        v$meta <- qIntInfo(INT_ID)
      } else {
        v$nam <- v$nam[names(v$nam) %in% unique(qt$IID)]
        v$title <- paste(unname(unlist(v$nam)), collapse = '; ')          
      }
    }
    
    if (length(nest) <= 1) {
      if (is.null(jsonfp)) {
        qt <- qTemporal(INT_ID, vTemporal) #%>% mutate(IID=INT_ID)
      } else {
        qt <- qTemporal_json(jsonfp) #%>% mutate(IID=INT_ID)
      }
 
      if (is.numeric(INT_ID)) {
        # if (length(v$meta$INT_NAME_ALT1[1])>0) v$nam <- paste0(v$meta$INT_NAME[1], ": ", v$meta$INT_NAME_ALT1[1]) else v$nam <- v$meta$INT_NAME[1]
        v$nam <- paste0(v$meta$LOC_NAME[1], ": ", v$meta$INT_NAME[1])
        qt <- qt %>% mutate(IID=INT_ID)
        names(v$nam) <- INT_ID
        v$title <- v$nam[[1]]
      } else {
        v$nam <- paste0(v$meta$LOC_NAME, ": ", v$meta$INT_NAME)
        names(v$nam) <- v$meta$INT_ID #scan(text = str_sub(IIDs,2,-2), sep = ",")
        v$title <- paste(v$nam, collapse = "; ")
      }
      print(v$title)
    }   
    
    v$v0 <- qt[1,"Val"] # first value
    v$t0 <- qt[1,"Date"]
    
    urdnc <- unique(qt$RDNC)
    xrdnc <- xr.RDNC[as.character(urdnc)]
    if (anyNA(xrdnc)) { showNotification(paste0("unknown RDNC: ", paste(as.character(urdnc[which(is.na(xrdnc))]), sep="' '", collapse=", ")), duration = 35) }
    print(paste0("available RDNC: ", paste(urdnc,collapse="; ")))
    
    # ### special case: remove initial values made long before monitoring occurred (YCDB fix)
    # if ( nrow(qt) > 10 ) {
    #   for(i in 1:10) {
    #     if  ( as.numeric(difftime(qt[i+1,"Date"], qt[i,"Date"], unit="days"))/365.24 > 1.5 ) {
    #       qt = qt[-(1:i),]
    #       break
    #     }
    #   }
    # }

    v$raw <- characterMap(qt %>% mutate(Date = as.POSIXct(Date, format="%Y-%m-%dT%H:%M:%OS")), v$nam)  #%>% mutate(Date = as.POSIXct(Date))

    # convert to daily
    v$df <- characterMap(qt %>% 
                           mutate(Date = zoo::as.Date(Date)) %>%
                           group_by(Date, IID, RDNC, RDTC, unit) %>%
                           dplyr::summarise(Val = mean(Val)) %>% # grouping and summarizing needed to remove duplicate rows
                           ungroup(),
                         v$nam)  
    
    v$DTb <- min(v$df$Date)
    v$DTe <- max(v$df$Date) 
   
    if (vTemporal!=3) {
      setProgress(0.6,"interpolating to location..")
      dfInterp <- qInterp(v$meta$LONG[1],v$meta$LAT[1]) 
      if (!is.null(dfInterp)) {
        dfInterp <- dfInterp %>% 
          subset( Date >= v$DTb  &  Date <= v$DTe ) %>%
          dplyr::select(-one_of(c('Tn','Tx','Sf'))) %>% # drop columns
          # mutate(Pa=Pa/1000) %>%
          gather(RDNC,Val,-Date) %>%
          drop_na() %>%
          mutate( IID = "interpolated climate", #v$nam[as.character(INT_ID)],
                  unit = xr.unit[RDNC],
                  RDTC = "interpolated",
                  grp = xr.group[RDNC],
                  RDNC = xr.RDNC[RDNC] )
        
        # print(nrow(v$raw))
        # print(head(v$raw, 3))
        # print(head(v$df, 3))
        # print(head(dfInterp, 3))
        v$df <- rbind(v$df, dfInterp) #%>% arrange(Date)
        # v$raw <- rbind(v$raw, dfInterp)
        # print(nrow(v$raw))
      }      
    }
    # print(head(v$df, 3)) 
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