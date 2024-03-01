##########################################################
#################### YCDB querying ####################### 
##########################################################
# in support of sHydrograph
# Nov, 2023
##########################################################

data.api <- 'http://golang.oakridgeswater.ca:8080/'

###########################################################################################
## API addresses
###########################################################################################
idbc <- paste0(data.api,'intgend/%d/%s')
idbcs <- paste0(data.api,'intgend/%d/[%s]')
iscreen <- paste0(data.api,'intscreen/%d')
inest <- paste0(data.api,'intnest/%d')
iinfo <- paste0(data.api,'intinfo/%s')
linfo <- paste0(data.api,'locinfo/%d')


qIntScreen <- function(INT_ID){ 
  out <- tryCatch(
    {
      fromJSON(sprintf(iscreen,INT_ID))
    },
    error=function(cond) {
      return(NULL)
    },
    warning=function(cond) {
      return(NULL)
    },
    finally={}
  )
  return(out[names(out) != 'INT_ID'])  
}


###########################################################################################
## temporal Query
###########################################################################################
qTemporal <- function(INT_ID,vTemporal=2){ qTemporal_json(sprintf(idbc,vTemporal,INT_ID)) }

qTemporal_nest <- function(nest,vTemporal) { qTemporal_json(sprintf(idbcs,vTemporal,paste(nest, collapse = ','))) }

qNest <- function(INT_ID){
  if ( !is.numeric(INT_ID) ) return(NULL)
  tryCatch(
    {
      fromJSON(sprintf(inest,INT_ID))
    },
    error=function(cond) {
      showNotification(paste0("Error: invalid interval ID"))
      return(NULL)
    },
    warning=function(cond) {
      showNotification(paste0("Error: invalid interval ID"))
      return(NULL)
    },
    finally={}
  )
}

qLocInfo <- function(LOC_ID){
  tryCatch(
    {
      fromJSON(sprintf(linfo,LOC_ID))
    },
    error=function(cond) {
      showNotification(paste0("Error: invalid location ID"))
      return(NULL)
    },
    warning=function(cond) {
      showNotification(paste0("Error: invalid location ID"))
      return(NULL)
    },
    finally={}
  )  
}

qIntInfo <- function(Int_ID){
  tryCatch(
    {
      print(sprintf(iinfo,Int_ID))
      fromJSON(sprintf(iinfo,Int_ID))
    },
    error=function(cond) {
      showNotification(paste0("Error: invalid interval ID"))
      return(NULL)
    },
    warning=function(cond) {
      showNotification(paste0("Error: invalid interval ID"))
      return(NULL)
    },
    finally={}
  )  
}
