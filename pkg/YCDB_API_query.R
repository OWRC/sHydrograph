##########################################################
#################### YCDB querying ####################### 
##########################################################
# in support of sHydrograph
# By M. Marchildon
#
# Dec, 2020
##########################################################


###########################################################################################
## API addresses
###########################################################################################
# ldbc <- 'https://api.oakridgeswater.ca/api/loc_met/' # 'https://camcfunctions.azurewebsites.net/api/loc_met/' # 'https://functions.oakridgeswater.ca/api/loc_met/'
idbc <- 'https://api.oakridgeswater.ca/api/intgen/?t=%d&i=%d'
sdbc <- 'https://api.oakridgeswater.ca/api/intscreen?i=%d'
inamapi <- 'https://api.oakridgeswater.ca/api/intnam/?i=%d'


###########################################################################################
## collect locations
###########################################################################################
qLoc <- function(API){
  return(fromJSON(API))
}

###########################################################################################
## collect location/interval info
###########################################################################################
qLocInfo <- function(API,LOC_ID){
  t1 <- qLoc(API)
  return(t1[t1$LID==LOC_ID,])
}

qIntName <- function(INT_ID){ fromJSON(sprintf(inamapi,INT_ID)) }

qIntScreen <- function(INT_ID){ 
  out <- tryCatch(
    {
      fromJSON(sprintf(sdbc,INT_ID))
    },
    error=function(cond) {
      return(NULL)
    },
    warning=function(cond) {
      return(NULL)
    },
    finally={}
  )
  return(out)  
}

###########################################################################################
## temporal Query
###########################################################################################
qTemporal <- function(INT_ID,vTemporal=2){ qTemporal_json(sprintf(idbc,vTemporal,INT_ID)) }

qTemporal_byLOC_ID <- function(lAPI,iAPI,LOC_ID){
  t1 <- qLoc(lAPI)
  return(qTemporal(iAPI,t1[t1$LID==LOC_ID,]$IID))
}

# df <- qTemporal_json("C:/Users/mason/OneDrive/R/web_ormgp/sMet/test/-1741125310.json")
qTemporal_json <- function(fp) {
  df <- fromJSON(fp)
  return(qTemporal_clean(df))
}

           