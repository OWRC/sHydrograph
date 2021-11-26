

v <- reactiveValues(title=NULL,nam=NULL,meta=NULL,df=NULL,typs=NULL,scrn=NULL,DTb=NULL,DTe=NULL)


source("srv/headers.R", local = TRUE)
source("func/collect_interval.R", local = TRUE)
source("func/qTemporal_clean.R", local = TRUE)
source("func/qTemporal_json.R", local = TRUE)
# # source("func/collect_interval_json.R", local = TRUE) # for testing
source("func/ORMGP_API.R", local = TRUE)
source("func/YCDB_API_query.R", local = TRUE)

source("func/frequency_analysis.R", local = TRUE)
source("func/frequency_plot.R", local = TRUE)
source("func/remove_outliers.R", local = TRUE)
source("func/whichQuantile.R", local = TRUE)
source("func/characterMap.R", local = TRUE)
source("func/wtr_yr.R", local = TRUE)
source("func/gam.R", local = TRUE)
source(file.path("func", "daterange.R"), local = TRUE)$value