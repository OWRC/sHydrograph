


v <- reactiveValues(title=NULL,inam=NULL,icoord=NULL,df=NULL,typs=NULL,scrn=NULL,DTb=NULL,DTe=NULL)



source("func/collect_interval.R", local = TRUE)
source("func/qTemporal_json.R", local = TRUE)
# source("func/collect_interval_json.R", local = TRUE) # for testing
source("func/frequency_analysis.R", local = TRUE)
source("func/remove_outliers.R", local = TRUE)
source("func/characterMap.R", local = TRUE)
source("func/wtr_yr.R", local = TRUE)
source("func/gam.R", local = TRUE)
source("func/ORMGP_API.R", local = TRUE)
source("func/qTemporal_clean.R", local = TRUE)
source("func/YCDB_API_query.R", local = TRUE)
source("func/loc_info.R", local = TRUE)
source(file.path("func", "daterange.R"), local = TRUE)$value

source(file.path("srv/temporal", "srv_main.R"), local = TRUE)$value
source(file.path("srv/temporal", "srv_scatter.R"), local = TRUE)$value 
source(file.path("srv/temporal", "srv_data_table.R"), local = TRUE)$value 
source(file.path("srv/trends", "srv_annual.R"), local = TRUE)$value
source(file.path("srv/trends", "srv_seasonal.R"), local = TRUE)$value
source(file.path("srv/stats", "srv_extreme.R"), local = TRUE)$value
source(file.path("srv/stats", "srv_distr_mon.R"), local = TRUE)$value
source(file.path("srv/stats", "srv_distr_gam.R"), local = TRUE)$value
source(file.path("srv/stats", "srv_qq.R"), local = TRUE)$value
