
source(file.path("srv", "daterange.R"), local = TRUE)$value

source(file.path("srv/temporal", "srv_main.R"), local = TRUE)$value
source(file.path("srv/trends", "srv_annual.R"), local = TRUE)$value
source(file.path("srv/trends", "srv_seasonal.R"), local = TRUE)$value
source(file.path("srv/trends", "srv_scatter.R"), local = TRUE)$value
source(file.path("srv/stats", "srv_extreme.R"), local = TRUE)$value
source(file.path("srv/stats", "srv_distr_mon.R"), local = TRUE)$value
source(file.path("srv/stats", "srv_distr_gam.R"), local = TRUE)$value
source(file.path("srv/stats", "srv_qq.R"), local = TRUE)$value
source(file.path("srv", "srv_data_table.R"), local = TRUE)$value 