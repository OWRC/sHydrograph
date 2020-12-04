

pltcol <- c("#ef8a62","#43a2ca")

xr.Ignore <- c(163,369,447,449,608,609,611,612,633,645,748,758,761,762,763,764,765,860,10854,10859,70813,70899,70903,70977)

xr.RNDC <- c("546"="Tmax", "547"="Tmin","551"="Precip", "554"="Stage",
             "628"="WtrLvl.s", "629"="WtrLvl", "70871"="Temp",
             "71212"="AtmosYld","1001"="StrmFlw") #, #"611"="Wlvlx", "612"="Wlvln", 

xr.Nindx <- setNames(names(xr.RNDC), unname(xr.RNDC)) #reverses above named list

xr.NLong <- c(
    "Precip"="Precipitation (mm)",
    "Stage"="Stage (masl)",
    "Tmax"="Daily max temperature (degC)", 
    "Tmin"="Daily min temperature (degC)",
    # "Wlvlx"="Water Level - Logger - Max (Compensated & Corrected)",
    # "Wlvln"="Water Level - Logger - Min (Compensated & Corrected)",
    "WtrLvl.s"="Water Level - Manual - Static (masl)",
    "WtrLvl"="Water Level - Logger (Compensated & Corrected-masl)",
    "Temp"="Temperature (Water) - Logger (degC)",
    "AtmosYld"="Atmospheric Yield (mm)",
    "StrmFlw"="Stream flow (cms)"
    )

xr.Nshrt <- setNames(names(xr.NLong), unname(xr.NLong)) #reverses above named list


xr.step <- c(
  "Precip"=TRUE,
  "Stage"=FALSE,
  "Tmax"=FALSE, 
  "Tmin"=FALSE,
  # "Wlvlx"=FALSE,
  # "Wlvln"=FALSE,
  "WtrLvl.s"=FALSE,
  "WtrLvl"=FALSE,
  "Temp"=FALSE,
  "AtmosYld"=TRUE,
  "StrmFlw"=TRUE
)


xr.unit <- c(
  "3"="C",
  "6"="masl",
  "21"="mm",
  "159"="map uncal",
  "1001"="cms"
)
