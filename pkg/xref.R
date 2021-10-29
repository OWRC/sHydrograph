

pltcol <- c("#ef8a62","#43a2ca")

xr.Ignore <- c(163,369,449,608,609,611,612,633,645,748,758,760,761,762,763,764,765,860,861,10854,10859,70813,70868,70872,70899,70903,70977,71025,71026,71212)

xr.RNDC <- c("447"="Pump",
             "546"="Tmax", 
             "547"="Tmin", 
             "548"="Tmean",
             "549"="Rain",
             "550"="Snow",
             "551"="Precip",
             "552"="PackDepth",
             "554"="Stage",
             "628"="WtrLvl.s", 
             "629"="WtrLvl", 
             "70871"="Temp",
             # "71212"="AtmosYld",
             "1001"="StrmFlw") #, #"611"="Wlvlx", "612"="Wlvln", 

xr.Nindx <- setNames(names(xr.RNDC), unname(xr.RNDC)) #reverses above named list

xr.NLong <- c(
    "Stage"="Stage (masl)",
    "Tmax"="Daily max temperature (C)", 
    "Tmin"="Daily min temperature (C)",
    "Tmean"="Daily mean temperature (C)",
    # "Rain"="Rainfall (mm)",
    # "Snow"="Snowfall (mm)",
    "PackDepth"="Snowpack depth (cm)",
    # "Wlvlx"="Water Level - Logger - Max (Compensated & Corrected)",
    # "Wlvln"="Water Level - Logger - Min (Compensated & Corrected)",
    "WtrLvl.s"="Water Level - Manual - Static (masl)",
    "WtrLvl"="Water Level - Logger (Compensated & Corrected-masl)",
    "Temp"="Temperature (Water) - Logger (degC)",
    "Pump"="Production (m3/d)",
    "Precip"="Precipitation (mm)",
    "Rf"="Rainfall (mm)",
    "Sm"="Snowmelt (mm)",
    "AtmosYld"="Atmospheric Yield (mm)",
    "StrmFlw"="Stream flow (cms)"
    )

xr.Nshrt <- setNames(names(xr.NLong), unname(xr.NLong)) #reverses above named list

xr.group <- c(
  # "Stage"="Stage (masl)",
  # "Tmax"="Daily max temperature (C)", 
  # "Tmin"="Daily min temperature (C)",
  # "Tmean"="Daily mean temperature (C)",
  # # "Rain"="Rainfall (mm)",
  # # "Snow"="Snowfall (mm)",
  # "PackDepth"="Snowpack depth (cm)",
  # # "Wlvlx"="Water Level - Logger - Max (Compensated & Corrected)",
  # # "Wlvln"="Water Level - Logger - Min (Compensated & Corrected)",
  "WtrLvl.s"="Waterlevel (masl)",
  "WtrLvl"="Waterlevel (masl)",
  "Temp"="Temperature (°C)",
  "Pump"="Production (m³/d)",
  "Precip"="Precipitation (mm)",
  "Rf"="Precipitation (mm)",
  "Sm"="Precipitation (mm)"
)


xr.step <- c(
  "Stage"=FALSE,
  "Tmax"=FALSE, 
  "Tmin"=FALSE,
  # "Wlvlx"=FALSE,
  # "Wlvln"=FALSE,
  "WtrLvl.s"=FALSE,
  "WtrLvl"=FALSE,
  "Temp"=FALSE,
  "Pump"=TRUE,
  "Precip"=TRUE,
  "Rf"=TRUE,
  "Sm"=TRUE,
  "AtmosYld"=TRUE,
  "StrmFlw"=TRUE
)


xr.unit <- c(
  "3"="C",
  "6"="masl",
  "21"="mm",
  "74"="m3/d",
  "159"="map uncal",
  "1001"="cms"
)
