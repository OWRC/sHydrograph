

pltcol <- c("#ef8a62","#43a2ca")

xr.Ignore <- c(163,369,449,608,609,611,612,633,645,748,757,758,760,761,762,763,764,765,860,861,10854,10859,70813,70868,70872,70899,70903,70977,71025,71026,71207,71208,71209,71212,71213,71350)

xr.RDNC <- c("447"="Pump",
             "546"="Tmax", 
             "547"="Tmin", 
             "548"="Tmean",
             "549"="Rain",
             "550"="Snow",
             "551"="Precip",
             "552"="PackDepth",
             "554"="Stage",
             "629"="WtrLvl",
             "628"="WtrLvl.s", 
             "70871"="Temp",
             # "71212"="AtmosYld",
             "1001"="StrmFlw",
             "Rf"="iRainfall",
             "Sm"="iSnowmelt",
             "Pa"="iAirPressure") #, #"611"="Wlvlx", "612"="Wlvln", 

xr.Nindx <- setNames(names(xr.RDNC), unname(xr.RDNC)) #reverses above named list

xr.NLong <- c(
    "Stage"="Stage (masl)",
    "Tmax"="Daily max temperature (C)", 
    "Tmin"="Daily min temperature (C)",
    "Tmean"="Daily mean temperature (C)",
    "Rain"="Rainfall (mm)",
    "Snow"="Snowfall (mm)",
    "PackDepth"="Snowpack depth (cm)",
    # "Wlvlx"="Water Level - Logger - Max (Compensated & Corrected)",
    # "Wlvln"="Water Level - Logger - Min (Compensated & Corrected)",
    "WtrLvl"="Water Level - Logger (Compensated & Corrected-masl)",
    "WtrLvl.s"="Water Level - Manual - Static (masl)",
    "StrmFlw"="Stream flow (cms)",
    "Temp"="Temperature (Water) - Logger (°C)",
    "Pump"="Production (m3/d)",
    "Precip"="Precipitation (mm)",
    "iRainfall"="Rainfall¹ (mm)",
    "iSnowmelt"="Snowmelt¹ (mm)",
    "iAirPressure"="AirPressure¹ (kPa)"
    # "AtmosYld"="Atmospheric Yield (mm)",
    )

xr.Nshrt <- setNames(names(xr.NLong), unname(xr.NLong)) #reverses above named list

xr.group <- c(
  # "Stage"="Stage (masl)",
  "StrmFlw"="Stream flow (m³/s)",
  "Tmax"="Temperature (°C)",
  "Tmin"="Temperature (°C)",
  "Tmean"="Temperature (°C)",
  "Rain"="Precipitation (mm)",
  "Snow"="Precipitation (mm)",
  "PackDepth"="Snowpack depth (cm)",
  # # "Wlvlx"="Water Level - Logger - Max (Compensated & Corrected)",
  # # "Wlvln"="Water Level - Logger - Min (Compensated & Corrected)",
  "WtrLvl"="Waterlevel (masl)",
  "WtrLvl.s"="Waterlevel (masl)",
  "Temp"="Temperature (°C)",
  "Tn"="Temperature (°C)",
  "Tx"="Temperature (°C)",
  "Pump"="Production (m³/d)",
  "Precip"="Precipitation (mm)",
  "Rf"="Precipitation (mm)",
  "Sm"="Precipitation (mm)",
  "Sf"="Precipitation (mm)",
  "Pa"="Atmospheric Pressure (kPa)"
)


xr.step <- c(
  "Stage"=FALSE,
  "Tmax"=FALSE, 
  "Tmin"=FALSE,
  "Tmean"=FALSE,
  # "Wlvlx"=FALSE,
  # "Wlvln"=FALSE,
  "WtrLvl"=FALSE,
  "WtrLvl.s"=FALSE,
  "Temp"=FALSE,
  "Pump"=TRUE,
  "Rain"=TRUE,
  "Snow"=TRUE,
  "Precip"=TRUE,
  "iRainfall"=TRUE,
  "iSnowmelt"=TRUE,
  "iAirPressure"=FALSE,
  "PackDepth"=TRUE,
  # "AtmosYld"=TRUE,
  "StrmFlw"=FALSE
)


xr.unit <- c(
  "3"="C",
  "6"="masl",
  "21"="mm",
  "24"="m3/s",
  "74"="m3/d",
  "159"="map uncal",
  "1001"="cms",
  "Tx"="C",
  "Tn"="C",
  "Temp"="C",
  "Rf"="mm",
  "Sf"="mm SWE",
  "Sm"="mm", 
  "Pa"="kPa"
)
