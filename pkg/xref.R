

pltcol <- c("#ef8a62","#43a2ca")

xr.Ignore <- c(163,369,447,449,608,609,611,612,645,748,761,762,763,764,765,70813,70899,70977)

xr.RNDC <- c("551"="Precip","546"="Tmax", "547"="Tmin",
             "628"="WtrLvl.s", "629"="WtrLvl", "70871"="Temp",
             "71212"="AtmosYld") #, #"611"="Wlvlx", "612"="Wlvln", 

xr.Nindx <- setNames(names(xr.RNDC), unname(xr.RNDC)) #reverses above named list

xr.NLong <- c(
    "Precip"="Precipitation (mm)",
    "Tmax"="Daily max temperature (degC)", 
    "Tmin"="Daily min temperature (degC)",
    # "Wlvlx"="Water Level - Logger - Max (Compensated & Corrected)",
    # "Wlvln"="Water Level - Logger - Min (Compensated & Corrected)",
    "WtrLvl.s"="Water Level - Manual - Static (masl)",
    "WtrLvl"="Water Level - Logger (Compensated & Corrected-masl)",
    "Temp"="Temperature (Water) - Logger (degC)",
    "AtmosYld"="Atmospheric Yield (mm)"
    )

xr.Nshrt <- setNames(names(xr.NLong), unname(xr.NLong)) #reverses above named list


xr.step <- c(
  "Precip"=TRUE,
  "Tmax"=FALSE, 
  "Tmin"=FALSE,
  # "Wlvlx"=FALSE,
  # "Wlvln"=FALSE,
  "WtrLvl.s"=FALSE,
  "WtrLvl"=FALSE,
  "Temp"=FALSE,
  "AtmosYld"=TRUE
)


xr.unit <- c(
  "3"="C",
  "6"="masl",
  "21"="mm",
  "159"="map uncal"
)
