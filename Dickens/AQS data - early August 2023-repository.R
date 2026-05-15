## Process early August (esp. August 2) 2023 data for AGES+ Chicago paper

require(tidyverse)

setwd("...")


#Load hourly data for July 30-August 5, 2023 for Chicago (downloaded from AQS)
#Ozone (44201), PM2.5 (88101), NO2 (42602), resultant wind speed (61103), resultant wind direction (61104), outdoor temperature (62101), and relative humidity (62201)

Aug.data <- read.table("AMP350_2309295-early Aug AQS data.txt", header=TRUE, fill=TRUE, sep="|")
Aug.data <- Aug.data %>%
  dplyr::filter(RAW.DATA.TYPE...2 != 1) %>%
  dplyr::mutate(param = ifelse(PARAMETER_CODE==44201, "Ozone", ifelse(PARAMETER_CODE==88101, "PM25", ifelse(PARAMETER_CODE==42602, "NO2", ifelse(PARAMETER_CODE== 61103, "ws",
                               ifelse(PARAMETER_CODE==61104, "wd", ifelse(PARAMETER_CODE==62101, "Temp", "RH")))))),
                Site = paste0(STATE_CODE, str_pad(COUNTY_CODE, 3, pad="0"), str_pad(SITE_ID, 4, pad="0")), Date = as.Date(as.character(SAMPLE_DATE), "%Y%m%d")) %>%
  dplyr::select(Site, POC, Date, param, DURATION, 18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64) %>%
  dplyr::mutate_at(6:29, ~as.numeric(.)) %>%
# Aug.data[,6:29] = apply(Aug.data[,6:29], 2, function(x) as.numeric(x))
  pivot_longer(6:29, names_to = "hour", values_to = "value") %>%
  dplyr::mutate(hour = as.numeric(substr(hour, 7, 8)), datetime.CST = as.POSIXct(paste(Date, hour), "%Y-%m-%d %H", tz="America/Denver"), #put in CST (=MDT)
                datetime.CDT = as.POSIXct(datetime.CST, tz="America/Chicago"), Date = as.Date(datetime.CDT, tz="America/Chicago")) %>% #convert to CDT
  dplyr::filter(DURATION == 1)
  # dplyr::group_by(Site,POC,Date,param) %>%
  # dplyr::mutate(value = ifelse(param=="PM25" & DURATION==7, max(value, na.rm = TRUE), value)) %>% #for 24-hr filter samples, fill all hours with 24-hr conc
  # dplyr::ungroup()

#Make plots for whole week
param.list <- unique(Aug.data$param)

    # for(i in param.list)
    # {
    #   param.subset <- dplyr::filter(Aug.data, param == i)
    #   
    #   a <- ggplot(param.subset, aes(x=datetime, y=value)) + geom_line() + facet_wrap(~ Site) +
    #     ggtitle(paste0("Chicago area ", i, " - July 30-August 5, 2023")) +
    #     theme(axis.text.x=element_text(angle=90))
    #   ggsave(paste0("Chicago early August - ", i, ".png"), plot = a, width = 6, height = 6)
    # }

#Make plots for just August 2nd
Aug2.data <- Aug.data %>%
  dplyr::filter(Date == as.Date("2023-08-02"))

    # for(i in param.list)
    # {
    #   param.subset <- dplyr::filter(Aug2.data, param == i)
    #   
    #   a <- ggplot(param.subset, aes(x=datetime, y=value)) + geom_line() + facet_wrap(~ Site) +
    #     ggtitle(paste0("Chicago area ", i, " - August 2, 2023")) +
    #     theme(axis.text.x=element_text(angle=90))
    #   ggsave(paste0("Chicago August 2 - ", i, ".png"), plot = a, width = 6, height = 6)
    # }

#Chiwaukee data only
CP.Aug2 <- Aug2.data %>%
  dplyr::filter(Site == "550590019") %>%
  dplyr::mutate(param = factor(param, levels = c("Temp","ws","wd","Ozone","PM25","NO2")))
  
    # a <- ggplot(CP.Aug2, aes(x=datetime, y=value)) + geom_line() + facet_grid(param~., scales = "free_y") +
    #   scale_x_datetime(date_labels = "%H:%M") + xlab("Time (CST)") + ylab(NULL) + ggtitle("Chiwaukee Prairie - August 2, 2023") 
    # ggsave("Chiwaukee August 2 met-AQ.png", plot = a, width = 6, height = 4)
    
#Northbrook data only (about 20 km NNW from North Park University and NEIU - closest EPA met)
Northbrook.Aug2 <- Aug2.data %>%
  dplyr::filter(Site == "170314201" & param != "RH" & POC != 3) %>%
  dplyr::mutate(param = factor(param, levels = c("Temp","ws","wd","Ozone","PM25","NO2")))
    
    # a <- ggplot(Northbrook.Aug2, aes(x=datetime, y=value)) + geom_line() + facet_grid(param~., scales = "free_y") +
    #   scale_x_datetime(date_labels = "%H:%M") + xlab("Time (CST)") + ylab(NULL) + ggtitle("Northbrook - August 2, 2023")
    # ggsave("Northbrook August 2 met-AQ.png", plot = a, width = 6, height = 4)



#Pandora data from Chicago North Park University
Pandora.NPU <- read.table("Pandora249s1_ChicagoIL_L2_rnvs3p1-8-no header.txt", sep= " ", header = FALSE)

Pandora.NPU <- Pandora.NPU %>%
  dplyr::mutate(datetime.UTC = as.POSIXct(V1, format = "%Y%m%dT%H%M%OSZ", tz = "UTC")) %>%
  dplyr::mutate(datetime.CDT = as.POSIXct(datetime.UTC, tz = "America/Chicago"), Date = as.Date(datetime.CDT, tz="America/Chicago"))
  # dplyr::mutate(datetime.UTC = format(datetime.UTC, digits = 1))

Pandora.Aug2 <- Pandora.NPU %>%
  dplyr::filter(Date == as.Date("2023-08-02")) %>%
  dplyr::select(57,56,39,36) %>%
  dplyr::rename(NO2.TVC = V39, NO2.flag = V36) %>%
  dplyr::filter(NO2.flag == 0) %>% #select only data with a "L2 data quality flag for nitrogen dioxide" of 0-assured high quality (drops almost half of the measurements)
  dplyr::mutate(NO2.TVC.molec.cm2 = NO2.TVC * 6.02E23 / 1E4 / 1E15) #convert moles/m2 to molecules/cm2 x 10^15
# write.csv(Pandora.Aug2, "Pandora data raw- NPU - Aug2.csv", row.names = FALSE)

# #Plot raw data
    a <- ggplot(Pandora.Aug2, aes(x=datetime.CDT, y=NO2.TVC.molec.cm2)) + geom_line() +
      scale_x_datetime(limits = c(as.POSIXct("2023-08-02 00:00:00", tz="America/Chicago"), as.POSIXct("2023-08-03 00:00:00", tz="America/Chicago")),
                       date_labels = "%H") +
      xlab("hour") + ylab("NO2 Total Vertical Column (molec/cm2 x 10^15)")

Pandora.Aug2.hourly <- Pandora.Aug2 %>%
  dplyr::mutate(hour = as.numeric(format(datetime.CDT, "%H"))) %>%
  dplyr::group_by(Date,hour) %>%
  dplyr::summarise(NO2.TVC.hour.molec.cm2 = mean(NO2.TVC.molec.cm2)) %>%
  dplyr::ungroup() #%>%
  # dplyr::mutate(NO2.TVC.hour.molec.cm2 = NO2.TVC.hour * 6.02E23 / 1E4 / 1E15) #convert moles/m2 to molecules/cm2 x 10^15

#Plot hourly data
    a <- ggplot(Pandora.Aug2.hourly, aes(x=hour, y=NO2.TVC.hour.molec.cm2, group=1)) + geom_line() + 
      scale_x_continuous(limits = c(0,24), breaks = seq(0,24,by=6)) +
      xlab("hour") + ylab("NO2 Total Vertical Column (molec/cm2 x 10^15)")
    
    
    
#AERONET data from North Park University
#Reproduce figure provided on website
AERONET.NPU <- read.table("./other figures/AERONET_20230801_20230803_NPU_Chicago_IL.lev20-no header.txt", sep=",", header=TRUE)
AERONET.NPU <- AERONET.NPU %>%
  dplyr::mutate(location = "Chicago")

AERONET.Chiwaukee <- read.table("./other figures/20230801_20230803_Chiwaukee_Prairie.lev20-no header.txt", sep=",", header=TRUE)
AERONET.Chiwaukee <- AERONET.Chiwaukee %>%
  dplyr::mutate(location = "Chiwaukee")

AERONET.both <- bind_rows(AERONET.NPU, AERONET.Chiwaukee)

AERONET.both <- AERONET.both %>%
  dplyr::select(114,1,2,5,6,7,10,19,22,25,26) %>%
  dplyr::mutate(datetime.UTC = as.POSIXct(paste(Date.dd.mm.yyyy., Time.hh.mm.ss.), "%d:%m:%Y %H:%M:%S", tz="UTC"),
                datetime.CDT = as.POSIXct(datetime.UTC, tz="America/Chicago"), Date = as.Date(datetime.CDT, tz="America/Chicago"))

AERONET.both.Aug2 <- AERONET.both %>%
  dplyr::filter(Date == as.Date("2023-08-02")) %>%
  dplyr::mutate(hour = as.numeric(format(datetime.CDT, "%H"))) %>%
  pivot_longer(4:11, names_to = "lambda", values_to = "AOD") %>%
  dplyr::mutate(lambda = substr(lambda, start=5, stop=10))

AERONET.both.Aug2$lambda <- factor(AERONET.both.Aug2$lambda, levels = c("340nm","380nm","440nm","500nm","675nm","870nm","1020nm","1640nm"))

#Plot raw data
    # a <- ggplot(AERONET.both.Aug2, aes(x=datetime.CDT, y=AOD, color=lambda, group=lambda)) + geom_line() + facet_grid(.~location) +
    #   scale_x_datetime(limits = c(as.POSIXct("2023-08-02 00:00:00", tz="America/Chicago"), as.POSIXct("2023-08-03 00:00:00", tz="America/Chicago")),
    #                    date_labels = "%H") +
    #   xlab("hour") + ylab("Aerosol Optical Depth (AOD)")

AERONET.both.Aug2.hourly <- AERONET.both.Aug2 %>%
  dplyr::group_by(location,Date,hour,lambda) %>%
  dplyr::summarise(AOD.hour = mean(AOD)) %>%
  dplyr::ungroup()

#Plot hourly data
    # a <- ggplot(AERONET.both.Aug2.hourly, aes(x=hour, y=AOD.hour, color=lambda, group=lambda)) + geom_line() + facet_grid(.~location) +
    #   scale_x_continuous(limits = c(0,24), breaks = seq(0,24,by=6)) +
    #   xlab("hour") + ylab("Aerosol Optical Depth (AOD)")


## Combine data for Chiwaukee and Schiller Park (gases)/ORD (met)/North Park University into one figure

#Schiller Park data only
Schil.Aug2 <- Aug2.data %>%
  dplyr::filter(Site == "170313103" & param != "Temp" & POC != 3) %>% #drops temperature (use ORD met) and uncorrected PM2.5 (POC 3)
  dplyr::mutate(location = "Chicago", hour = as.numeric(format(datetime.CDT, "%H")))

ORD.met.Aug2 <- read.csv("./other figures/ORD met - Aug 1-3 2023.csv", header = TRUE)
ORD.met.Aug2 <- ORD.met.Aug2 %>%
  dplyr::mutate(datetime.CST = as.POSIXct(date, "%m/%d/%Y %H:%M", tz="America/Denver"), datetime.CDT = as.POSIXct(datetime.CST, tz="America/Chicago"),
                Date = as.Date(datetime.CDT, tz="America/Chicago"), location = "Chicago", hour = as.numeric(format(datetime.CDT, "%H")),
                Wind.Direction..deg. = as.numeric(ifelse(Wind.Direction..deg. == "VAR", "0", Wind.Direction..deg.))) %>%
  dplyr::filter(Date == as.Date("2023-08-02")) %>%
  dplyr::rename(Temp = Temperature..F., ws = Wind.Speed..mph., wd = Wind.Direction..deg.) %>%
  dplyr::select(location,Date,datetime.CDT,hour,Temp,ws,wd) %>%
  pivot_longer(Temp:wd, names_to = "param", values_to = "value")

CP.Aug2.short <- CP.Aug2 %>%
  dplyr::mutate(location = "Chiwaukee", hour = as.numeric(format(datetime.CDT, "%H"))) %>%
  dplyr::select(location,Date,datetime.CDT,hour,param,value) 
# Northbrook.Aug2.short <- Northbrook.Aug2 %>%
#   dplyr::mutate(location = "Chicago", hour = as.numeric(format(datetime.CDT, "%H"))) %>%
#   dplyr::select(location,Date,datetime.CDT,hour,param,value) 
# NPU.Pandora.NO2.short <- Pandora.Aug2.hourly %>%
#   dplyr::mutate(location = "Chicago", param = "NO2 TVC") %>%
#   dplyr::rename(value = NO2.TVC.hour.molec.cm2)
Pandora.NO2.short <- Pandora.Aug2 %>%
  dplyr::mutate(location = "Chicago", param = "NO2 TVC") %>%
  dplyr::rename(value = NO2.TVC.molec.cm2)
AERONET.both.short <- AERONET.both.Aug2 %>%
  dplyr::mutate(param = "AOD") %>%
  dplyr::rename(value = AOD) %>%
  dplyr::select(location,Date,datetime.CDT,hour,param,lambda,value)
# write.csv(AERONET.both.short, "AERONET data - CP and NPU - Aug2.csv", row.names = FALSE)

Aug2.combined <- bind_rows(CP.Aug2.short, Schil.Aug2, ORD.met.Aug2, NPU.Pandora.NO2.short) #excludes AERONET - will plot separately to separate by wavelength

#Convert to metric units: AQS winds are in knots (1.94384 knots = 1 m/s). ORD winds are in mph (2.23694 mph = 1 m/s). Deg C = (deg F-32)*5/9. 
Aug2.combined <- Aug2.combined %>%
  dplyr::mutate(value = ifelse(param == "ws", ifelse(location == "Chiwaukee", value/1.94384, value/2.23694), value), 
                value = ifelse(param == "Temp", (value-32)*5/9, value), value = ifelse(param == "Ozone", value*1000, value))

# param_labels <- c(
#   "Temp"      = "Temp~'('*degree*C*')'",
#   "ws"        = "Wnd~spd~'(m/s)'",
#   "wd"        = "Wind~dir.~'('*degree*')'",
#   "Ozone"     = "O[3]~'(ppb)'",
#   "NO2"       = "NO[2]~'(ppb)'",
#   "NO2 TVC"   = "NO[2]~' TVC'",
#   # "NO2 TVC"   = 'atop("NO"[2]~"TVC","(molec/cm"^2*"\u00D710"^15*")")',
#   "PM25"      = "PM[2.5]~'(µg/'*m^3*')'"
# )
param_labels <- c(
  "Temp"      = 'atop("Temp","("*degree*C*")")',
  "ws"        = 'atop("Wind spd","(m/s)")',
  "wd"        = 'atop("Wind dir.","("*degree*")")',
  "Ozone"     = 'atop(O[3],"(ppb)")',
  "NO2"       = 'atop(NO[2],"(ppb)")',
  "NO2 TVC"   = "NO[2]~' TVC'",
  # "NO2 TVC"   = 'atop("NO"[2]~"TVC","(molec/cm"^2*"E15)")',
  # "NO2 TVC"   = 'atop("NO"[2]~"TVC","(molec/cm"^2*"\u00D710"^15*")")',
  "PM25"      = 'atop(PM[2.5],"(µg/"*m^3*")")',
  "AOD"       = 'atop("AOD")'
)

#Define consistent ordering of factors (param)
# Aug2.combined$param <- factor(Aug2.combined$param, levels = c("Temp","ws","wd","Ozone","NO2","NO2 TVC","PM25","AOD"))
param_levels <- c("Temp","ws","wd","Ozone","NO2","NO2 TVC","PM25","AOD")
Aug2.combined$param <- factor(Aug2.combined$param, levels = param_levels)
AERONET.both.short$param <- factor(AERONET.both.short$param, levels = param_levels)

#Define panel labels:

# Compute a global left x (POSIXct) and a tiny nudge inside the panel
x_left  <- min(Aug2.combined$datetime.CDT, na.rm = TRUE)
x_right <- max(Aug2.combined$datetime.CDT, na.rm = TRUE)
x_tag   <- x_left + (x_right - x_left) * 0.01  # 1% in from the left

panel_labels <- expand.grid(location = c("Chicago","Chiwaukee"), param = param_levels, KEEP.OUT.ATTRS = FALSE) %>%
  dplyr::mutate(
    location = factor(location, levels = c("Chicago","Chiwaukee")),  
    param = factor(param, levels = param_levels),
    tag      = paste0("(", letters[seq_len(n())], ")"),
    x        = x_tag,
    y        = Inf
)

    # a <- ggplot() +
    #   geom_line(data=Aug2.combined, aes(x=datetime.CDT, y=value)) +
    #   geom_line(data=AERONET.both.short, aes(x=datetime.CDT, y=value, color = .data[["lambda"]], group = .data[["lambda"]])) +
    #   # panel tags
    #   geom_text(data = panel_labels, aes(x = x, y = y, label = tag), inherit.aes = FALSE, hjust = -0.1, vjust = 1.1, fontface = "bold" ) + # tuck into top-left
    #   facet_grid(rows = vars(param), cols = vars(location), switch = "y", labeller = labeller(param = as_labeller(param_labels, default = label_parsed)),  scales = "free_y") +
    #   scale_x_datetime(date_labels = "%H") + scale_color_hue(direction = -1, h.start=0) +
    #   # scale_x_continuous(limits = c(0,24), breaks = seq(0,24,by=6)) +
    #   xlab("Hour (CDT)") + ylab(NULL) +
    #   theme(strip.placement = "outside",
    #         strip.background = element_blank(),
    #         strip.text = element_text(size=12),
    #         axis.title = element_text(size=14))
    # ggsave("Chicago-Chiwaukee August 2 met-AQ.png", plot = a, width = 6, height = 7)

  



    
    
    
#Prepare daily summary PM2.5 and Ozone MDA8 values to map in ArcGIS -------------------------------------------
#Data were filtered in Excel from AQS reports (daily max report for ozone (AMP350MX) and daily summary report (AMP435) for PM2.5)
    
Aug2.MDA8 <- read.csv("Aug2 O3 MDA8.csv", header = TRUE)
Aug2.PM25 <- read.csv("Aug2 PM25.csv", header = TRUE)

#Import lat/lon to merge with concentrations
O3.lat.lon <- read.csv("AMP480_2315101_DV_Ozone 2001-24.csv", header = TRUE) 
O3.lat.lon <- O3.lat.lon %>%
  dplyr::mutate(Site = paste0(STATE_CODE, str_pad(COUNTY_CODE, 3, pad = "0"), str_pad(SITE_ID, 4, pad = "0"))) %>%
  dplyr::select(Site,LATITUDE,LONGITUDE) %>%
  dplyr::distinct() %>%
  dplyr::rename(Latitude = LATITUDE, Longitude = LONGITUDE)

PM.lat.lon <- read.csv("AMP480_2315102_DV_PM25 2001-24.csv", header = TRUE) 
PM.lat.lon <- PM.lat.lon %>%
  dplyr::mutate(Site = paste0(STATE_CODE, str_pad(COUNTY_CODE, 3, pad = "0"), str_pad(SITE_NUMBER, 4, pad = "0"))) %>%
  dplyr::select(Site,LATITUDE,LONGITUDE) %>%
  dplyr::distinct() %>%
  dplyr::rename(Latitude = LATITUDE, Longitude = LONGITUDE)

Aug2.MDA8.narrow <- Aug2.MDA8 %>%
  dplyr::mutate(Site = paste0(STATE_CODE, str_pad(COUNTY_CODE, 3, pad = "0"), str_pad(SITE_ID, 4, pad = "0")),
                MDA8 = VALUE_0 * 1000) %>%
  dplyr::select(Site,MDA8) %>%
  left_join(., O3.lat.lon, by="Site") %>%
  dplyr::mutate(Latitude = ifelse(Site == "170890005", 42.04914776, Latitude), Longitude = ifelse(Site == "170890005", -88.27302929, Longitude)) #add missing Elgin monitor

Aug2.PM25.narrow <- Aug2.PM25 %>%
  dplyr::mutate(Site = paste0(STATE.CODE, str_pad(COUNTY.CODE, 3, pad = "0"), str_pad(SITE.ID, 4, pad = "0"))) %>%
  dplyr::select(Site,ARITHMETIC.MEAN) %>%
  left_join(., PM.lat.lon, by="Site") %>%
  dplyr::rename(PM25.24h = ARITHMETIC.MEAN)

#Export
write.csv(Aug2.MDA8.narrow, file = "Aug2 O3 MDA8 Chicago area.csv", row.names = FALSE)
write.csv(Aug2.PM25.narrow, file = "Aug2 24hr PM25 Chicago area.csv", row.names = FALSE)   



## August 2nd AEROMMA flight path ----------------------------------------------------

AEROMMA.track <- read.table("./overview map/AEROMMA-MetNav_DC8_20230802_R0-no metadata.txt", sep = ",", header = TRUE)

AEROMMA.track <- AEROMMA.track %>%
  dplyr::select(1,3,4,5)

write.csv(AEROMMA.track, "August 2nd AEROMMA flight route.csv", row.names = FALSE)

    states <- map_data("state")
    LM.states <- subset(states, region %in% c("wisconsin","illinois","indiana","michigan"))

    Chicago.map <- ggplot(data = LM.states, mapping = aes(x=long, y=lat, group=group)) + geom_polygon(color = "black", fill="white") + 
      coord_fixed(xlim=c(-88.7,-86.8), ylim=c(41,43.3), ratio=1.3)   
    
    a <- Chicago.map + geom_point(data = AEROMMA.track, mapping = aes(x=Longitude, y=Latitude, group=1), color="lightblue3", size = 0.5)
    
    
## August 2nd STAQS flight path ------------------------------------------------------
#Use the center pixel from each swath (of 30 pixels) to represent the airplane location. (Note that I tried to select the nadir pixel, but the code wasn't working. This should be good enough for mapping.)
#Note that John Hair pointed out that this is the lat/lon for the GCAS measurements, not the exact flight track. Pulling in the flight track below
# require("M3")
# require("rasterVis")
require(ncdf4)
    
f <- "./overview map/staqs-GCAS-NO2_JSC-GV_20230802_R1.nc"
nc <- nc_open(f)

lon <- as.data.frame(ncvar_get(nc, "lon"))  # 2D
lat <- as.data.frame(ncvar_get(nc, "lat"))  # 2D

#Select just the center pixel of each swath of 30:
center.lon <- lon %>%
  dplyr::slice(15) %>%
  pivot_longer(1:13324, names_to = "swath", values_to = "lon")
center.lat <- lat %>%
  dplyr::slice(15) %>%
  pivot_longer(1:13324, names_to = "swath", values_to = "lat")
STAQS.center <- full_join(center.lon, center.lat, by="swath")

# write.csv(STAQS.center, file = "August 2nd STAQS flight route.csv", row.names = FALSE)

#add on to map

    b <- a + geom_point(data = STAQS.center, mapping = aes(x=lon, y=lat, group=1), color = "pink3", size = 0.1)

    
## August 2nd STAQS flight path (exact flight track, not LIDAR scan location) - in HSRL data, which is .h5 format
# install.packages("BiocManager") #need to install this to install rhdf5 (not hosted on CRAN)
# BiocManager::install("rhdf5")
require(rhdf5)

#open the file
HSRL2.data <- H5Fopen("./overview map/staqs-HSRL2_JSC-GV_20230802_R1.h5")

#Look at what's in the Nav_Data group:
rhdf5::h5ls(HSRL2.data, recursive = TRUE) |> 
  subset(grepl("^/Nav_Data", group))

#Read lat/lon
ground.lat <- rhdf5::h5read("./overview map/staqs-HSRL2_JSC-GV_20230802_R1.h5", "Nav_Data/gps_lat")
ground.lon <- rhdf5::h5read("./overview map/staqs-HSRL2_JSC-GV_20230802_R1.h5", "Nav_Data/gps_lon")

h5closeAll()

#Convert to data frame
ground.lat <- data.frame(ground.lat)
ground.lon <- data.frame(ground.lon)
ground.lat <- pivot_longer(ground.lat, 1:3181, names_to = "X", values_to = "lat")
ground.lon <- pivot_longer(ground.lon, 1:3181, names_to = "X", values_to = "lon")

ground.lat.lon <- full_join(ground.lat, ground.lon, by = "X")

    a <- ggplot(ground.lat.lon, aes(x=lon, y=lat)) + geom_point() #Check - looks good

# write.csv(ground.lat.lon, file = "August 2nd STAQS flight route - ground track.csv", row.names = FALSE)
    

    
    
#August 2nd Searey flight tracks
Searey1 <- read.table("./overview map/STAQS-O3-NO2-PM25-MET_SEAREY_20230802_R0_L1-no header.txt", sep = ",", header = TRUE)
Searey2 <- read.table("./overview map/STAQS-O3-NO2-PM25-MET_SEAREY_20230802_R0_L2-no header.txt", sep = ",", header = TRUE)
Searey3 <- read.table("./overview map/STAQS-O3-NO2-PM25-MET_SEAREY_20230802_R0_L3-no header.txt", sep = ",", header = TRUE)

#Add leg number on
Searey1 <- Searey1 %>%
  dplyr::mutate(leg = 1)
Searey2 <- Searey2 %>%
  dplyr::mutate(leg = 2)
Searey3 <- Searey3 %>%
  dplyr::mutate(leg = 3)

Searey.all <- bind_rows(Searey1, Searey2, Searey3)

write.csv(Searey.all, "August 2nd Searey flight routes.csv", row.names = FALSE)

#add on to map

    c <- b + geom_point(data = Searey.all, mapping = aes(x=Longitude, y=Latitude, group=1), color = "darkgreen", size = 0.1)
    
  
#August 2nd GMAP routes
GMAP.route <- read.csv("AGES23_08_1_12_MA01-update.csv", header = TRUE)
GMAP.route <- GMAP.route %>%
  dplyr::mutate(Date = substr(Time, start=1, stop=8)) %>%
  dplyr::select(10,1,6,7) %>%
  dplyr::rename(Longitude = GPS.Longitude, Latitude = GPS.Latitude)
GMAP.Aug2 <- GMAP.route %>%
  dplyr::filter(Date == "8/2/2023")

write.csv(GMAP.Aug2, "August 2nd GMAP route.csv", row.names = FALSE)

#add on to map

    d <- c + geom_point(data = GMAP.Aug2, mapping = aes(x=Longitude, Latitude, group=1), color="purple", size = 0.1)
    
    
    
## Routine monitoring locations (take from network assessment files - for 2024)
O3.monitors <- read.csv("Ozone_monitors-LADCO.csv", header=TRUE)
O3.monitors <- O3.monitors %>%
  dplyr::filter(State %in% c("Illinois","Indiana","Michigan","Wisconsin")) %>%
  dplyr::select(1,9,10) %>%
  dplyr::mutate(ozone = "X") 
PM25.monitors <- read.csv("PM25_NAAQS_monitors_LADCO-Apr update.csv", header=TRUE)
PM25.monitors <- PM25.monitors %>%
  dplyr::filter(State %in% c("Illinois","Indiana","Michigan","Wisconsin")) %>%
  dplyr::select(1,9,10) %>%
  dplyr::mutate(PM2.5 = "X")
PM25.AQI.monitors <- read.csv("PM25_AQI_monitors.csv", header=TRUE)
PM25.AQI.monitors <- PM25.AQI.monitors %>% #monitors not used for NAAQS compliance (AQI only)
  dplyr::filter(State %in% c("Illinois","Indiana","Michigan","Wisconsin")) %>%
  dplyr::select(2,10,11) %>%
  dplyr::mutate(PM2.5 = "X")
PM25.monitors <- PM25.monitors %>%
  bind_rows(., PM25.AQI.monitors) %>%
  dplyr::distinct()

O3.PM25.mons <- full_join(O3.monitors, PM25.monitors, by=c("AQS_Site_ID", "Longitude", "Latitude"))
O3.PM25.mons <- O3.PM25.mons %>%
  dplyr::mutate(param = ifelse(is.na(ozone), "PM2.5", ifelse(is.na(PM2.5), "O3", "O3 + PM2.5")))

NO2.mons <- read.csv("NO2_monitors.csv", header = TRUE)
NO2.mons <- NO2.mons %>% 
  dplyr::filter(State %in% c("Illinois","Indiana","Michigan","Wisconsin")) %>%
  dplyr::select(2,10,11) %>%
  dplyr::mutate(param = "NO2")

CO.mons <- read.csv("CO_monitors.csv", header = TRUE)
CO.mons <- CO.mons %>% 
  dplyr::filter(State %in% c("Illinois","Indiana","Michigan","Wisconsin")) %>%
  dplyr::select(2,10,11) %>%
  dplyr::mutate(param = "CO")

#Combine VOCs and carbonyls. Note that the NATTS site is also the PAMS site so don't have to load it here.
PAMS.mons <- read.csv("pams_r5_updated.csv", header=TRUE)
PAMS.mons <- PAMS.mons %>% 
  dplyr::filter(State_Name %in% c("Illinois","Indiana","Michigan","Wisconsin")) %>%
  dplyr::select(30,5,6) %>%
  dplyr::rename(site = Site) %>%
  dplyr::mutate(site = gsub("-", "", site))
other.VOC.mons <- read.csv("monitors_vocs.csv", header = TRUE)
other.VOC.mons <- other.VOC.mons %>% 
  dplyr::filter(State_Name %in% c("Illinois","Indiana","Michigan","Wisconsin")) %>%
  dplyr::select(14,2,3) %>%
  dplyr::mutate(site = as.character(site))
other.carbonyls <- read.csv("monitors_carbonyls.csv", header = TRUE)
other.carbonyls <- other.carbonyls %>% 
  dplyr::filter(State_Name %in% c("Illinois","Indiana","Michigan","Wisconsin")) %>%
  dplyr::select(14,2,3) %>%
  dplyr::mutate(site = as.character(site))

all.VOCs.carbonyls <- bind_rows(PAMS.mons, other.VOC.mons, other.carbonyls)
all.VOCs.carbonyls <- all.VOCs.carbonyls %>%
  dplyr::distinct() %>%
  dplyr::mutate(param = "VOCs/carbonyls") %>%
  dplyr::rename(AQS_Site_ID = site)

#Combine all parameters
routine.monitoring <- bind_rows(O3.PM25.mons, NO2.mons, CO.mons, all.VOCs.carbonyls)
routine.monitoring <- routine.monitoring %>%
  dplyr::select(-ozone,-PM2.5)

write.csv(routine.monitoring, "Routine monitoring sites.csv", row.names = FALSE)

#add on to map

    e <- d + geom_point(data = routine.monitoring, mapping = aes(x=Longitude, Latitude, color = param, group=param), size = 4, alpha = 0.7)
 
       
#AGES+ field sites

ground.sites <- read.csv("./overview map/AGES Chicago ground sites.csv", header = TRUE)

    f <- e + geom_point(data = ground.sites, mapping = aes(x=Longitude, y=Latitude, group=1), size = 5, shape = 5, stroke = 2)
    ggsave("AGES Chicago assets map - sketch in R.png", plot = f, width = 7, height = 9)
    
    
    
    
#Prepare timeseries and maps for paper SI --------------------------------------------------------

#Time series plots
    
require(ggh4x) #for fancier faceting

#Load in site names
site.names <- read.csv("R5-plus ozone monitoring names and codes-2023.csv", header = TRUE)
site.names <- dplyr::mutate(site.names, Site = as.character(Site))

addl.sites <- data.frame(Site = c("170310119", "170310219", "170434002", "171971002", "180890034", "180890036"),
                         Site.Name.new = c("Kingery Near Rd", "Kennedy Near Rd", "Naperville", "Joliet", "East Chicago", "Hammond")) #non-ozone site names
site.names <- bind_rows(site.names, addl.sites)

Aug2.for.paper <- Aug.data %>% #need to redo August 2nd selection in CST (above was CDT)
  dplyr::mutate(Date = as.Date(datetime.CST, tz="America/Denver")) %>%
  dplyr::filter(Date == as.Date("2023-08-02", tz="America/Denver"))

Aug2.w.names <- left_join(Aug2.for.paper, site.names, by="Site")
Aug2.w.names <- Aug2.w.names %>%
  dplyr::filter(Site != "181270011") #remove one site that only has met 

#remove PM2.5 for timeseries plots (not very interesting), and RH & ws (also not that interesting)
Aug2.no.PM <- Aug2.w.names %>%
  dplyr::filter(!(param %in% c("PM25","ws","RH")) & !(Site %in% addl.sites$Site)) %>%
  dplyr::mutate(value = ifelse(param == "Ozone", value * 1000, value), 
                State = ifelse(grepl('^17',Site), "IL", ifelse(grepl('^18',Site), "IN", "WI")))

#Convert to metric (Temp: deg F to deg C)
Aug2.no.PM <- Aug2.no.PM %>%
  dplyr::mutate(value = ifelse(param == "Temp", (value - 32)*5/9, value))

Aug2.no.PM$param <- factor(Aug2.no.PM$param, levels = c("Ozone","NO2","Temp","wd"))
Aug2.no.PM$Site.Name.new <- factor(Aug2.no.PM$Site.Name.new, levels = c("Kenosha WT","Chiwaukee","Zion","Cary","Elgin","Northbrook","Des Plaines","Evanston",
                                                                        "Chicago Taft HS","Schiller Park","Cicero","Lisle","Chicago Com Ed","Chicago SWFP",
                                                                        "Alsip","Lemont","Braidwood","Valparaiso","Hammond","Gary-IITRI","Ogden Dunes"))

    # a <- ggplot(Aug2.no.PM, aes(x=hour, y=value, color = param, group = param)) + geom_line() +
    #   facet_nested_wrap(vars(Site.Name.new, param), dir = "v", strip.position = "left", scales = "free_y", ncol = 4,
    #                     nest_line = element_line(linetype = 2)) +
    #   scale_x_continuous(breaks = seq(0,21,by=6), minor_breaks = seq(0,21,by=2)) +
    #   labs(x = "Hour (CST)", y = expression("Concentration (ppb), Temperature (" *degree * "C), or Wind Direction (" *degree * ")")) +
    #   scale_color_brewer(palette = "Set1") +
    #   theme(strip.placement = "outside")
    # ggsave("AQS data - August 2 for paper-metric.png", width = 8, height = 10)


#Plot Ox at sites where available (regulatory + NEIU + Kenosha Water Utility)
NO2.sites <- Aug2.no.PM %>%
  dplyr::filter(param == "NO2") %>%
  dplyr::select(Site) %>%
  dplyr::distinct()

Aug2.NO2.sites <- Aug2.no.PM %>%
  dplyr::filter(Site %in% NO2.sites$Site) %>%
  dplyr::filter(param  %in% c("Ozone","NO2")) %>%
  dplyr::select(Site.Name.new,datetime.CST,param,value) %>%
  dplyr::rename(Site = Site.Name.new) %>%
  dplyr::mutate(param = ifelse(param == "Ozone", "O3", "NO2"))

#Load NEIU data
NEIU.data <- read.csv("NEIU AQ data - Aug2 2023.csv", header = TRUE)
NEIU.data <- NEIU.data %>%
  dplyr::filter(param %in% c("O3","NO2")) %>%
  dplyr::select(site,datetime.CST,param,value) %>%
  dplyr::mutate(site = "NEIU", datetime.CST = as.POSIXct(datetime.CST)) %>%
  dplyr::rename(Site = site)

#Load Kenosha Water Utility (DOAS) data
DOAS.data <- read.csv("./other figures/staqs-Ground-Kenosha-DOAS_OTHER_20230701_R0_thru20230831-no header.csv", header = TRUE)

# Manually define the starting time/origin
origin <- as.POSIXct("2023-07-01 00:00:00", "%Y-%m-%d %H:%M:%S", tz="UTC")

DOAS.data.Aug2 <- DOAS.data %>%
  dplyr::mutate(datetime.UTC = origin + as.difftime(Time_Start, units = "secs"), datetime.CST = as.POSIXct(datetime.UTC, tz = "America/Denver")) %>%
  dplyr::filter(datetime.CST >= as.POSIXct("2023-08-02 00:00:00", tz = "America/Denver") &
                  datetime.CST <  as.POSIXct("2023-08-03 00:00:00", tz = "America/Denver")) %>%
  dplyr::mutate(Site = "Kenosha Harbor") %>%
  dplyr::select(Site, datetime.CST, NO2_ppb, O3_ppb) %>%
  dplyr::rename(NO2 = NO2_ppb, O3 = O3_ppb) %>%
  pivot_longer(NO2:O3, names_to = "param", values_to = "value")

Ox.data <- bind_rows(Aug2.NO2.sites, NEIU.data, DOAS.data.Aug2)
Ox.data$Site <- factor(Ox.data$Site, levels = c("Kenosha Harbor","Chiwaukee","Northbrook","NEIU","Schiller Park","Cicero","Chicago Com Ed","Gary-IITRI"))

    a <- ggplot(Ox.data, aes(x=datetime.CST, y=value, fill=param, group=param)) + geom_area(position = "stack", alpha = 0.5) + facet_wrap(~Site) +
      scale_fill_manual(values = c("blue","goldenrod1")) + scale_x_datetime(date_labels = "%H") + scale_y_continuous(breaks = seq(0,80,by=20)) +
      ylab("Concentration (ppb)") + xlab("Hour (CST)")
    ggsave("Ox plots - Aug2 2023.png", width = 6, height = 4)





#Maps of MDA8 ozone and 24-hour PM2.5

require(sf)
require(ggspatial)
require(prettymapr)
require(colorRamps)
require(ggrepel)
# require(basemaps)

#ozone
MDA8.ozone <- read.csv("Aug2 O3 MDA8 Chicago area.csv", header = TRUE)
MDA8.ozone <- dplyr::mutate(MDA8.ozone, Site = as.character(Site))
MDA8.ozone <- left_join(MDA8.ozone, site.names, by="Site")

MDA8.Chicago <- MDA8.ozone %>%
  dplyr::filter(Latitude > 41.15 & Latitude < 42.75 & Longitude > -88.35 & Longitude < -86.95)

MDA8.for.map <- st_as_sf(MDA8.Chicago, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) #convert to sf object for mapping
MDA8.reproject <- st_transform(MDA8.for.map, 3857) #reproject data

#specify x/y for labels:
labels <- MDA8.reproject %>% 
  dplyr::mutate(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = Site.Name.new)

#AGES Chicago site ozone
AGES.ozone.MDA8 <- read.csv("AGES Chicago sites - O3 MDA8s Aug2.csv", header = TRUE)
AGES.ozone.MDA8 <- AGES.ozone.MDA8 %>%
  dplyr::mutate(avg.8hr = ifelse(site == "Chiwaukee", 72, avg.8hr)) %>% #use the official MDA8 for Chiwaukee
  dplyr::rename(MDA8 = avg.8hr)

AGES.lat.lon <- read.csv("./overview map/AGES Chicago ground sites.csv", header = TRUE)
AGES.lat.lon <- AGES.lat.lon %>%
  dplyr::mutate(Site = ifelse(Site == "Chiwaukee Prairie", "Chiwaukee", ifelse(Site == "Kenosha Water Utility", "Kenosha WU", 
                                                                               ifelse(Site == "ANL", "ATMOS", Site)))) %>%
  dplyr::select(-4)
AGES.ozone.MDA8 <- left_join(AGES.ozone.MDA8, AGES.lat.lon, by=c("site"="Site"))

AGES.MDA8.for.map <- st_as_sf(AGES.ozone.MDA8, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) #convert to sf object for mapping
AGES.MDA8.reproject <- st_transform(AGES.MDA8.for.map, 3857) #reproject data


    a <- ggplot() + annotation_map_tile(type = "cartolight", zoomin = 0) +
      # basemap_gglayer(ext = MDA8.reproject, map_service = "carto", map_type = "positron_no_labels") +
      geom_sf(data = MDA8.reproject, aes(fill = MDA8), shape = 21, color = "black", size = 3, stroke = 0.25) +
      geom_sf(data = AGES.MDA8.reproject, aes(fill = MDA8), shape = 24, color = "black", size = 3, stroke = 0.25) +
      coord_sf(xlim = c(-9840000, -9680000), ylim = c(5030000, 5260000), expand = FALSE) +
      geom_text_repel(data = labels, aes(x = x, y = y, label = label), size = 3, min.segment.length = 0, seed = 123) +
      # scale_fill_distiller(palette = "YlOrRd", direction = 1) +
      scale_fill_viridis_c(option = "C", name = "MDA8\n(ppb)", direction = -1) +
      # scale_fill_gradientn(colors = matlab.like(87), name = "MDA8") +
      ylab(NULL) + xlab(NULL) +
      theme_minimal()
    ggsave("AGES Chicago Ozone MDA8s - reg and special sites.png", width = 6, height = 7)

#PM2.5
PM25.24avg <- read.csv("Aug2 24hr PM25 Chicago area.csv", header = TRUE)
PM25.24avg <- dplyr::mutate(PM25.24avg, Site = as.character(Site))
PM25.24avg <- left_join(PM25.24avg, site.names, by="Site")

PM25.Chicago <- PM25.24avg %>%
  dplyr::filter(Latitude > 41.15 & Latitude < 42.75 & Longitude > -88.35 & Longitude < -86.95) %>%
  dplyr::mutate(Site.Name.new = ifelse(Site == "170313301", "Summit", Site.Name.new)) %>%
  dplyr::distinct() %>%
  dplyr::filter(!(Site.Name.new == "Hammond" & PM25.24h == 11.8)) #drop higher-POC PM2.5 value for Hammond (Other value is used in AirNow Tech)

PM25.for.map <- st_as_sf(PM25.Chicago, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) #convert to sf object for mapping
PM25.reproject <- st_transform(PM25.for.map, 3857) #reproject data

#specify x/y for labels:
labels.PM <- PM25.reproject %>% 
  dplyr::mutate(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = Site.Name.new)

#AGES Chicago site PM2.5
AGES.PM25 <- read.csv("AGES Chicago sites - 24hr PM25 Aug2.csv", header = TRUE)
AGES.PM25 <- AGES.PM25 %>%
  dplyr::rename(PM25.24h = avg.24hr)

AGES.PM25 <- left_join(AGES.PM25, AGES.lat.lon, by=c("site"="Site"))

AGES.PM25.for.map <- st_as_sf(AGES.PM25, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) #convert to sf object for mapping
AGES.PM25.reproject <- st_transform(AGES.PM25.for.map, 3857) #reproject data

#CARE PurpleAir sites
CARE.PM25 <- read.csv("PurpleAirData_forMap_PingJing.csv", header = TRUE)

CARE.PM25.for.map <- st_as_sf(CARE.PM25, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) #convert to sf object for mapping
CARE.PM25.reproject <- st_transform(CARE.PM25.for.map, 3857) #reproject data

#specify x/y for labels:
labels.CARE <- CARE.PM25.reproject %>% 
  dplyr::mutate(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = sensor_index)


    a <- ggplot() + annotation_map_tile(type = "cartolight", zoomin = 0) +
      geom_sf(data = PM25.reproject, aes(fill = PM25.24h), shape = 21, color = "black", size = 3, stroke = 0.25) +
      geom_sf(data = AGES.PM25.reproject, aes(fill = PM25.24h), shape = 24, color = "black", size = 3, stroke = 0.25) +
      geom_sf(data = CARE.PM25.reproject, aes(fill = pm25_mean), shape = 23, color = "black", size = 2, stroke = 0.25) +
      coord_sf(xlim = c(-9840000, -9680000), ylim = c(5030000, 5260000), expand = FALSE) +
      geom_text_repel(data = labels.PM, aes(x = x, y = y, label = label), size = 3, min.segment.length = 0, seed = 123) +
      geom_text_repel(data = labels.CARE, aes(x = x, y = y, label = label), size = 3, min.segment.length = 0, seed = 123) +
      # scale_fill_distiller(palette = "YlOrRd", direction = 1) +
      scale_fill_viridis_c(option = "D", name = "PM2.5\n(ug/m3)", direction = -1) +
      # scale_fill_gradientn(colors = matlab.like(87), name = "MDA8") +
      ylab(NULL) + xlab(NULL) +
      theme_minimal()
    ggsave("AGES Chicago 24hr PM25 - reg and special and PurpleAir sites.png", width = 6, height = 7)






    
 

   