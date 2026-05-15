## Process August 2, 2023 ground timeseries data for AGES+ Chicago paper

require(tidyverse)
require(ncdf4)
require(ggnewscale)
require(see) #for color-blind friendly palette ()
require(cowplot)

setwd("...")


#Load CROCUS data -----------------------------------------------------------------------

#Ozone at ATMOS ---------------
ATMOS.O3 <- ncdf4::nc_open("./other figures/CROCUS data/atmos.O3.20230802.nc")
print(ATMOS.O3)

O3.time <- ncvar_get(ATMOS.O3, "time")
O3.time_units <- ncatt_get(ATMOS.O3, "time", "units")$value

# Manually define the starting time/origin
origin <- as.POSIXct("2023-08-02 00:00:00", "%Y-%m-%d %H:%M:%S", tz="America/Denver")

O3.time.CST <- origin + as.difftime(O3.time, units = "hours")

#Read variables
avg_O3       <- ncvar_get(ATMOS.O3, "avg_O3")
inst_O3      <- ncvar_get(ATMOS.O3, "inst_O3")
gas_pressure <- ncvar_get(ATMOS.O3, "gas_pressure")
pressure     <- ncvar_get(ATMOS.O3, "pressure")

#Combine with time
ATMOS.O3.df <- data.frame(O3.time.CST, avg_O3, inst_O3, gas_pressure, pressure)
ATMOS.O3.df <- ATMOS.O3.df %>%
  dplyr::select(1,3) %>%
  dplyr::rename(datetime.CST = O3.time.CST, O3 = inst_O3) %>%
  pivot_longer(2, names_to = "param", values_to = "value") %>%
  dplyr::mutate(panel = "O3", plot = "plot 2", site = "ATMOS")

#NOx at ATMOS ---------------
ATMOS.NOx <- ncdf4::nc_open("./other figures/CROCUS data/atmos.NOx.20230802.nc")
print(ATMOS.NOx)

NOx.time <- ncvar_get(ATMOS.NOx, "time")
NOx.time_units <- ncatt_get(ATMOS.NOx, "time", "units")$value

# Manually enter the starting time/origin
origin <- as.POSIXct("2023-08-02 00:00:00", "%Y-%m-%d %H:%M:%S", tz="America/Denver")

NOx.time.CST <- origin + as.difftime(NOx.time, units = "hours")

#Read variables
avg_NO       <- ncvar_get(ATMOS.NOx, "avg_NO")
avg_NO2   <- ncvar_get(ATMOS.NOx, "avg_NO2")
avg_NOx <- ncvar_get(ATMOS.NOx, "avg_NOx")
inst_NO <- ncvar_get(ATMOS.NOx, "inst_NO")
inst_NO2 <- ncvar_get(ATMOS.NOx, "inst_NO2")
inst_NOx <- ncvar_get(ATMOS.NOx, "inst_NOx")
gas_pressure <- ncvar_get(ATMOS.NOx, "gas_pressure")

#Combine with time
ATMOS.NOx.df <- data.frame(NOx.time.CST, avg_NO, avg_NO2, avg_NOx, inst_NO, inst_NO2, inst_NOx, gas_pressure)
ATMOS.NOx.df <- ATMOS.NOx.df %>%
  dplyr::select(1,5,6,7) %>%
  dplyr::rename(datetime.CST = NOx.time.CST, NO = inst_NO, NO2 = inst_NO2, NOx = inst_NOx) %>%
  pivot_longer(2:4, names_to = "param", values_to = "value") %>%
  dplyr::mutate(panel = "NOx", plot = "plot 3", site = "ATMOS", value = ifelse(value == 9999, NA, value))

#Met at ATMOS -----------------
ATMOS.met <- read.csv("./other figures/CROCUS data/ATMOS.20230802_15min_data.csv", header = TRUE)
ATMOS.met <- ATMOS.met %>%
  dplyr::mutate(datetime = as.POSIXct(datetime, tz="America/Denver")) %>%
  dplyr::mutate(ws.60m = spdv60m/100, ws.10m = spdV10m/100) %>% #convert cm/s to m/s
  dplyr::select(1,7,28,10,15,29,18,13) %>%
  dplyr::rename(wd.60m = dirV60m, wd.10m = dirV10m, T.60m = TaC_60m, T.10m = TaC_10m, RH.10m = rh_10m, datetime.CST = datetime) %>%
  pivot_longer(2:8, names_to = c("param","height"), names_pattern = "(.*)\\.(.*)") %>%
  dplyr::mutate(plot = "plot 1", site = "ATMOS", panel = param)


#AQ data at NEIU --------------
NEIU.AQ.Aug2 <- ncdf4::nc_open("./other figures/CROCUS data/crocus_neiu_aqt_a1_20230802_000000.nc")
NEIU.AQ.Aug3 <- ncdf4::nc_open("./other figures/CROCUS data/crocus_neiu_aqt_a1_20230803_000000.nc")
print(NEIU.AQ.Aug2)

NEIU.time.Aug2 <- ncvar_get(NEIU.AQ.Aug2, "time")
NEIU.time.Aug2_units <- ncatt_get(NEIU.AQ.Aug2, "time", "units")$value
NEIU.time.Aug2 <- NEIU.time.Aug2/1E9 #convert ns to s
NEIU.time.Aug3 <- ncvar_get(NEIU.AQ.Aug3, "time")
NEIU.time.Aug3_units <- ncatt_get(NEIU.AQ.Aug3, "time", "units")$value
NEIU.time.Aug3 <- NEIU.time.Aug3/1E9 #convert ns to s

# Manually enter the starting time/origin
origin.Aug2 <- as.POSIXct("2023-08-02 00:00:59.585347552", "%Y-%m-%d %H:%M:%S", tz="UTC")
origin.Aug3 <- as.POSIXct("2023-08-03 00:00:59.791904302", "%Y-%m-%d %H:%M:%S", tz="UTC")

NEIU.time.Aug2.UTC <- origin.Aug2 + as.difftime(NEIU.time.Aug2, units = "secs")
NEIU.time.Aug3.UTC <- origin.Aug3 + as.difftime(NEIU.time.Aug3, units = "secs")

#Read variables - Aug 2
PM2.5 <- ncvar_get(NEIU.AQ.Aug2, "pm2.5")
PM1.0 <- ncvar_get(NEIU.AQ.Aug2, "pm1.0")
PM10.0 <- ncvar_get(NEIU.AQ.Aug2, "pm10.0")
NO <- ncvar_get(NEIU.AQ.Aug2, "no")
NO2 <- ncvar_get(NEIU.AQ.Aug2, "no2")
O3 <- ncvar_get(NEIU.AQ.Aug2, "o3")
CO <- ncvar_get(NEIU.AQ.Aug2, "co")
temperature <- ncvar_get(NEIU.AQ.Aug2, "temperature")
humidity <- ncvar_get(NEIU.AQ.Aug2, "humidity")
pressure <- ncvar_get(NEIU.AQ.Aug2, "pressure")
dewpoint <- ncvar_get(NEIU.AQ.Aug2, "dewpoint")

#Combine with time - Aug 2
NEIU.AQ.Aug2.df <- data.frame(NEIU.time.Aug2.UTC, PM2.5, PM1.0, PM10.0, NO, NO2, O3, CO, temperature, humidity, pressure, dewpoint)
NEIU.AQ.Aug2.df <- NEIU.AQ.Aug2.df %>%
  dplyr::rename(datetime.UTC = NEIU.time.Aug2.UTC)

#Read variables - Aug 3
PM2.5 <- ncvar_get(NEIU.AQ.Aug3, "pm2.5")
PM1.0 <- ncvar_get(NEIU.AQ.Aug3, "pm1.0")
PM10.0 <- ncvar_get(NEIU.AQ.Aug3, "pm10.0")
NO <- ncvar_get(NEIU.AQ.Aug3, "no")
NO2 <- ncvar_get(NEIU.AQ.Aug3, "no2")
O3 <- ncvar_get(NEIU.AQ.Aug3, "o3")
CO <- ncvar_get(NEIU.AQ.Aug3, "co")
temperature <- ncvar_get(NEIU.AQ.Aug3, "temperature")
humidity <- ncvar_get(NEIU.AQ.Aug3, "humidity")
pressure <- ncvar_get(NEIU.AQ.Aug3, "pressure")
dewpoint <- ncvar_get(NEIU.AQ.Aug3, "dewpoint")

#Combine with time - Aug 3
NEIU.AQ.Aug3.df <- data.frame(NEIU.time.Aug3.UTC, PM2.5, PM1.0, PM10.0, NO, NO2, O3, CO, temperature, humidity, pressure, dewpoint)
NEIU.AQ.Aug3.df <- NEIU.AQ.Aug3.df %>%
  dplyr::rename(datetime.UTC = NEIU.time.Aug3.UTC)

#Combine both days
NEIU.AQ <- bind_rows(NEIU.AQ.Aug2.df, NEIU.AQ.Aug3.df)
NEIU.AQ <- NEIU.AQ %>%
  dplyr::mutate(O3 = O3*1000, NO = NO*1000, NO2 = NO2*1000, #convert ppm to ppb (keep CO in ppb)
                datetime.CST = as.POSIXct(datetime.UTC, tz="America/Denver"), Date = as.Date(datetime.CST, tz="America/Denver")) %>%
  dplyr::filter(Date == as.Date("2023-08-02")) %>%
  dplyr::select(13,2:8) %>%
  pivot_longer(2:8, names_to = "param", values_to = "value") %>%
  dplyr::mutate(panel = ifelse(param %in% c("NO","NO2"), "NOx", ifelse(param %in% c("PM2.5","PM1.0","PM10.0"), "PM", param)), site = "NEIU/North Park U",
                plot = ifelse(panel %in% c("O3","CO"), "plot 2", ifelse(panel == "NOx", "plot 3", "plot 5")))
# write.csv(NEIU.AQ, "NEIU AQ data - Aug2 2023.csv", row.names = FALSE)
  
#Met data at NEIU --------------
NEIU.met.Aug2 <- ncdf4::nc_open("./other figures/CROCUS data/crocus_neiu_wxt_a1_20230802_000000.nc")
NEIU.met.Aug3 <- ncdf4::nc_open("./other figures/CROCUS data/crocus_neiu_wxt_a1_20230803_000000.nc")
print(NEIU.met.Aug2)

NEIU.time.Aug2 <- ncvar_get(NEIU.met.Aug2, "time")
NEIU.time.Aug2_units <- ncatt_get(NEIU.met.Aug2, "time", "units")$value
NEIU.time.Aug3 <- ncvar_get(NEIU.met.Aug3, "time")
NEIU.time.Aug3_units <- ncatt_get(NEIU.met.Aug3, "time", "units")$value

# Manually enter the starting time/origin
origin.Aug2 <- as.POSIXct("2023-08-02 00:00:00", "%Y-%m-%d %H:%M:%S", tz="UTC")
origin.Aug3 <- as.POSIXct("2023-08-03 00:00:00", "%Y-%m-%d %H:%M:%S", tz="UTC")

NEIU.time.Aug2.UTC <- origin.Aug2 + as.difftime(NEIU.time.Aug2, units = "secs")
NEIU.time.Aug3.UTC <- origin.Aug3 + as.difftime(NEIU.time.Aug3, units = "secs")

#Read variables - Aug 2
temperature <- ncvar_get(NEIU.met.Aug2, "temperature")
humidity <- ncvar_get(NEIU.met.Aug2, "humidity")
pressure <- ncvar_get(NEIU.met.Aug2, "pressure")
rainfall <- ncvar_get(NEIU.met.Aug2, "rainfall")
dewpoint <- ncvar_get(NEIU.met.Aug2, "dewpoint")
wetbulb <- ncvar_get(NEIU.met.Aug2, "wetbulb")
wd <- ncvar_get(NEIU.met.Aug2, "wind_dir_10s")
ws <- ncvar_get(NEIU.met.Aug2, "wind_mean_10s")
ws.max <- ncvar_get(NEIU.met.Aug2, "wind_max_10s")

#Combine with time - Aug 2
NEIU.met.Aug2.df <- data.frame(NEIU.time.Aug2.UTC, temperature, humidity, pressure, rainfall, dewpoint, wetbulb, wd, ws, ws.max)
NEIU.met.Aug2.df <- NEIU.met.Aug2.df %>%
  dplyr::rename(datetime.UTC = NEIU.time.Aug2.UTC)

#Read variables - Aug 3
temperature <- ncvar_get(NEIU.met.Aug3, "temperature")
humidity <- ncvar_get(NEIU.met.Aug3, "humidity")
pressure <- ncvar_get(NEIU.met.Aug3, "pressure")
rainfall <- ncvar_get(NEIU.met.Aug3, "rainfall")
dewpoint <- ncvar_get(NEIU.met.Aug3, "dewpoint")
wetbulb <- ncvar_get(NEIU.met.Aug3, "wetbulb")
wd <- ncvar_get(NEIU.met.Aug3, "wind_dir_10s")
ws <- ncvar_get(NEIU.met.Aug3, "wind_mean_10s")
ws.max <- ncvar_get(NEIU.met.Aug3, "wind_max_10s")

#Combine with time - Aug 3
NEIU.met.Aug3.df <- data.frame(NEIU.time.Aug3.UTC, temperature, humidity, pressure, rainfall, dewpoint, wetbulb, wd, ws, ws.max)
NEIU.met.Aug3.df <- NEIU.met.Aug3.df %>%
  dplyr::rename(datetime.UTC = NEIU.time.Aug3.UTC)

#Combine both days
NEIU.met <- bind_rows(NEIU.met.Aug2.df, NEIU.met.Aug3.df)
NEIU.met <- NEIU.met %>%
  dplyr::mutate(datetime.CST = as.POSIXct(datetime.UTC, tz="America/Denver"), Date = as.Date(datetime.CST, tz="America/Denver")) %>%
  dplyr::filter(Date == as.Date("2023-08-02")) %>%
  dplyr::select(11,2,3,9,8) %>%
  dplyr::rename(T = temperature, RH = humidity) %>%
  pivot_longer(2:5, names_to = "param", values_to = "value") %>%
  dplyr::mutate(panel = param, site = "NEIU/North Park U", plot = "plot 1", height = "16m")
#Calculate 1-minute averages (from 10-s values)
NEIU.met.min <- NEIU.met %>%
  dplyr::select(-panel) %>%
  pivot_wider(names_from = "param", values_from = "value") %>%
  dplyr::mutate(time.min = substr(as.character(datetime.CST), start = 12, stop = 16), time.min = ifelse(time.min == "", "00:00", time.min),
                u.wind = -abs(ws)*sin(wd*pi/180), v.wind = -abs(ws)*cos(wd*pi/180)) %>%
  dplyr::group_by(time.min,site,plot,height) %>%
  dplyr::summarise(T = mean(T, na.rm = TRUE), RH = mean(RH, na.rm = TRUE), ws = mean(ws, na.rm = TRUE), 
                   wd = atan2(mean(u.wind, na.rm=TRUE),mean(v.wind, na.rm=TRUE))*180/pi + 180) %>%
  # dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(datetime.CST = as.POSIXct(paste0("2023-08-02 ", time.min), "%Y-%m-%d %H:%M", tz="America/Denver")) %>%
  dplyr::select(2:4,9,5:8) %>%
  pivot_longer(T:wd, names_to = "param", values_to = "value") %>%
  dplyr::mutate(panel = param)


  
  
## Chiwaukee ground data -------------------------
Chiwaukee.data <- read.csv("./other figures/Chiwaukee_Summer_2023_1MinuteData.csv", header = TRUE)
Chiwaukee.data <- Chiwaukee.data %>%
  dplyr::mutate(datetime.CST = as.POSIXct(DateTime, tz="America/Denver"), Date = as.Date(datetime.CST, tz="America/Denver"), 
                T = (TEMP..degF.-32)*5/9, WS.m.s = WS..mph.*0.44704) %>% #convert to metric units (deg C and m/s)
  dplyr::filter(Date == as.Date("2023-08-02")) %>%
  dplyr::select(12,14,10,15,2:6) %>%
  dplyr::rename(wd = WD..deg., ws = WS.m.s, CO = CO..ppm., NO2 = NO2_CAPS..ppb., O3 = O3..ppb., PM2.5 = PM2.5..ug.m3., PM10.0 = PM10..ug.m3.) %>%
  pivot_longer(2:9, names_to = "param", values_to = "value") %>%
  dplyr::mutate(panel = ifelse(param == "NO2", "NOx", ifelse(param %in% c("PM2.5","PM10.0"), "PM", param)), 
                site = "Chiwaukee", 
                plot = ifelse(param %in% c("T","wd","ws"), "plot 1", ifelse(param %in% c("CO","O3"), "plot 2", ifelse(param == "NO2", "plot 3", "plot 5"))),
                height = ifelse(plot == "plot 1", "8.5m", NA),
                value = ifelse(value == -999, NA, value))


## Kenosha Harbor ground data (DOAS and KNSW weather station) ----------------------

DOAS.data <- read.csv("./other figures/staqs-Ground-Kenosha-DOAS_OTHER_20230701_R0_thru20230831-no header.csv", header = TRUE)

# Manually define the starting time/origin
origin <- as.POSIXct("2023-07-01 00:00:00", "%Y-%m-%d %H:%M:%S", tz="UTC")

DOAS.data.Aug2 <- DOAS.data %>%
  dplyr::mutate(datetime.UTC = origin + as.difftime(Time_Start, units = "secs"), datetime.CST = as.POSIXct(datetime.UTC, tz = "America/Denver")) %>%
  dplyr::filter(datetime.CST >= as.POSIXct("2023-08-02 00:00:00", tz = "America/Denver") &
                  datetime.CST <  as.POSIXct("2023-08-03 00:00:00", tz = "America/Denver")) %>%
  dplyr::select(14,10,11) %>%
  dplyr::rename(NO2 = NO2_ppb, O3 = O3_ppb) %>%
  pivot_longer(NO2:O3, names_to = "param", values_to = "value") %>%
  dplyr::mutate(site = "Kenosha Harbor", plot = ifelse(param == "O3", "plot 2", "plot 3"), panel = ifelse(param == "O3", "O3", "NOx"))

KNSW.data <- read.table("./other figures/staqs-Ground-Kenosha-KNSW3_OTHER_20230701_R0_thru20230831-no header.txt", sep=",", header = TRUE)

KNSW.data.Aug2 <- KNSW.data %>%
  dplyr::mutate(datetime.UTC = origin + as.difftime(Time_Start, units = "secs"), datetime.CST = as.POSIXct(datetime.UTC, tz = "America/Denver")) %>%
  dplyr::filter(datetime.CST >= as.POSIXct("2023-08-02 00:00:00", tz = "America/Denver") &
                  datetime.CST <  as.POSIXct("2023-08-03 00:00:00", tz = "America/Denver")) %>%
  dplyr::select(9,3,4,7) %>%
  dplyr::rename(wd = WDIR, ws = WSPD, T = ATMP) %>%
  pivot_longer(wd:T, names_to = "param", values_to = "value") %>%
  dplyr::mutate(site = "Kenosha Harbor", plot = "plot 1", panel = param)


## Combine ground monitoring data then separate for plots------------------------

all.ground.data <- bind_rows(ATMOS.met, ATMOS.NOx.df, ATMOS.O3.df, NEIU.met.min, NEIU.AQ, Chiwaukee.data, DOAS.data.Aug2, KNSW.data.Aug2)
# all.ground.data <- bind_rows(ATMOS.met, ATMOS.NOx.df, ATMOS.O3.df, NEIU.met.min, NEIU.AQ, Chiwaukee.data)

met.plot.data <- dplyr::filter(all.ground.data, plot == "plot 1")
O3.CO.plot.data <- dplyr::filter(all.ground.data, plot == "plot 2")
NOx.plot.data <- dplyr::filter(all.ground.data, plot == "plot 3")
PM.plot.data <- dplyr::filter(all.ground.data, plot == "plot 5")

# wd.check <- all.ground.data %>%
#   dplyr::filter(datetime.CST < as.POSIXct("2023-08-02 11:00:00", tz="America/Denver"))

## Pandora and AERONET data ---------------------

Pandora.NPU <- read.csv("Pandora data raw- NPU - Aug2.csv", header = TRUE)
Pandora.NPU <- Pandora.NPU %>%
  dplyr::mutate(datetime.CDT = as.POSIXct(datetime.CDT, tz="America/Chicago"), datetime.CST = as.POSIXct(datetime.CDT, tz="America/Denver")) %>%
  dplyr::select(datetime.CST, NO2.TVC.molec.cm2) %>%
  dplyr::rename(value = NO2.TVC.molec.cm2) %>%
  dplyr::mutate(param = "NO2 TVC", panel = "NO2 TVC", site = "NEIU/North Park U", plot = "plot 4")

AERONET.data <- read.csv("AERONET data - CP and NPU - Aug2.csv", header = TRUE)
AERONET.data <- AERONET.data %>%
  dplyr::mutate(datetime.CDT = as.POSIXct(datetime.CDT, tz="America/Chicago"), datetime.CST = as.POSIXct(datetime.CDT, tz="America/Denver")) %>%
  dplyr::mutate(panel = param, site = ifelse(location == "Chicago", "NEIU/North Park U", "Chiwaukee"), plot = "plot 6") %>%
  dplyr::select(datetime.CST, param, lambda, value, panel, site, plot)


#Make multi-paneled plot ---------------------------

#Define consistent ordering of factors (panel)
param_levels <- c("T","RH","ws","wd","O3","CO","NOx","NO2 TVC","PM","AOD")
met.plot.data$panel <- factor(met.plot.data$panel, levels = param_levels)
O3.CO.plot.data$panel <- factor(O3.CO.plot.data$panel, levels = param_levels)
NOx.plot.data$panel <- factor(NOx.plot.data$panel, levels = param_levels)
Pandora.NPU$panel <- factor(Pandora.NPU$panel, levels = param_levels)
PM.plot.data$panel <- factor(PM.plot.data$panel, levels = param_levels)
AERONET.data$panel <- factor(AERONET.data$panel, levels = param_levels)

row_labels <- c(
  "T"      = 'atop("Temp","("*degree*C*")")',
  "RH"     = 'atop("RH","(%)")',
  "ws"        = 'atop("Wind spd","(m/s)")',
  "wd"        = 'atop("Wind dir.","("*degree*")")',
  "O3"     = 'atop(O[3],"(ppb)")',
  "CO"     = 'atop("CO","(ppb)")',
  "NOx"       = 'atop(NO[x],"(ppb)")',
  "NO2 TVC"   = "NO[2]~' TVC'",
  # "NO2 TVC"   = 'atop("NO"[2]~"TVC","(molec/cm"^2*"E15)")',
  # "NO2 TVC"   = 'atop("NO"[2]~"TVC","(molec/cm"^2*"\u00D710"^15*")")',
  "PM"      = 'atop("PM","(µg/"*m^3*")")',
  "AOD"       = "AOD"
)

#Define consistent ordering of factors (site)
site_levels <- c("NEIU/North Park U", "ATMOS", "Chiwaukee", "Kenosha Harbor")
met.plot.data$site <- factor(met.plot.data$site, levels = site_levels)
O3.CO.plot.data$site <- factor(O3.CO.plot.data$site, levels = site_levels)
NOx.plot.data$site <- factor(NOx.plot.data$site, levels = site_levels)
Pandora.NPU$site <- factor(Pandora.NPU$site, levels = site_levels)
PM.plot.data$site <- factor(PM.plot.data$site, levels = site_levels)
AERONET.data$site <- factor(AERONET.data$site, levels = site_levels)

AERONET.data$lambda <- factor(AERONET.data$lambda, levels = c("340nm","380nm","440nm","500nm","675nm","870nm","1020nm","1640nm"))

#Define panel labels:

# Compute a global left x (POSIXct) and a tiny nudge inside the panel
x_left  <- min(met.plot.data$datetime.CST, na.rm = TRUE)
x_right <- max(met.plot.data$datetime.CST, na.rm = TRUE)
x_tag   <- x_left + (x_right - x_left) * 0.01  # 1% in from the left

panel_labels <- expand.grid(site = site_levels, panel = param_levels, KEEP.OUT.ATTRS = FALSE) %>%
  dplyr::mutate(
    site = factor(site, levels = site_levels),  
    panel = factor(panel, levels = param_levels),
    tag      = paste0("(", c(letters[seq_len(26)],"aa","ab","ac","ad"), ")"),
    x        = x_tag,
    y        = Inf
  )

# #One big plot with all parameters - too big!
#     a <- ggplot() +
#       geom_line(data=subset(met.plot.data, site != "ATMOS"), aes(x=datetime.CST, y=value, group = .data[["height"]]), color = "black") +
#       geom_line(data=subset(met.plot.data, site == "ATMOS"), aes(x=datetime.CST, y=value, color = .data[["height"]], group = .data[["height"]])) +
#       scale_color_brewer(palette = "Set1", guide = guide_legend(order = 1)) + ggnewscale::new_scale_color() +
#       # geom_line(data=met.plot.data, aes(x=datetime.CST, y=value, color = .data[["height"]], group = .data[["height"]])) +
#       # scale_color_brewer(palette = "Set1") + ggnewscale::new_scale_color() +
#       geom_line(data=O3.CO.plot.data, aes(x=datetime.CST, y=value), color = "black") +
#       geom_line(data=NOx.plot.data, aes(x=datetime.CST, y=value, color = .data[["param"]], group = .data[["param"]])) +
#       scale_color_brewer(palette = "Set2", guide = guide_legend(order = 2)) + ggnewscale::new_scale_color() +
#       geom_line(data=Pandora.NPU, aes(x=datetime.CST, y=value), color = "black") +
#       geom_line(data=PM.plot.data, aes(x=datetime.CST, y=value, color = .data[["param"]], group = .data[["param"]])) +
#       scale_color_brewer(palette = "Dark2", guide = guide_legend(order = 3)) + ggnewscale::new_scale_color() +
#       geom_line(data=AERONET.data, aes(x=datetime.CST, y=value, color = .data[["lambda"]], group = .data[["lambda"]])) +
#       scale_color_hue(direction = -1, h.start=0, guide = guide_legend(order = 4)) + ggnewscale::new_scale_color() +
#       # # panel tags
#       geom_text(data = panel_labels, aes(x = x, y = y, label = tag), inherit.aes = FALSE, hjust = -0.1, vjust = 1.1, fontface = "bold" ) + # tuck into top-left
#       # facet_grid(rows = vars(panel), cols = vars(site), switch = "y", scales = "free_y") +
#       facet_grid(rows = vars(panel), cols = vars(site), switch = "y", labeller = labeller(panel = as_labeller(row_labels, default = label_parsed)),  scales = "free_y") +
#       scale_x_datetime(date_labels = "%H") +
#       # scale_x_continuous(limits = c(0,24), breaks = seq(0,24,by=6)) +
#       xlab("Hour (CST)") + ylab(NULL) +
#       theme(strip.placement = "outside",
#             strip.background = element_blank(),
#             strip.text = element_text(size=12),
#             axis.title = element_text(size=14),
#             panel.background = element_rect(fill = "white", color = "black"),
#             panel.grid = element_line(color = "gray85"),
#             legend.title = element_blank(),
#             legend.key = element_rect(fill = "transparent", color = NA))
#     ggsave("Chicago-ATMOS-Chiwaukee August 2 met-AQ.png", plot = a, width = 8, height = 9)
#     
# #Do separate met plot
#     a <- ggplot() +
#       geom_line(data=subset(met.plot.data, site != "ATMOS"), aes(x=datetime.CST, y=value, group = .data[["height"]]), color = "black") +
#       geom_line(data=subset(met.plot.data, site == "ATMOS"), aes(x=datetime.CST, y=value, color = .data[["height"]], group = .data[["height"]])) +
#       scale_color_brewer(palette = "Set1", guide = guide_legend(order = 1)) + ggnewscale::new_scale_color() +
#       # panel tags
#       # geom_text(data = panel_labels, aes(x = x, y = y, label = tag), inherit.aes = FALSE, hjust = -0.1, vjust = 1.1, fontface = "bold" ) + # tuck into top-left
#       # facet_grid(rows = vars(panel), cols = vars(site), switch = "y", scales = "free_y") +
#       facet_grid(rows = vars(panel), cols = vars(site), switch = "y", labeller = labeller(panel = as_labeller(row_labels, default = label_parsed)),  scales = "free_y") +
#       scale_x_datetime(date_labels = "%H") +
#       # scale_x_continuous(limits = c(0,24), breaks = seq(0,24,by=6)) +
#       xlab("Hour (CST)") + ylab(NULL) +
#       theme(strip.placement = "outside",
#             strip.background = element_blank(),
#             strip.text = element_text(size=12),
#             axis.title = element_text(size=14),
#             panel.background = element_rect(fill = "white", color = "black"),
#             panel.grid = element_line(color = "gray85"),
#             legend.title = element_blank(),
#             legend.key = element_rect(fill = "transparent", color = NA))
#     ggsave("Chicago-ATMOS-Chiwaukee August 2 met only.png", plot = a, width = 10, height = 6)

#Make separate AQ plot

row_labels <- c(
  "O3"     = "O[3]~'(ppb)'",
  "CO"     = "CO~'(ppb)'",
  "NOx"       = "NO[x]~'(ppb)'",
  "NO2 TVC"   = "NO[2]~' TVC'",
  "PM"      = "PM~'(µg/'*m^3*')'",
  "AOD"       = "AOD"
)

param_levels_AQ <- c("O3","CO","NOx","NO2 TVC","PM","AOD")

panel_labels <- expand.grid(site = site_levels, panel = param_levels_AQ, KEEP.OUT.ATTRS = FALSE) %>%
  dplyr::mutate(
    site = factor(site, levels = site_levels),  
    param = factor(panel, levels = param_levels_AQ),
    tag      = paste0("(", letters[seq_len(n())], ")"),
    x        = x_tag,
    y        = Inf
  )


    # a <- ggplot() +
    #   geom_line(data=O3.CO.plot.data, aes(x=datetime.CST, y=value), color = "black") +
    #   geom_line(data=NOx.plot.data, aes(x=datetime.CST, y=value, color = .data[["param"]], group = .data[["param"]])) +
    #   scale_color_brewer(palette = "Set1", guide = guide_legend(order = 2)) + ggnewscale::new_scale_color() +
    #   geom_line(data=Pandora.NPU, aes(x=datetime.CST, y=value), color = "black") +
    #   geom_line(data=PM.plot.data, aes(x=datetime.CST, y=value, color = .data[["param"]], group = .data[["param"]])) +
    #   scale_color_brewer(palette = "Dark2", guide = guide_legend(order = 3)) + ggnewscale::new_scale_color() +
    #   geom_line(data=AERONET.data, aes(x=datetime.CST, y=value, color = .data[["lambda"]], group = .data[["lambda"]])) +
    #   scale_color_hue(direction = -1, h.start=0, guide = guide_legend(order = 4)) + ggnewscale::new_scale_color() +
    #   # panel tags
    #   geom_text(data = panel_labels, aes(x = x, y = y, label = tag), inherit.aes = FALSE, hjust = -0.1, vjust = 1.1, fontface = "bold" ) + # tuck into top-left
    #   # facet_grid(rows = vars(panel), cols = vars(site), switch = "y", scales = "free_y") +
    #   facet_grid(rows = vars(panel), cols = vars(site), switch = "y", labeller = labeller(panel = as_labeller(row_labels, default = label_parsed)),  scales = "free_y") +
    #   scale_x_datetime(date_labels = "%H") + 
    #   # scale_x_continuous(limits = c(0,24), breaks = seq(0,24,by=6)) +
    #   xlab("Hour (CST)") + ylab(NULL) +
    #   theme(strip.placement = "outside",
    #         strip.background = element_blank(),
    #         strip.text = element_text(size=12),
    #         axis.title = element_text(size=14),
    #         panel.background = element_rect(fill = "white", color = "black"),
    #         panel.grid = element_line(color = "gray85"),
    #         legend.title = element_blank(),
    #         legend.key = element_rect(fill = "transparent", color = NA))
    # ggsave("Chicago-ATMOS-Chiwaukee August 2 AQ only.png", plot = a, width = 10, height = 7)


#Make AQ plot for the paper - save each row separately to make legends line up and keep a consistent distance between rows

#Function to pad the y-axis labels (to ensure consistent width between figures)
pad_fig <- function(x, width = 4) {
  # Pad to one decimal place
  s <- sprintf("%.1f", x)
  n_pad <- pmax(0, width - nchar(s))
  paste0(strrep("\u2007", n_pad), s)
}

    ozone <- ggplot() +
      geom_line(data=subset(O3.CO.plot.data, param == "O3"), aes(x=datetime.CST, y=value), color = "black") + 
      facet_grid(cols = vars(site), scales = "free_y") +
      xlab(NULL) + labs(y = expression("O"[3]*" (ppb)"^phantom(2))) + #Note that "^phantom(2)" adds a phantom superscript to make the x-labels line up (PM has a superscript)
      scale_x_datetime(date_minor_breaks = "2 hours") + scale_y_continuous(labels = function(x) pad_fig(x, width = 4)) +
      theme(strip.placement = "outside",
            strip.background = element_blank(),
            strip.text = element_text(size=14),
            axis.text.x = element_blank(),
            axis.title.y = element_text(size=14),
            axis.text.y = element_text(size = 12),
            panel.background = element_rect(fill = "white", color = "black"),
            panel.grid = element_line(color = "gray85"),
            panel.spacing = unit(1, "lines"),
            legend.title = element_blank(),
            legend.text = element_text(size = 10),
            legend.key = element_rect(fill = "transparent", color = NA))

    CO <- ggplot() +
      geom_line(data=subset(O3.CO.plot.data, param == "CO"), aes(x=datetime.CST, y=value), color = "black") + 
      facet_grid(cols = vars(site), scales = "free_y", drop = FALSE) +
      xlab(NULL) + labs(y = expression("CO (ppb)"^phantom(2))) + 
      scale_x_datetime(date_minor_breaks = "2 hours") + scale_y_continuous(labels = function(x) pad_fig(x, width = 5)) +
      theme(strip.placement = "outside",
            strip.background = element_blank(),
            strip.text = element_blank(),
            axis.text.x = element_blank(),
            axis.title.y = element_text(size=14),
            axis.text.y = element_text(size = 12),
            panel.background = element_rect(fill = "white", color = "black"),
            panel.grid = element_line(color = "gray85"),
            panel.spacing = unit(1, "lines"),
            legend.title = element_blank(),
            legend.text = element_text(size = 10),
            legend.key = element_rect(fill = "transparent", color = NA))

NOx_labels <- c(
  "NO" = expression(NO),
  "NO2" = expression(NO[2]),
  "NOx"  = expression(NO[x]))
    
    NOx <- ggplot() +
      geom_line(data=NOx.plot.data, aes(x=datetime.CST, y=value, color = .data[["param"]], group = .data[["param"]])) +
      # scale_color_brewer(palette = "Set1", guide = guide_legend(order = 2)) + 
      # scale_color_okabeito(palette = "full", labels = NOx_labels) + 
      scale_color_viridis_d(option = "D", begin = 0, end = 0.75, labels = NOx_labels) +
      scale_x_datetime(date_minor_breaks = "2 hours") + scale_y_continuous(labels = function(x) pad_fig(x, width = 4)) +
      facet_grid(cols = vars(site), scales = "free_y", drop = FALSE) +
      xlab(NULL) + labs(y = expression("NO"[x]*" (ppb)"^phantom(2))) +
      theme(strip.placement = "outside",
            strip.background = element_blank(),
            strip.text = element_blank(),
            axis.text.x = element_blank(),
            axis.title.y = element_text(size=14),
            axis.text.y = element_text(size = 12),
            panel.grid = element_line(color = "gray85"),
            panel.background = element_rect(fill = "white", color = "black"),
            panel.spacing = unit(1, "lines"),
            legend.title = element_blank(),
            legend.text = element_text(size = 10),
            legend.key = element_rect(fill = "transparent", color = NA))
    
    NO2.TVC <- ggplot() +
      geom_line(data=Pandora.NPU, aes(x=datetime.CST, y=value), color = "black") +
      facet_grid(cols = vars(site), scales = "free_y", drop = FALSE) +
      scale_x_datetime(limits = c(as.POSIXct("2023-08-02 00:00:00", tz="Etc/GMT+6"), as.POSIXct("2023-08-03 00:00:00", tz="Etc/GMT+6")), date_minor_breaks = "2 hours") +
      scale_y_continuous(breaks = seq(10,18,by=2), labels = function(x) pad_fig(x, width = 4)) +
      xlab(NULL) + labs(y = expression("NO"[2]*" TVC"^phantom(2))) + 
      theme(strip.placement = "outside",
            strip.background = element_blank(),
            strip.text = element_blank(),
            axis.text.x = element_blank(),
            axis.title.y = element_text(size=14),
            axis.text.y = element_text(size = 12),
            panel.grid = element_line(color = "gray85"),
            panel.background = element_rect(fill = "white", color = "black"),
            panel.spacing = unit(1, "lines"),
            legend.title = element_blank(),
            legend.text = element_text(size = 10),
            legend.key = element_rect(fill = "transparent", color = NA))

PM_labels <- c(
  "PM1.0" = expression(PM[1.0]),
  "PM2.5" = expression(PM[2.5]),
  "PM10.0"  = expression(PM[10]))
PM.plot.data$param <- factor(PM.plot.data$param, c("PM1.0","PM2.5","PM10.0"))
    
    PM <- ggplot() +
      geom_line(data=PM.plot.data, aes(x=datetime.CST, y=value, color = .data[["param"]], group = .data[["param"]])) +
      # scale_color_brewer(palette = "Dark2", guide = guide_legend(order = 3)) + 
      # scale_color_okabeito(palette = "full", labels = PM_labels) + 
      scale_color_viridis_d(option = "D", begin = 0, end = 0.75, labels = PM_labels) +
      scale_x_datetime(date_minor_breaks = "2 hours") + scale_y_continuous(labels = function(x) pad_fig(x, width = 4)) +
      facet_grid(cols = vars(site), scales = "free_y", drop = FALSE) +
      xlab(NULL) + labs(y = expression("PM ("*mu*"g/m"^3*")")) +
      theme(strip.placement = "outside",
            strip.background = element_blank(),
            strip.text = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size=14),
            axis.text.y = element_text(size = 12),
            panel.background = element_rect(fill = "white", color = "black"),
            panel.grid = element_line(color = "gray85"),
            panel.spacing = unit(1, "lines"),
            legend.title = element_blank(),
            legend.text = element_text(size = 10),
            legend.key = element_rect(fill = "transparent", color = NA))
    
    AOD <- ggplot() +
      geom_line(data=subset(AERONET.data, lambda %in% c("340nm","500nm","870nm","1640nm")), aes(x=datetime.CST, y=value, color = .data[["lambda"]], group = .data[["lambda"]])) +
      # scale_color_hue(direction = -1, h.start=0, guide = guide_legend(order = 4)) + 
      # scale_color_okabeito(palette = "full") + 
      scale_color_viridis_d(option = "D", begin = 0, end = 1) +
      scale_y_continuous(labels = function(x) pad_fig(x, width = 6)) +
      facet_grid(cols = vars(site), scales = "free_y", drop = FALSE) +
      scale_x_datetime(limits = c(as.POSIXct("2023-08-02 00:00:00", tz="Etc/GMT+6"), as.POSIXct("2023-08-02 23:55:00", tz="Etc/GMT+6")), 
                       date_minor_breaks = "2 hours", date_labels = "%H:%M") +
      xlab("Time (CST)") + labs(y = expression("AOD"^phantom(2))) +
      theme(strip.placement = "outside",
            strip.background = element_blank(),
            strip.text = element_blank(),
            axis.title = element_text(size=14),
            axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5),
            axis.text.y = element_text(size = 12),
            panel.background = element_rect(fill = "white", color = "black"),
            panel.grid = element_line(color = "gray85"),
            panel.spacing = unit(1, "lines"),
            legend.title = element_blank(),
            legend.text = element_text(size = 10),
            legend.key = element_rect(fill = "transparent", color = NA))
    
    
#Combine all plots together
    
    #add white space to plots without legends (or with narrower legends)
    pad_white <- ggdraw() + theme(plot.background = element_rect(fill = "white", color = NA),
                                  panel.background = element_rect(fill = "white", color = NA))
    
    O3.space <- plot_grid(ozone, pad_white, nrow = 1, rel_widths = c(1,0.145)) 
    CO.space <- plot_grid(CO, pad_white, nrow = 1, rel_widths = c(1,0.145)) 
    NO2.TVC.space <- plot_grid(NO2.TVC, pad_white, nrow = 1, rel_widths = c(1,0.145)) 
    NOx.space <- plot_grid(NOx, pad_white, nrow = 1, rel_widths = c(1,0.03))
    PM.space <- plot_grid(PM, pad_white, nrow = 1, rel_widths = c(1,0.019)) 
    
    Chicago.AQ <- cowplot::plot_grid(O3.space, CO.space, NOx.space, NO2.TVC.space, PM.space, AOD, ncol = 1, rel_heights = c(1.25,1,1,1,1,1.4))
    
    #Add panel labels
    Chicago.AQ <- ggdraw(Chicago.AQ) +
      draw_plot_label(
        label = c("(a)","(b)","(c)","(d)", "(e)","(f)", "(g)","(h)","(i)", "(j)", "(k)","(l)", "(m)", "(n)", "(o)"),
        x =     c(0.09, 0.29, 0.49, 0.69, 0.09, 0.49, 0.09, 0.29, 0.49, 0.69, 0.09, 0.09, 0.49, 0.09, 0.49),
        y =     c(0.945, 0.945, 0.945, 0.945, 0.79, 0.79, 0.645, 0.645, 0.645, 0.645, 0.49, 0.34,  0.34, 0.19, 0.19),
        hjust = 0, vjust = 1, size = 12
      )

    ggsave("Chicago AQ vs time-w Kenosha.png", Chicago.AQ, width = 9, height = 8)
    # ggsave("Chicago AQ vs time.png", Chicago.AQ, width = 9, height = 8)
    

    



    
#Make met plot with sites combined into one panel per parameter
#Also calculate 15-minute averages for all params at NEIU and Chiwaukee. (ATMOS is reported as 15-min averages, and Kenosha Harbor is reported as 10-min averages)
met.plot.15min.all <- met.plot.data %>% #Just for NEIU and Chiwaukee
  dplyr::filter(site %in% c("NEIU/North Park U", "Chiwaukee")) %>%
  dplyr::select(-panel) %>%
  pivot_wider(names_from = "param", values_from = "value") %>%
  dplyr::mutate(hour = format(datetime.CST, "%H"), minutes = as.numeric(format(datetime.CST, "%M")), 
                min.bin = ifelse(minutes < 15, "00", ifelse(minutes < 30, "15", ifelse(minutes < 45, "30", "45"))),
                u.wind = -abs(ws)*sin(wd*pi/180), v.wind = -abs(ws)*cos(wd*pi/180)) %>%
  dplyr::group_by(height,site,hour,min.bin) %>%
  dplyr::summarise(ws = mean(ws, na.rm = TRUE), wd = atan2(mean(u.wind, na.rm=TRUE),mean(v.wind, na.rm=TRUE))*180/pi + 180,
                   T = mean(T, na.rm = TRUE), RH = mean(RH, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(datetime.CST = as.POSIXct(paste0("2023-08-02 ",hour,":",min.bin), "%Y-%m-%d %H:%M", tz="America/Denver")) %>%
  dplyr::select(-hour,-min.bin) %>%
  pivot_longer(3:6, names_to = "param", values_to = "value")
    
# #Also calculate 15-minute average winds at NEIU and Chiwaukee. (ATMOS is reported as 15-min averages, and Kenosha Harbor is reported as 10-min averages)
# met.plot.15minwinds <- met.plot.data %>% #Just for NEIU and Chiwaukee
#   dplyr::filter(site %in% c("NEIU/North Park U", "Chiwaukee")) %>%
#   dplyr::select(-panel) %>%
#   pivot_wider(names_from = "param", values_from = "value") %>%
#   dplyr::mutate(hour = format(datetime.CST, "%H"), minutes = as.numeric(format(datetime.CST, "%M")), 
#                 min.bin = ifelse(minutes < 15, "00", ifelse(minutes < 30, "15", ifelse(minutes < 45, "30", "45"))),
#                 u.wind = -abs(ws)*sin(wd*pi/180), v.wind = -abs(ws)*cos(wd*pi/180)) %>%
#   dplyr::group_by(height,site,hour,min.bin) %>%
#   dplyr::summarise(ws = mean(ws, na.rm = TRUE), wd = atan2(mean(u.wind, na.rm=TRUE),mean(v.wind, na.rm=TRUE))*180/pi + 180) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(datetime.CST = as.POSIXct(paste0("2023-08-02 ",hour,":",min.bin), "%Y-%m-%d %H:%M", tz="America/Denver")) %>%
#   dplyr::select(-hour,-min.bin) %>%
#   pivot_longer(3:4, names_to = "param", values_to = "value")

met.plot.avgs <- met.plot.data %>%
  dplyr::filter(site %in% c("ATMOS", "Kenosha Harbor")) %>% #drop 1-min data for NEIU and Chiwaukee
  # dplyr::filter(param %in% c("T","RH") | (site %in% c("ATMOS", "Kenosha Harbor"))) %>% #drop wind data for NEIU and Chiwaukee
  dplyr::select(-plot,-panel) %>% #drop unneeded columns
  bind_rows(., met.plot.15min.all) %>% #add averaged NEIU and Chiwaukee data
  # bind_rows(., met.plot.15minwinds) %>%
  dplyr::mutate(site.height = ifelse(site == "Chiwaukee", "Chiwaukee", ifelse(site == "NEIU/North Park U", "NEIU", 
                                                                              ifelse(site == "Kenosha Harbor", "Kenosha Harbor", paste0("ATMOS-", height)))))

# Compute a global left x (POSIXct) and a tiny nudge inside the panel
x_left  <- min(met.plot.avgs$datetime.CST, na.rm = TRUE)
x_right <- max(met.plot.avgs$datetime.CST, na.rm = TRUE)
x_tag   <- x_left + (x_right - x_left) * 0.01  # 1% in from the left

met_param_levels <- c("T","RH","ws","wd")
met.plot.avgs$param <- factor(met.plot.avgs$param, levels = met_param_levels)

panel_labels <- expand.grid(param = met_param_levels, KEEP.OUT.ATTRS = FALSE) %>%
  dplyr::mutate(
    param = factor(param, levels = param_levels),
    tag      = paste0("(", letters[seq_len(n())], ")"),
    x        = x_tag,
    y        = Inf
  )

row_labels_met <- c(
  "T"      = "Temperature~'('*degree*C*')'",
  "RH"        = "Rel.~Humidity~'(%)'",
  "ws"        = "Wind~speed~'(m/s)'",
  "wd"        = "Wind~direction~'('*degree*')'"
)


    a <- ggplot() +
      geom_line(data=met.plot.avgs, aes(x=datetime.CST, y=value, group = site.height, color = site.height)) +
      # scale_color_brewer(palette = "Set1", guide = guide_legend(order = 1)) +
      scale_color_viridis_d(option = "D") +
      # scale_color_okabeito(palette = "full") +
      # panel tags
      geom_text(data = panel_labels, aes(x = x, y = y, label = tag), inherit.aes = FALSE, hjust = 0, vjust = 1.2, fontface = "bold" ) + # tuck into top-left
      facet_grid(rows = vars(param), switch = "y", labeller = labeller(param = as_labeller(row_labels_met, default = label_parsed)),  scales = "free_y") +
      scale_x_datetime(date_labels = "%H:%M", date_minor_breaks = "2 hours") +
      xlab("Time (CST)") + ylab(NULL) +
      theme(strip.placement = "outside",
            strip.background = element_blank(),
            strip.text = element_text(size=11.5),
            axis.title = element_text(size=12),
            axis.text = element_text(size=10),
            panel.background = element_rect(fill = "white", color = "black"),
            panel.grid = element_line(color = "gray85"),
            legend.title = element_blank(),
            legend.key = element_rect(fill = "transparent", color = NA))
    # ggsave("Chicago-ATMOS-Chiwaukee August 2 met only-combined.png", plot = a, width = 5, height = 6)
    # ggsave("Chicago-ATMOS-Chiwaukee-Kenosha August 2 met only-combined.png", plot = a, width = 5, height = 6)
    
#Dropping the RH panel
met.plot.avgs.no.RH <- met.plot.avgs %>%
  dplyr::filter(param != "RH")

met_param_levels <- c("T","ws","wd")
met.plot.avgs.no.RH$param <- factor(met.plot.avgs.no.RH$param, levels = met_param_levels)

panel_labels <- expand.grid(param = met_param_levels, KEEP.OUT.ATTRS = FALSE) %>%
  dplyr::mutate(
    param = factor(param, levels = param_levels),
    tag      = paste0("(", letters[seq_len(n())], ")"),
    x        = x_tag,
    y        = Inf
  )

row_labels_met <- c(
  "T"      = "Temperature~'('*degree*C*')'",
  # "RH"        = "Rel.~Humidity~'(%)'",
  "ws"        = "Wind~speed~'(m/s)'",
  "wd"        = "Wind~direction~'('*degree*')'"
)


    a <- ggplot() +
      geom_line(data=met.plot.avgs.no.RH, aes(x=datetime.CST, y=value, group = site.height, color = site.height)) +
      # scale_color_brewer(palette = "Set1", guide = guide_legend(order = 1)) +
      scale_color_viridis_d(option = "D") +
      # scale_color_okabeito(palette = "full") +
      # panel tags
      geom_text(data = panel_labels, aes(x = x, y = y, label = tag), inherit.aes = FALSE, hjust = 0, vjust = 1.2, fontface = "bold" ) + # tuck into top-left
      facet_grid(rows = vars(param), switch = "y", labeller = labeller(param = as_labeller(row_labels_met, default = label_parsed)),  scales = "free_y") +
      scale_x_datetime(date_labels = "%H:%M", date_minor_breaks = "2 hours") +
      xlab("Time (CST)") + ylab(NULL) +
      theme(strip.placement = "outside",
            strip.background = element_blank(),
            strip.text = element_text(size=11.5),
            axis.title = element_text(size=12),
            axis.text = element_text(size=10),
            panel.background = element_rect(fill = "white", color = "black"),
            panel.grid = element_line(color = "gray85"),
            legend.title = element_blank(),
            legend.key = element_rect(fill = "transparent", color = NA))
    # ggsave("Chicago-ATMOS-Chiwaukee-Kenosha August 2 met only-combined-no RH.png", plot = a, width = 5, height = 5)


    
    
#Calculate MDA8 ozone and 24-hour PM2.5 for sites
    
require(zoo)

#Ozone MDA8s
#Add in DOAS data at Kenosha Water Utility
DOAS.data <- read.csv("./other figures/staqs-Ground-Kenosha-DOAS_OTHER_20230701_R0_thru20230831-no header.csv", header = TRUE)

# Manually define the starting time/origin
origin <- as.POSIXct("2023-07-01 00:00:00", "%Y-%m-%d %H:%M:%S", tz="UTC")

DOAS.data.Aug2 <- DOAS.data %>%
  dplyr::mutate(datetime.UTC = origin + as.difftime(Time_Start, units = "secs"), datetime.CST = as.POSIXct(datetime.UTC, tz = "America/Denver")) %>%
  dplyr::filter(datetime.CST >= as.POSIXct("2023-08-02 00:00:00", tz = "America/Denver") &
                datetime.CST <  as.POSIXct("2023-08-03 00:00:00", tz = "America/Denver")) %>%
  dplyr::mutate(Ox = O3_ppb + NO2_ppb)
DOAS.Aug2.for.MDA8 <- DOAS.data.Aug2 %>%
  dplyr::select(datetime.CST, O3_ppb) %>%
  dplyr::mutate(param = "O3", site = "Kenosha WU") %>%
  dplyr::rename(value = O3_ppb)

#add on
O3.MDA8s <- bind_rows(all.ground.data, DOAS.Aug2.for.MDA8)

O3.MDA8s <- O3.MDA8s %>%
  dplyr::filter(param == "O3") %>%
  dplyr::mutate(hour = as.numeric(format(datetime.CST, "%H"))) %>%
  dplyr::group_by(site,hour) %>%
  dplyr::summarise(hour.mean = mean(value, na.rm = TRUE)) %>% #verified that all hours have >= 75% complete data
  dplyr::ungroup() %>%
  dplyr::group_by(site) %>%
  dplyr::mutate(avg.8hr = rollmean(x=hour.mean, 8, align = "left", fill = NA)) %>%
  dplyr::filter(avg.8hr == max(avg.8hr, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(site = ifelse(site == "NEIU/North Park U", "NEIU", site)) %>%
  dplyr::select(1,4)

write.csv(O3.MDA8s, "AGES Chicago sites - O3 MDA8s Aug2.csv", row.names = FALSE)

#PM2.5

PM25.24hr <- all.ground.data %>%
  dplyr::filter(param == "PM2.5") %>%
  dplyr::mutate(hour = as.numeric(format(datetime.CST, "%H"))) %>%
  dplyr::group_by(site,hour) %>%
  dplyr::summarise(hour.mean = mean(value, na.rm = TRUE)) %>% #verified that all hours have >= 75% complete data
  dplyr::ungroup() %>%
  dplyr::group_by(site) %>%
  dplyr::summarise(avg.24hr = mean(hour.mean, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(site = ifelse(site == "NEIU/North Park U", "NEIU", site)) 

write.csv(PM25.24hr, "AGES Chicago sites - 24hr PM25 Aug2.csv", row.names = FALSE)



#Pollution roses for NEIU (to identify the origin of the really high NO2)------------------------------
require(lubridate)
require(openair)
require(data.table)

setwd("...")

#Prepare to read in all netcdf files for all days

# Parse time units string -> (unit, origin POSIXct to the second, origin string)
parse_time_units_simple <- function(units_str, tz = "UTC") {
  m <- str_match(units_str, "^\\s*(\\w+)\\s+since\\s+(.+)\\s*$")
  if (is.na(m[1,1])) stop("Could not parse time units: ", units_str)
  
  unit <- tolower(m[1,2])
  origin_raw <- m[1,3]
  
  # keep only "YYYY-mm-dd HH:MM:SS" (drop fractional seconds if present)
  origin_sec_str <- str_match(origin_raw, "^(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})")[1,2]
  if (is.na(origin_sec_str)) stop("Could not parse origin datetime: ", origin_raw)
  
  origin <- ymd_hms(origin_sec_str, tz = tz)
  
  list(unit = unit, origin = origin, origin_str = origin_sec_str, units_str = units_str)
}

# Convert offsets to seconds based on unit name
offsets_to_seconds <- function(offsets, unit) {
  unit <- tolower(unit)
  
  mult <- switch(
    unit,
    "nanosecond" = 1e-9,
    "nanoseconds" = 1e-9,
    "microsecond" = 1e-6,
    "microseconds" = 1e-6,
    "millisecond" = 1e-3,
    "milliseconds" = 1e-3,
    "second" = 1,
    "seconds" = 1,
    "minute" = 60,
    "minutes" = 60,
    "hour" = 3600,
    "hours" = 3600,
    stop("Unsupported time unit: ", unit)
  )
  
  as.numeric(offsets) * mult
}


read_one_netcdf_simple <- function(path, vars = c("var1", "var2"), tz = "UTC") {
  nc <- nc_open(path)
  on.exit(nc_close(nc), add = TRUE)
  
  # Find time variable/dimension name (assume "time" if present)
  time_name <- if ("time" %in% names(nc$dim)) "time" else if ("time" %in% names(nc$var)) "time" else {
    stop("Couldn't find a 'time' dim/var in: ", path)
  }
  
  # Get units string from dim if available, else from var
  units_str <- nc$dim[[time_name]]$units
  if (is.null(units_str) && time_name %in% names(nc$var)) units_str <- nc$var[[time_name]]$units
  if (is.null(units_str)) stop("No time units found in: ", path)
  
  info <- parse_time_units_simple(units_str, tz = tz)
  
  # read time offsets
  time_offsets <- ncvar_get(nc, time_name)
  secs <- offsets_to_seconds(time_offsets, info$unit)
  
  # Build timestamps and round to nearest second
  time <- as.POSIXct(info$origin + secs, tz = tz)
  time <- as.POSIXct(round(as.numeric(time)), origin = "1970-01-01", tz = tz)
  
  out <- data.table(
    file = basename(path),
    file_origin = info$origin_str,   # origin truncated to second
    time = time
  )
  
  # Read selected variables
  for (v in vars) {
    if (!v %in% names(nc$var)) stop("Variable '", v, "' not found in ", path)
    out[[v]] <- as.vector(ncvar_get(nc, v))
  }
  
  out
}

# ---- Import the AQ files ----
AQ.folder <- "./NEIU_AQ_data_all/data"
AQ.files <- list.files(AQ.folder, pattern = "\\.nc", full.names = TRUE)

NEIU_all_AQ <- rbindlist(
  lapply(AQ.files, read_one_netcdf_simple, vars = c("no", "no2","o3","co"), tz = "UTC"),
  use.names = TRUE, fill = TRUE
)

NEIU_all_AQ <- as.data.frame(NEIU_all_AQ)

# ---- Import the met files ----
met.folder <- "./NEIU_met_data_all/data"
met.files <- list.files(met.folder, pattern = "\\.nc", full.names = TRUE)

NEIU_all_met <- rbindlist(
  lapply(met.files, read_one_netcdf_simple, vars = c("wind_dir_10s","wind_mean_10s"), tz = "UTC"),
  use.names = TRUE, fill = TRUE
)

NEIU_all_met <- as.data.frame(NEIU_all_met)

#Merge AQ and met data
#Calculate 1-minute averages for met (from 10-s values)
NEIU.all.met.min <- NEIU_all_met %>%
  dplyr::mutate(datetime.CST = as.POSIXct(time, tz="Etc/GMT+6"), Date = as.Date(datetime.CST, tz="Etc/GMT+6")) %>%
  dplyr::select(6,7,4,5) %>%
  dplyr::rename(wd = wind_dir_10s, ws = wind_mean_10s) %>%
  # pivot_longer(wd:ws, names_to = "param", values_to = "value") %>%
  dplyr::mutate(time.min = substr(as.character(datetime.CST), start = 12, stop = 16), time.min = ifelse(time.min == "", "00:00", time.min),
                u.wind = -abs(ws)*sin(wd*pi/180), v.wind = -abs(ws)*cos(wd*pi/180)) %>%
  dplyr::group_by(Date, time.min) %>%
  dplyr::summarise(ws = mean(ws, na.rm = TRUE), wd = atan2(mean(u.wind, na.rm=TRUE),mean(v.wind, na.rm=TRUE))*180/pi + 180) %>%
  # dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(datetime.CST = as.POSIXct(paste0(Date, time.min), "%Y-%m-%d %H:%M", tz="America/Denver")) %>%
  dplyr::select(5,3,4)

NEIU.all.AQ.min <- NEIU_all_AQ %>%
  dplyr::mutate(datetime.CST = as.POSIXct(time, tz="Etc/GMT+6"), Date = as.Date(datetime.CST, tz="Etc/GMT+6")) %>%
  dplyr::mutate(datetime.CST = round_date(datetime.CST, unit = "minute")) %>%
  dplyr::select(datetime.CST, Date, no:co) %>%
  dplyr::rename(NO = no, NO2 = no2, O3 = o3, CO = co)

NEIU.all.AQ.met <- inner_join(NEIU.all.met.min, NEIU.all.AQ.min, by="datetime.CST")
NEIU.all.AQ.met <- NEIU.all.AQ.met %>%
  dplyr::select(1,4,2:3,5:8) %>%
  pivot_longer(NO:CO, names_to = "param", values_to = "value") %>%
  dplyr::rename(date = datetime.CST) %>%
  dplyr::mutate(value = value * 1000)

#Make pollution roses
param.list <- unique(NEIU.all.AQ.met$param)

    # for(i in param.list)
    # {
    #   param.subset <- dplyr::filter(NEIU.all.AQ.met, param == i)
    #   
    #   png(file = paste0("NEIU pollution rose - ", i, ".png"), width = 800, height = 700)
    #   
    #   pollutionRose(param.subset, pollutant = "value", angle = 10, par.settings = list(fontsize = list(text=25)), 
    #                 main = paste0("NEIU ", i, " - May 19-Aug 30, 2023"))
    #   
    #   dev.off()
    # }

#Make time variation plots

    # for(i in param.list)
    # {
    #   param.subset <- dplyr::filter(NEIU.all.AQ.met, param == i)
    #   
    #   png(file = paste0("NEIU time variation plot - ", i, ".png"), width = 1000, height = 700)
    #   
    #   timeVariation(param.subset, pollutant = "value", par.settings = list(fontsize = list(text=15)), 
    #                 main = paste0("NEIU ", i, " - May 19-Aug 30, 2023"))
    #   
    #   dev.off()
    # }

    



# NEIU.AQ.min <- NEIU.AQ %>%
#   dplyr::mutate(datetime.CST = round_date(datetime.CST, unit = "minute")) %>%
#   dplyr::select(datetime.CST,param,value)
# NEIU.met.min <- NEIU.met.min %>%
#   dplyr::select(datetime.CST,param,value)
# NEIU.all <- bind_rows(NEIU.AQ.min,NEIU.met.min)
# NEIU.all <- NEIU.all %>%
#   pivot_wider(names_from = "param", values_from = "value") %>%
#   pivot_longer(PM2.5:CO, names_to = "param", values_to = "value") %>%
#   dplyr::rename(date = datetime.CST)
# 
#     pollutionRose(NEIU.all, pollutant = "value", type = "param", angle = 10)



#Calculate hourly average concentrations to plot with GCAS data

hourly.AQ <- all.ground.data %>%
  dplyr::mutate(hour.CST = format(datetime.CST, "%H")) %>%
  dplyr::filter(param %in% c("NO2","O3","PM2.5")) %>%
  dplyr::group_by(site,param,hour.CST) %>%
  dplyr::summarise(hour.mean = mean(value, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(site = ifelse(site == "NEIU/North Park U", "NEIU", site))

#add lat/lon
lat.lon <- read.csv("AGES Chicago ground sites.csv", header=TRUE)
lat.lon <- lat.lon %>%
  dplyr::mutate(Site = ifelse(Site == "Chiwaukee Prairie", "Chiwaukee", ifelse(Site == "Kenosha Water Utility", "Kenosha Harbor", ifelse(Site == "ANL", "ATMOS", Site)))) %>%
  dplyr::select(-Notes)

hourly.AQ <- left_join(hourly.AQ, lat.lon, by=c("site"="Site"))

write.csv(hourly.AQ, "Hourly O3-NO2-PM25 at AGES Chicago sites-Aug2.csv", row.names = FALSE)
  

