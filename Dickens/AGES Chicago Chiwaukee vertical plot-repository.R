## AGES Chicago paper - Chiwaukee vertical measurements plot

require(tidyverse)
require(ncdf4)
require(raster)
require(lubridate)
require(thunder)
require(colorRamps)
require(scales)
require(cowplot)
require(ggtext)
require(gridExtra)
require(grid)
require(zoo)
require(see)
require(RColorBrewer)
require(sf)
require(ggspatial)
require(plyr)



setwd("...")


## LiDAR data -------------------------------------------
#Used this site for info about how to open hdf files: https://www.hdfeos.org/software/r.php

LIDAR.data <- nc_open("groundbased_lidar.o3_uah001_hires_kenosha.wi_20230802t123843z_20230803t020854z_001.hdf")

LIDAR.O3 <- ncvar_get(LIDAR.data, "O3.MIXING.RATIO.VOLUME_DERIVED")
LIDAR.datetime <- ncvar_get(LIDAR.data, "DATETIME")
LIDAR.altitude <- ncvar_get(LIDAR.data, "ALTITUDE")
LIDAR.ground <- ncvar_get(LIDAR.data, "ALTITUDE.INSTRUMENT") # = 180 m

#datetime is MJD2K, for which 0 = January 1, 2000. (8401 days from 2000 through 2022; 212 days in Jan-July 2023 - subtract off 8612 days (not 8313 because 1/1/2000 = 0))
datetime.crosswalk <- data.frame(datetime.marker = paste0("V",seq(1,356,by=1)), datetime.MJD2K = LIDAR.datetime)#, Aug.datetime = LIDAR.datetime - 8612)

#convert MJD2K time to POSIXct:
MJD2K_to_POSIX <- function(MJD2K, tz = "UTC", origin = "2000-01-01 00:00:00") {
  as.POSIXct(origin, tz = tz) + MJD2K * 86400
}

datetime.crosswalk <- datetime.crosswalk %>%
  dplyr::mutate(datetime.UTC = MJD2K_to_POSIX(datetime.MJD2K)) %>%
  dplyr::select(-datetime.MJD2K)

# datetime.crosswalk <- datetime.crosswalk %>%
#   dplyr::mutate(Aug.date = substr(as.character(Aug.datetime), start=1, stop=1), time = as.numeric(substr(as.character(Aug.datetime), start=2, stop=10))*24,
#                 datetime.UTC = as.POSIXct(paste0("2023-08-",Aug.date, " ", time), "%Y-%m-%d %H", tz="UTC"))

LIDAR.O3 <- as.data.frame(LIDAR.O3)
LIDAR.O3.data <- LIDAR.O3 %>%
  dplyr::mutate(altitude.m.amsl = LIDAR.altitude) %>% 
  pivot_longer(V1:V356, names_to = "datetime.marker", values_to = "O3.ppmv") %>%
  dplyr::mutate(O3.ppmv = ifelse(O3.ppmv == -90000, NA, O3.ppmv), O3.ppb = O3.ppmv*1000) %>%
  left_join(., datetime.crosswalk, by="datetime.marker") %>%
  dplyr::mutate(datetime.CST = as.POSIXct(datetime.UTC, tz="America/Denver")) %>%
  dplyr::select(6,1,4)

LIDAR.O3.3000m <- LIDAR.O3.data %>%
  dplyr::filter(altitude.m.amsl <= 3000)

    # a <- ggplot(LIDAR.O3.3000m, aes(x=datetime.CST, y=altitude.m.amsl, fill=O3.ppb)) + geom_tile() +
    #   # scale_fill_distiller(palette = "Spectral") 
    #   scale_fill_viridis_c(option = "C")


aerosol.lidar <- nc_open("RO3QET_Aerosol_230802.nc") #-------------------------

aerosol.ext.coeff <- ncvar_get(aerosol.lidar, "merged_ext_coeff")
aerosol.altitude <- ncvar_get(aerosol.lidar, "altitude")
aerosol.datetime.start <- ncvar_get(aerosol.lidar, "datetime_start")
# aerosol.ext.coeff <- ncvar_get(aerosol.lidar, "ext_coeff")

altitude.crosswalk <- data.frame(altitude.link = paste0("V",seq(1,600,by=1)), altitude.m.amsl = aerosol.altitude)

LIDAR.aerosol <- as.data.frame(aerosol.ext.coeff)
LIDAR.aerosol.data <- LIDAR.aerosol %>%
  dplyr::mutate(datetime.MJD2K = aerosol.datetime.start) %>% 
  pivot_longer(V1:V600, names_to = "altitude.link", values_to = "ext.coeff") %>%
  dplyr::mutate(ext.coeff = ifelse(ext.coeff == -999, NA, ext.coeff), datetime.UTC = MJD2K_to_POSIX(datetime.MJD2K)) %>%
  left_join(., altitude.crosswalk, by="altitude.link") %>%
  dplyr::mutate(datetime.CST = as.POSIXct(datetime.UTC, tz="America/Denver")) %>%
  dplyr::select(6,5,3)

LIDAR.aerosol.3000m <- LIDAR.aerosol.data %>%
  dplyr::filter(altitude.m.amsl <= 3000 & ext.coeff > 4e-06) #%>% #4e-6 seems to be used as a null data code
  # dplyr::mutate(ext.coeff.perMm = ext.coeff * 1e6) #convert to Mm-1 (inverse megameters)

    # a <- ggplot(LIDAR.aerosol.3000m, aes(x=datetime.CST, y=altitude.m.amsl, fill=ext.coeff)) + geom_tile() +
    #   scale_fill_viridis_c(option = "D", limits = c(0,3E-4), oob = scales::squish)


## Ozone sondes ---------------------------------

O3.sonde.1 <- read.table("staqs-ChiwaukeePrairie_SONDE_20230802_R0_L1_ozonesonde-no header.txt", sep=",", header = TRUE)
O3.sonde.2 <- read.table("staqs-ChiwaukeePrairie_SONDE_20230802_R0_L2_ozonesonde-no header.txt", sep=",", header = TRUE)
O3.sonde.3 <- read.table("staqs-ChiwaukeePrairie_SONDE_20230802_R0_L3_ozonesonde-no header.txt", sep=",", header = TRUE)

O3.sonde.all <- bind_rows(O3.sonde.1, O3.sonde.2, O3.sonde.3)
    
O3.sonde.3000m <- O3.sonde.all %>%
  dplyr::mutate(datetime.UTC = as.POSIXct("2023-08-02 00:00:00", tz="UTC") + Seconds_UTC, datetime.CST = as.POSIXct(datetime.UTC, tz="America/Denver"), 
                altitude.m = Altitude_km*1000, Ozone_ppmv = ifelse(Ozone_ppmv == -99999, NA, Ozone_ppmv)) %>%
  dplyr::rename(O3.ppb = Ozone_ppmv) %>% #this must be mislabeled - pretty clearly is ppb not ppm
  dplyr::select(datetime.CST, altitude.m, O3.ppb) %>%
  dplyr::filter(altitude.m <= 3000 & altitude.m > 0)


#Chiwaukee ground ozone & PM2.5 ---------------------

CP.ground <- read.csv("Chiwaukee_Summer_2023_1MinuteData.csv", header=TRUE)
CP.ground <- CP.ground %>%
  dplyr::mutate(datetime.CST = as.POSIXct(DateTime, tz="America/Denver")) %>%
  dplyr::select(12,4,5) %>%
  dplyr::rename(O3.ppb = O3..ppb., PM2.5 = PM2.5..ug.m3.) %>%
  dplyr::mutate(altitude.m = 180, O3.ppb = ifelse(O3.ppb == -999, NA, O3.ppb), PM2.5 = ifelse(PM2.5 == -999, NA, PM2.5)) %>% #add ground altitude (180 m)
  dplyr::filter(datetime.CST >= as.POSIXct("2023-08-02 00:00:00", tz="America/Denver") & datetime.CST < as.POSIXct("2023-08-03 00:00:00", tz="America/Denver")) %>%
  dplyr::mutate(location = "over land")


#Function to pad the y-axis labels (to ensure consistent width between figures)
pad_fig <- function(x, width = 4) {
  # x numeric -> character with left padding using figure spaces
  s <- format(round(x), trim = TRUE, scientific = FALSE)
  n_pad <- pmax(0, width - nchar(s))
  paste0(strrep("\u2007", n_pad), s)
}

#Ozone combined plot (LiDAR, sondes, ground)------------------------
    a <- ggplot(LIDAR.O3.3000m, aes(x=datetime.CST, y=altitude.m.amsl, fill=O3.ppb)) + geom_tile() +
      # scale_fill_viridis_c(option = "C", limits = c(27,113), name = "Ozone", direction = -1) + 
      scale_fill_gradientn(colors = matlab.like(87), limits = c(27,113), name = "Ozone") + 
      scale_x_datetime(limits = c(as.POSIXct("2023-08-02 05:45:00", tz="America/Denver"), as.POSIXct("2023-08-02 20:15:00", tz="America/Denver")),
                       minor_breaks = date_breaks("1 hour"), expand = expansion(mult = 0)) +
      # scale_y_continuous(limits = c(0,3000), expand = expansion(mult = 0)) +
      scale_y_continuous(limits = c(0,2000), expand = expansion(mult = 0), labels = function(x) pad_fig(x, width = 4)) +
      xlab(NULL) + ylab(NULL) + #ylab("Altitude (m ASL)") +
      guides(fill = "none", color = "none") + #don't show legend
      theme(
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "gray85"),
        axis.text = element_text(size = 10)
        # axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
      )
    a <- a + geom_point(data = O3.sonde.3000m, aes(x=datetime.CST, y=altitude.m), shape = 17) +
      geom_point(data = O3.sonde.3000m, aes(x=datetime.CST, y=altitude.m, color=O3.ppb)) + 
      # scale_color_viridis_c(option = "C", limits = c(27,113), name = "Ozone", direction = -1) +
      scale_color_gradientn(colors = matlab.like(87), limits = c(27,113), name = "Ozone") + 
      geom_point(data = CP.ground, aes(x=datetime.CST, y=altitude.m, color=O3.ppb))
    # ggsave("Chiwaukee ozone lidar plot - Aug2.png", width = 2.7, height = 2.7)
    # ggsave("Chiwaukee ozone lidar plot - Aug2-wide.png", width = 6, height = 2.5)
    # ggsave("Chiwaukee ozone lidar plot - Aug2-wide-2000m.png", width = 6.1, height = 1.5)
    # ggsave("Chiwaukee ozone lidar plot - Aug2-wide-2000m-spectral.png", width = 6.1, height = 1.5)
    

    
#Aerosol/PM2.5 combined plot (LiDAR, ground) ----------------------------
    b <- ggplot() + 
      geom_tile(data = LIDAR.aerosol.3000m, aes(x=datetime.CST, y=altitude.m.amsl, fill=ext.coeff)) +
      # scale_fill_viridis_c(option = "D", limits = c(0,3.01E-4), oob = scales::squish, name = "Aerosol", direction = -1) + 
      # scale_fill_distiller(palette = "YlGnBu", limits = c(0,8e-4), direction = 1) +
      # scale_fill_gradientn(colors = matlab.like(87), limits = c(0,8e-4)) + 
      scale_fill_gradientn(colors = matlab.like(54), limits = c(0,8e-4)) + 
      geom_point(data = CP.ground, aes(x=datetime.CST, y=altitude.m, color=PM2.5), inherit.aes = FALSE) +
      # scale_color_viridis_c(option = "D", limits = c(7,60), direction = -1) +
      scale_color_distiller(palette = "Spectral", limits = c(6,24), breaks = seq(8,24,by=4)) +
      scale_x_datetime(limits = c(as.POSIXct("2023-08-02 05:45:00", tz="America/Denver"), as.POSIXct("2023-08-02 20:15:00", tz="America/Denver")), 
                       minor_breaks = date_breaks("1 hour"), expand = expansion(mult = 0)) +
      # scale_y_continuous(limits = c(0,3000), expand = expansion(mult = 0)) +
      scale_y_continuous(limits = c(0,2000), expand = expansion(mult = 0), labels = function(x) pad_fig(x, width = 4)) +
      labs(fill = "Extinction coeff. (m<sup>-1</sup>)", color = "PM<sub>2.5</sub> (\u03bcg/m<sup>3</sup>)") +
      xlab(NULL) + ylab(NULL) + #ylab("Altitude (m ASL)") +
      # guides(color = "none") + #don't show legend for color
      # guides(
      #   fill  = guide_colorbar(barheight = unit(40, "pt"), title.position = "right",
      #     title.theme = element_markdown(size = 9, angle = 90, hjust = 0.5), label.theme = element_text(size = 8)),
      #   color = guide_colorbar(barheight = unit(40, "pt"), title.position = "right",
      #     title.theme = element_markdown(size = 9, angle = 90, hjust = 0.5), label.theme    = element_text(size = 8))) +
      theme(
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "gray85"),
        axis.text = element_text(size = 10),
        # axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.title = element_markdown(size = 9, angle = 90, hjust = 0.5),
        # legend.title = element_text(size = 9, angle = 90, hjust = 0.5),
        legend.text = element_text(size = 8),
        legend.direction = "vertical",
        legend.box = "horizontal",
        legend.title.position = "right"
      )
    # ggsave("Chiwaukee aerosol lidar plot - Aug2.png", width = 2.7, height = 2.7)
    # ggsave("Chiwaukee aerosol lidar plot - Aug2-wide.png", width = 8, height = 2.5)
    # ggsave("Chiwaukee aerosol lidar plot - Aug2-wide-2000.png", width = 8.2, height = 1.5)
    # ggsave("Chiwaukee aerosol lidar plot - Aug2-wide-2000-spectral.png", width = 8.2, height = 1.5)

    
## UAV data --------------------------------------------------------------

Aug2.UAH.UAS <- read.table("staqs-O3-PM25-MET_DRONE_20230802_R0_EPAcorr-no header.txt", sep = ",", header = TRUE) #PM2.5 data corrected for RH
# Aug2.UAH.UAS <- read.table("staqs-O3-PM25-MET_DRONE_20230802_R0-no header.txt", sep = ",", header = TRUE)
Aug3.UAH.UAS <- read.table("staqs-O3-PM25-MET_DRONE_20230803_R0_EPAcorr-no header.txt", sep = ",", header = TRUE)
# Aug3.UAH.UAS <- read.table("staqs-O3-PM25-MET_DRONE_20230803_R0-no header.txt", sep = ",", header = TRUE)

Aug2.UAH.UAS <- Aug2.UAH.UAS %>%
  dplyr::mutate(datetime.UTC = as.POSIXct("2023-08-02 00:00:00", tz="UTC") + Time_Start, datetime.CST = as.POSIXct(datetime.UTC, tz="America/Denver"))
Aug3.UAH.UAS <- Aug3.UAH.UAS %>%
  dplyr::mutate(datetime.UTC = as.POSIXct("2023-08-03 00:00:00", tz="UTC") + Time_Start, datetime.CST = as.POSIXct(datetime.UTC, tz="America/Denver"))

UAH.UAS <- bind_rows(Aug2.UAH.UAS, Aug3.UAH.UAS)
UAH.UAS <- UAH.UAS %>%
  dplyr::filter(datetime.CST >= as.POSIXct("2023-08-02 00:00:00", tz="America/Denver") & datetime.CST < as.POSIXct("2023-08-03 00:00:00", tz="America/Denver"))%>%
  dplyr::select(datetime.CST, Altitude, O3, PM25_corr) %>%
  dplyr::rename(PM25 = PM25_corr) %>%
  dplyr::mutate(location = "over land")

#Adjust altitude by setting minimum altitude = 180 m and adjusting the rest of each flight accordingly.
UAH.UAS.ht.adj <- UAH.UAS %>%
  dplyr::mutate(hour = as.numeric(format(datetime.CST, "%H")), min = as.numeric(format(datetime.CST, "%M")), 
                UAS.flight = ifelse(min < 30, hour, hour+1)) %>% #create label for each drone flight based on hour
  dplyr::group_by(UAS.flight) %>%
  # dplyr::mutate(Alt.start = Altitude[datetime.CST == min(datetime.CST)]) %>% #calculate starting altitude
  dplyr::mutate(Alt.min = min(Altitude)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Alt.adj = Altitude - (Alt.min - 180)) %>% #adjust by subtracting off the delta between starting altitude and "true" starting altitude (180 m)
  dplyr::select(1,10,3,4,5) %>%
  dplyr::rename(Altitude = Alt.adj)


Aug2.UWEC.UAS <- read.table("staqs-UWEC-UAS_DRONE_20230802_R0-no header.txt", sep = ",", header = TRUE)
# Aug3.UWEC.UAS <- read.table("staqs-O3-PM25-MET_DRONE_20230803_R0-no header.txt", sep = ",", header = TRUE) #All UWEC's 8/2 flights are in the 20230802 file

    # Check which flights are over land and which over water. Over-water is longitude > -87.8075. Only plot the over-water flights.
    # ggplot(Aug2.UWEC.UAS, aes(x=Time_Start, y=Longitude)) + geom_point()

UWEC.UAS <- Aug2.UWEC.UAS %>%
  dplyr::mutate(datetime.UTC = as.POSIXct("2023-08-02 00:00:00", tz="UTC") + Time_Start, datetime.CST = as.POSIXct(datetime.UTC, tz="America/Denver"),
                Altitude = Altitude_mAGL + 177) %>% #convert to m amsl by adding the altitude of the lake on August 2, 2023 ~ 177 m
  dplyr::filter(Longitude > -87.8080) %>% #select just over-water flights
  dplyr::select(datetime.CST, Altitude, O3_ppb) %>%
  dplyr::mutate(location = "over lake") %>%
  dplyr::rename(O3 = O3_ppb)

both.UAS <- bind_rows(UAH.UAS.ht.adj, UWEC.UAS)
both.UAS$location <- factor(both.UAS$location, levels = c("over land","over lake"))

#Calculate 30s averages (per email from Patti Cleary 1/15/26)
both.UAS.30s <- both.UAS %>%
  dplyr::mutate(time = format(datetime.CST, "%H:%M"), sec = as.numeric(format(datetime.CST, "%S")), sec.mid = ifelse(sec < 30, 15, 45)) %>% #split into 30-s bins
  dplyr::group_by(location,time,sec.mid) %>%
  dplyr::summarise(Altitude = mean(Altitude), O3 = mean(O3), PM25 = mean(PM25)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(datetime.CST = as.POSIXct(paste0("2023-08-02 ", time, ":", sec.mid), "%Y-%m-%d %H:%M:%S", tz="America/Denver")) %>%
  dplyr::select(-time, -sec.mid)

# CP.ground <- CP.ground %>%
#   dplyr::mutate(location = "over land")
CP.ground$location <- factor(CP.ground$location, levels = c("over land","over lake"))

#Ozone UAS plot
    c <- ggplot() + geom_point(data = both.UAS.30s, aes(x=datetime.CST, y=Altitude, color = O3, group = O3)) + facet_grid(location~.) +
    # c <- ggplot() + geom_point(data = both.UAS, aes(x=datetime.CST, y=Altitude, color = O3, group = O3)) + facet_grid(location~.) +
      geom_point(data = CP.ground, aes(x=datetime.CST, y=altitude.m, color=O3.ppb)) +
      scale_x_datetime(limits = c(as.POSIXct("2023-08-02 05:45:00", tz="America/Denver"), as.POSIXct("2023-08-02 20:15:00", tz="America/Denver")), 
                       minor_breaks = date_breaks("1 hour"), expand = expansion(mult = 0)) +
      scale_y_continuous(labels = function(x) pad_fig(x, width = 5)) +
      # scale_color_viridis_c(option = "C", limits = c(27,113), name = "Ozone (ppb)", direction = -1) +
      scale_color_gradientn(colors = matlab.like(87), limits = c(27,113), name = "Ozone (ppb)") +
      xlab(NULL) + ylab(NULL) + #ylab("Altitude (m ASL)") +
      theme(
        axis.text = element_text(size = 10),
        # axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "gray85"),
        strip.background = element_blank(), 
        strip.text = element_blank(),
        # strip.background = element_rect(fill = "white"),
        # strip.text = element_text(size = 8),
        legend.title = element_text(size = 9, angle = 90, hjust = 0.5),
        legend.text = element_text(size = 8),
        legend.direction = "vertical",
        legend.title.position = "right",
        panel.spacing = unit(0.9, "lines") #increase spacing between facets
      )
    # ggsave("Chiwaukee drone ozone plot - Aug2.png", width = 4.3, height = 2.7)
    # ggsave("Chiwaukee drone ozone plot - Aug2-wide.png", width = 7, height = 1.5)
    # ggsave("Chiwaukee drone ozone plot - Aug2-wide-spectral.png", width = 7, height = 1.5)
    # ggsave("Chiwaukee drone ozone plot - Aug2-wide-spectral-30s avg.png", width = 7, height = 1.5)

#PM2.5 UAS plot
    # d <- ggplot() + geom_point(data=UAH.UAS, aes(x=datetime.CST, y=Altitude, color = PM25, group = PM25)) + #facet_wrap(~location) +
    d <- ggplot() + geom_point(data=subset(both.UAS.30s, location == "over land"), aes(x=datetime.CST, y=Altitude, color = PM25, group = PM25)) + facet_grid(location~.) +
    # d <- ggplot() + geom_point(data=subset(both.UAS, location == "over land"), aes(x=datetime.CST, y=Altitude, color = PM25, group = PM25)) + facet_grid(location~.) +
      geom_point(data = CP.ground, aes(x=datetime.CST, y=altitude.m, color=PM2.5)) +
      scale_x_datetime(limits = c(as.POSIXct("2023-08-02 05:45:00", tz="America/Denver"), as.POSIXct("2023-08-02 20:15:00", tz="America/Denver")), 
                       minor_breaks = date_breaks("1 hour"), expand = expansion(mult = 0)) +
      scale_y_continuous(labels = function(x) pad_fig(x, width = 5)) +
      # scale_color_viridis_c(option = "D", limits = c(6,24), breaks = seq(8,24,by=4), name = "PM2.5 (ug/m3)", direction = -1)  +
      scale_color_distiller(palette = "Spectral", limits = c(6,24), breaks = seq(8,24,by=4)) +
      labs(color = "PM<sub>2.5</sub> (\u03bcg/m<sup>3</sup>)") +
      xlab(NULL) + ylab(NULL) + #ylab("Altitude (m ASL)") +
      # guides(color = "none") + #don't show legend
      theme(
        axis.text = element_text(size = 10),
        # axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "gray85"),
        strip.background = element_blank(), 
        strip.text = element_blank(),
        # strip.background = element_rect(fill = "white"),
        # strip.text = element_text(size = 8),
        legend.title = element_markdown(size = 9, angle = 90, hjust = 0.5),
        legend.text = element_text(size = 8),
        # legend.title = element_text(size = 9, angle = 90, hjust = 0.5),
        legend.direction = "vertical",
        legend.title.position = "right"
      )
    # ggsave("Chiwaukee drone PM25 plot - Aug2.png", width = 2.8, height = 2.7)
    # ggsave("Chiwaukee drone PM25 plot - Aug2.png", width = 6, height = 0.9)
    # ggsave("Chiwaukee drone PM25 plot - Aug2-spectral-corrected.png", width = 6, height = 4)
 
       
# #Calculate mean O3 at 185-210 m during the 18:00 flight over land and over lake & altitude < 210 m
# low.18h.O3 <- both.UAS.30s %>%
#   dplyr::filter(datetime.CST > as.POSIXct("2023-08-02 18:00:00", tz="America/Denver") & datetime.CST < as.POSIXct("2023-08-02 18:10:00", tz="America/Denver") &
#                   Altitude < 210) %>%
#   dplyr::group_by(location) %>%
#   dplyr::summarise(O3.mean = mean(O3)) %>%
#   dplyr::ungroup()
# low.19h.O3 <- both.UAS.30s %>%
#   dplyr::filter(datetime.CST > as.POSIXct("2023-08-02 19:00:00", tz="America/Denver") & datetime.CST < as.POSIXct("2023-08-02 19:08:30", tz="America/Denver") &
#                   Altitude < 210) %>%
#   dplyr::group_by(location) %>%
#   dplyr::summarise(O3.mean = mean(O3)) %>%
#   dplyr::ungroup()
# low.17h.O3 <- both.UAS.30s %>%
#   dplyr::filter(datetime.CST > as.POSIXct("2023-08-02 17:00:00", tz="America/Denver") & datetime.CST < as.POSIXct("2023-08-02 17:08:30", tz="America/Denver") &
#                   Altitude < 210) %>%
#   dplyr::group_by(location) %>%
#   dplyr::summarise(O3.mean = mean(O3)) %>%
#   dplyr::ungroup()



#NEW Combined ozone plot: lidar, ground, sondes and UAV -------------------------------------------
# UAH.UAS <- dplyr::rename(UAH.UAS, O3.ppb = O3)
UAH.UAS.30s <- both.UAS.30s %>%
  dplyr::filter(location == "over land") %>%
  dplyr::rename(O3.ppb = O3)
    
    a <- ggplot(LIDAR.O3.3000m, aes(x=datetime.CST, y=altitude.m.amsl, fill=O3.ppb)) + geom_tile() +
      # scale_fill_viridis_c(option = "C", limits = c(27,113), name = "Ozone", direction = -1) +
      scale_fill_gradientn(colors = matlab.like(87), limits = c(27,113), name = "Ozone (ppb)") +
      scale_x_datetime(limits = c(as.POSIXct("2023-08-02 05:45:00", tz="America/Denver"), as.POSIXct("2023-08-02 20:15:00", tz="America/Denver")),
                       minor_breaks = date_breaks("1 hour"), expand = expansion(mult = 0)) +
      scale_y_continuous(limits = c(0,2000), expand = expansion(mult = 0), labels = function(x) pad_fig(x, width = 4)) +
      xlab(NULL) + ylab(NULL) + #ylab("Altitude (m ASL)") +
      # guides(fill = "none", color = "none") + #don't show legend
      theme(
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "gray85"),
        axis.text = element_text(size = 10),
        # axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.title = element_text(size = 9, angle = 90, hjust = 0.5),
        # legend.title = element_text(size = 9, angle = 90, hjust = 0.5),
        legend.text = element_text(size = 8),
        legend.direction = "vertical",
        legend.box = "horizontal",
        legend.title.position = "right"
      )
    a <- a + geom_point(data = O3.sonde.3000m, aes(x=datetime.CST, y=altitude.m), shape = 17) +
      geom_point(data = O3.sonde.3000m, aes(x=datetime.CST, y=altitude.m, color=O3.ppb)) + 
      # scale_color_viridis_c(option = "C", limits = c(27,113), name = "Ozone", direction = -1)
      scale_color_gradientn(colors = matlab.like(87), limits = c(27,113), name = "Ozone (ppb)")
    a <- a + geom_point(data = UAH.UAS.30s, aes(x=datetime.CST, y=Altitude), shape = 17, size=1.7) +
      geom_point(data = UAH.UAS.30s, aes(x=datetime.CST, y=Altitude, color=O3.ppb)) + 
      geom_point(data = CP.ground, aes(x=datetime.CST, y=altitude.m, color=O3.ppb))
    # ggsave("Chiwaukee ozone lidar plot - Aug2-wide-2000m-spectral-UAV.png", width = 6.1, height = 4)
    
    
#NEW Combined aerosol/PM2.5 plot: lidar, ground and UAV
    b <- ggplot() + 
      geom_tile(data = LIDAR.aerosol.3000m, aes(x=datetime.CST, y=altitude.m.amsl, fill=ext.coeff)) +
      # scale_fill_viridis_c(option = "D", limits = c(0,3.01E-4), oob = scales::squish, name = "Aerosol", direction = -1) + 
      # scale_fill_viridis_c(option = "D", limits = c(0,8e-4), name = "Aerosol", direction = -1) + 
      # scale_fill_distiller(palette = "YlGnBu", limits = c(0,8e-4), direction = 1) +
      scale_fill_gradientn(colors = matlab.like(54), limits = c(0,8e-4)) +
      geom_point(data = CP.ground, aes(x=datetime.CST, y=altitude.m, color=PM2.5), inherit.aes = FALSE) +
      # scale_color_viridis_c(option = "D", limits = c(6,24), breaks = seq(8,24,by=4), direction = -1) +
      scale_color_distiller(palette = "Spectral", limits = c(6,24), breaks = seq(8,24,by=4)) +
      scale_x_datetime(limits = c(as.POSIXct("2023-08-02 05:45:00", tz="America/Denver"), as.POSIXct("2023-08-02 20:15:00", tz="America/Denver")), 
                       minor_breaks = date_breaks("1 hour"), expand = expansion(mult = 0)) +
      # scale_y_continuous(limits = c(0,3000), expand = expansion(mult = 0)) +
      scale_y_continuous(limits = c(0,2000), expand = expansion(mult = 0), labels = function(x) pad_fig(x, width = 4)) +
      labs(fill = "Extinction coeff. (m<sup>-1</sup>)", color = "PM<sub>2.5</sub> (\u03bcg/m<sup>3</sup>)") +
      xlab(NULL) + ylab(NULL) + #ylab("Altitude (m ASL)") +
      # guides(color = "none") + #don't show legend for color
      # guides(
      #   fill  = guide_colorbar(barheight = unit(40, "pt"), title.position = "right",
      #     title.theme = element_markdown(size = 9, angle = 90, hjust = 0.5), label.theme = element_text(size = 8)),
      #   color = guide_colorbar(barheight = unit(40, "pt"), title.position = "right",
      #     title.theme = element_markdown(size = 9, angle = 90, hjust = 0.5), label.theme    = element_text(size = 8))) +
      theme(
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "gray85"),
        axis.text = element_text(size = 10),
        # axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.title = element_markdown(size = 9, angle = 90, hjust = 0.5),
        # legend.title = element_text(size = 9, angle = 90, hjust = 0.5),
        legend.text = element_text(size = 8),
        legend.direction = "vertical",
        legend.box = "horizontal",
        legend.title.position = "right"
      )
    b <- b + geom_point(data = UAH.UAS.30s, aes(x=datetime.CST, y=Altitude), shape = 17, size=1.7) +
      geom_point(data = UAH.UAS.30s, aes(x=datetime.CST, y=Altitude, color=PM25)) + 
      geom_point(data = CP.ground, aes(x=datetime.CST, y=altitude.m, color=PM2.5))
    # ggsave("Chiwaukee aerosol lidar plot - Aug2-wide-2000-spectral-UAV.png", width = 8.2, height = 4)
    
    
    


#Meteorology -------------------------------------------------------------
met.sonde1 <- read.table("staqs-ChiwaukeePrairie_SONDE_20230802_R0_L1_windsonde-no header.txt", sep=",", header = TRUE)
met.sonde2 <- read.table("staqs-ChiwaukeePrairie_SONDE_20230802_R0_L2_windsonde-no header.txt", sep=",", header = TRUE)
met.sonde3 <- read.table("staqs-ChiwaukeePrairie_SONDE_20230802_R0_L3_windsonde-no header.txt", sep=",", header = TRUE)

met.sondes <- bind_rows(met.sonde1, met.sonde2, met.sonde3)

met.sondes.3000m <- met.sondes %>%
  dplyr::mutate(datetime.UTC = as.POSIXct("2023-08-02 00:00:00", tz="UTC") + Seconds_UTC, datetime.CST = with_tz(datetime.UTC, tzone = "Etc/GMT+6"),
                datetime.CST = if_else(Seconds_UTC < 3250, datetime.CST + days(1), datetime.CST)) %>% #correct date to 8/2 - seconds were entered incorrectly
  dplyr::select(datetime.CST, Altitude_m_MSL, Temperature_C, Relative_humidity) %>%
  dplyr::filter(Altitude_m_MSL <= 3000) %>%
  dplyr::mutate(time = ifelse(datetime.CST < as.POSIXct("2023-08-02 09:00:00", tz="Etc/GMT+6"), "7:00", 
                              ifelse(datetime.CST < as.POSIXct("2023-08-02 15:00:00", tz="Etc/GMT+6"), "14:00", "18:00"))) %>%
  pivot_longer(Temperature_C:Relative_humidity, names_to = "param", values_to = "values") %>%
  dplyr::filter((time == "7:00" & datetime.CST >= as.POSIXct("2023-08-02 07:02:49", tz = "Etc/GMT+6")) |
                  time == "14:00" & datetime.CST >= as.POSIXct("2023-08-02 14:16:08", tz = "Etc/GMT+6")|
                  time == "18:00" & datetime.CST >= as.POSIXct("2023-08-02 17:59:27", tz = "Etc/GMT+6"))
met.sondes.3000m$time <- factor(met.sondes.3000m$time, levels = c("7:00","14:00","18:00"))
met.sondes.3000m$param <- factor(met.sondes.3000m$param, levels = c("Temperature_C","Relative_humidity"))

# col_labels <- c(
#   "Temperature_C"     = "Temp.~'('*degree*C*')'",
#   "Relative_humidity" = "RH~'(%)'"
# )

    # e <- ggplot(met.sondes.3000m, aes(x=values, y=Altitude_m_MSL, color=time, group=time)) + geom_path() + #facet_wrap(~param, scales="free_x") +
    #   xlab(NULL) + ylab("Altitude (m ASL)") +
    #   facet_grid(cols = vars(param), switch = "x", labeller = labeller(param = facet_labs),  scales = "free_x") +
    #   # facet_grid(cols = vars(param), switch = "x", labeller = labeller(param = as_labeller(col_labels, default = label_parsed)),  scales = "free_x") +
    #   scale_color_brewer(palette = "Set1") + scale_y_continuous(limits = c(0,2000)) +
    #   guides(color = guide_legend(position = "inside")) +
    #   theme(strip.placement = "outside",
    #         strip.background = element_blank(),
    #         strip.text = element_text(size=12),
    #         axis.title = element_text(size=12),
    #         axis.text = element_text(size=11),
    #         panel.background = element_rect(fill = "white", color = "black"),
    #         panel.grid = element_line(color = "gray85"),
    #         legend.title = element_blank(),
    #         legend.key = element_rect(fill = "transparent", color = NA),
    #         # Place legend inside the first facet (top-left-ish)
    #         legend.position.inside = c(0.02, 0.15),
    #         legend.justification = c("left", "top"),
    #         legend.direction = "vertical",
    #         legend.background = element_rect(fill = scales::alpha("white", 0.8),
    #                                          color = "grey70"),
    #         legend.key.height = unit(0.35, "lines"),
    #         legend.key.width  = unit(0.7, "lines"),
    #         legend.text = element_text(size = 9))
    # # ggsave("Chiwaukee sondes - temp and RH.png", width = 4, height = 4)
    # ggsave("Chiwaukee sondes - temp and RH-2000.png", width = 3, height = 4)

#Separate plots for RH and Temp (for publication)


    temp <- ggplot(data = subset(met.sondes.3000m, param == "Temperature_C"), aes(x=values, y=Altitude_m_MSL, color=time, group=time)) + 
      geom_rect(aes(xmin = 0, xmax = 35, ymin = 0, ymax = 180), fill = "gray50", color = NA) +
      geom_path() + ylab("Altitude (m ASL)") + labs(x = expression("Temp. ("*degree*C*")"[phantom(2)])) + #add phantom subscript to align plots exactly
      # facet_grid(cols = vars(param), switch = "x", labeller = labeller(param = facet_labs),  scales = "free_x") +
      # scale_color_brewer(palette = "Set1") +
      scale_color_okabeito(palette = "full") + #colorblind friendly
      scale_y_continuous(limits = c(0,2000)) + coord_cartesian(xlim = c(8, 28)) +
      guides(color = guide_legend(position = "inside"), fill="none") +
      theme(axis.title = element_text(size=12),
            axis.text = element_text(size=11),
            panel.background = element_rect(fill = "white", color = "black"),
            panel.grid = element_line(color = "gray85"),
            legend.title = element_blank(),
            legend.key = element_rect(fill = "transparent", color = NA),
            # Place legend inside the first facet 
            legend.position.inside = c(0.03, 0.29),
            legend.justification = c("left", "top"),
            legend.direction = "vertical",
            legend.background = element_rect(fill = scales::alpha("white", 0.8),
                                             color = "grey70"),
            legend.key.height = unit(0.35, "lines"),
            legend.key.width  = unit(0.7, "lines"),
            legend.text = element_text(size = 9))
    # ggsave("Chiwaukee sondes - temp.png", width = 1.75, height = 4)
    
    RH <- ggplot(data = subset(met.sondes.3000m, param == "Relative_humidity"), aes(x=values, y=Altitude_m_MSL, color=time, group=time)) + 
      geom_rect(aes(xmin = 0, xmax = 90, ymin = 0, ymax = 180), fill = "gray50", color = NA) +
      geom_path() + ylab("Altitude (m ASL)") + labs(x = expression("RH (%)"[phantom(2)])) +
      # facet_grid(cols = vars(param), switch = "x", labeller = labeller(param = facet_labs),  scales = "free_x") +
      # scale_color_brewer(palette = "Set1") + 
      scale_color_okabeito(palette = "full") + #colorblind friendly
      scale_y_continuous(limits = c(0,2000)) + coord_cartesian(xlim = c(30, 85)) +
      guides(color = "none", fill = "none") +
      theme(axis.title.x = element_text(size=12),
            axis.text.x = element_text(size=11),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            panel.background = element_rect(fill = "white", color = "black"),
            panel.grid = element_line(color = "gray85"))
    # ggsave("Chiwaukee sondes - RH.png", width = 1.25, height = 4)
    


# Plot AEROMMA and SeaRey NO2 data -------------------------------------
AEROMMA.NO2 <- read.table("AEROMMA-ACES-NO2_DC8_20230802_R0-no header.txt", sep=",", header=TRUE)
AEROMMA.nav <- read.table("AEROMMA-MetNav_DC8_20230802_R0-no header.txt", sep=",", header = TRUE) #includes lat/lon, altitude, etc.

AEROMMA.nav <- AEROMMA.nav %>%
  dplyr::select(Time_Start, Latitude, Longitude, MSL_GPS_Altitude)

AEROMMA.NO2 <- AEROMMA.NO2 %>%
  left_join(., AEROMMA.nav, by="Time_Start") %>%
  dplyr::mutate(datetime.UTC = as.POSIXct("2023-08-02 00:00:00", tz="UTC") + Time_Start, datetime.CST = with_tz(datetime.UTC, tzone = "Etc/GMT+6"),
                NO2_ACES = ifelse(NO2_ACES == -9999, NA, NO2_ACES))

#SeaRey data in archive is missing the ends of flights 1 and 3, so use raw data files from UAH

SeaRey1 <- read.table("./SeaRey-raw data/2023_08_02_15Z_searey.txt", sep = ",", header = TRUE)
SeaRey2 <- read.table("./SeaRey-raw data/2023_08_02_17Z_searey.txt", sep = ",", header = TRUE)
SeaRey3 <- read.table("./SeaRey-raw data/2023_08_02_20Z_searey.txt", sep = ",", header = TRUE)

# SeaRey1 <- read.table("STAQS-O3-NO2-PM25-MET_SEAREY_20230802_R0_L1-no header.txt", sep=",", header = TRUE)
# SeaRey2 <- read.table("STAQS-O3-NO2-PM25-MET_SEAREY_20230802_R0_L2-no header.txt", sep=",", header = TRUE)
# SeaRey3 <- read.table("STAQS-O3-NO2-PM25-MET_SEAREY_20230802_R0_L3-no header.txt", sep=",", header = TRUE)

SeaRey.all <- bind_rows(SeaRey1, SeaRey2, SeaRey3)
SeaRey.all <- SeaRey.all %>%
  # dplyr::mutate(datetime.UTC = as.POSIXct("2023-08-02 00:00:00", tz="UTC") + Time_Mid, datetime.CST = with_tz(datetime.UTC, tzone = "Etc/GMT+6")) %>%
  dplyr::mutate(datetime.UTC = as.POSIXct(time, tz="UTC"), datetime.CST = with_tz(datetime.UTC, tzone = "Etc/GMT+6"),
                altitude = altitude + 35) #%>% #add 35 m on to altitude - adjustment UAH made to airplane altitude
  # dplyr::filter(Latitude != -9999)

# SeaRey.all.archive <- bind_rows(SeaRey1, SeaRey2, SeaRey3)
# SeaRey.all.archive <- SeaRey.all.archive %>%
#   dplyr::mutate(datetime.UTC = as.POSIXct("2023-08-02 00:00:00", tz="UTC") + Time_Mid, datetime.CST = with_tz(datetime.UTC, tzone = "Etc/GMT+6")) %>%
#   # dplyr::mutate(datetime.UTC = as.POSIXct(time, tz="UTC"), datetime.CST = with_tz(datetime.UTC, tzone = "Etc/GMT+6")) #%>%
# dplyr::filter(Latitude != -9999)
# 
# AEROMMA.NO2.by.CP <- AEROMMA.NO2 %>%
#   dplyr::filter(Longitude > -87.85 & Longitude < -87.6 & Latitude > 42.4 & Latitude < 42.58 & !is.na(NO2_ACES) & MSL_GPS_Altitude < 3000) %>%
#   dplyr::mutate(NO2_ACES = ifelse(NO2_ACES < 0, 0, NO2_ACES), #set all negative concentrations to zero
#                 time = ifelse(datetime.CST < as.POSIXct("2023-08-02 14:00:00", tzone = "Etc/GMT+6"), "midday", "pm"), platform = "DC-8") %>%
#   # dplyr::select(platform, datetime.CST, time, MSL_GPS_Altitude, NO2_ACES) %>%
#   dplyr::rename(Altitude.m = MSL_GPS_Altitude, NO2 = NO2_ACES)
# 
#     ggplot() + geom_point(data=AEROMMA.NO2.by.CP, aes(x=Longitude, y=Latitude), color="darkred") +
#       geom_point(data=SeaRey.all, aes(x=longitude, y=latitude), color="blue") +
#       xlim(-88,-87.5) + ylim(42.2,42.7)

#Look at the area just near Chiwaukee
AEROMMA.NO2.by.CP <- AEROMMA.NO2 %>%
  dplyr::filter(Longitude > -87.85 & Longitude < -87.6 & Latitude > 42.4 & Latitude < 42.58 & !is.na(NO2_ACES) & MSL_GPS_Altitude < 3000) %>%
  dplyr::mutate(NO2_ACES = ifelse(NO2_ACES < 0, 0, NO2_ACES), #set all negative concentrations to zero
                time = ifelse(datetime.CST < as.POSIXct("2023-08-02 14:00:00", tzone = "Etc/GMT+6"), "midday", "pm"), platform = "DC-8") %>%
  dplyr::select(platform, datetime.CST, time, MSL_GPS_Altitude, NO2_ACES) %>%
  dplyr::rename(Altitude.m = MSL_GPS_Altitude, NO2 = NO2_ACES)

SeaRey.for.merge <- SeaRey.all %>%
  dplyr::mutate(time = ifelse(datetime.CST < as.POSIXct("2023-08-02 11:00:00", tz = "Etc/GMT+6"), "am",
                              ifelse(datetime.CST < as.POSIXct("2023-08-02 14:00:00", tz = "Etc/GMT+6"), "midday", "pm")), platform = "SeaRey") %>%
  # dplyr::select(platform, datetime.CST, time, Altitude_m_MSL, NO2_ppbv) %>%
  dplyr::select(platform, datetime.CST, time, altitude, no2) %>%
  dplyr::rename(Altitude.m = altitude, NO2 = no2)

SeaRey.DC8.NO2 <- bind_rows(AEROMMA.NO2.by.CP, SeaRey.for.merge)

#Trim data from before takeoff and after landing
SeaRey.DC8.NO2 <- SeaRey.DC8.NO2 %>%
  dplyr::filter((time == "am" & datetime.CST >= as.POSIXct("2023-08-02 09:05:27", tz = "Etc/GMT+6") & datetime.CST <= as.POSIXct("2023-08-02 10:49:52", tz="Etc/GMT+6")) |
                (time == "midday" & datetime.CST >= as.POSIXct("2023-08-02 11:43:34", tz = "Etc/GMT+6") & datetime.CST <= as.POSIXct("2023-08-02 13:23:55", tz="Etc/GMT+6")) |
                (time == "pm" & datetime.CST >= as.POSIXct("2023-08-02 14:12:48", tz = "Etc/GMT+6") & datetime.CST <= as.POSIXct("2023-08-02 16:56:25", tz="Etc/GMT+6")))

    # ggplot() + geom_point(data=AEROMMA.NO2.by.CP, aes(x=NO2_ACES, y=MSL_GPS_Altitude, color=datetime.CST, group=datetime.CST), shape = 2) +
    #   geom_point(data=SeaRey.all, aes(x=NO2_ppbv, y=Altitude_m_MSL, color=datetime.CST, group=datetime.CST), shape=16) +
    #   ylim(0,3000) + xlim(0,12)
    # 
    # ggplot() + geom_point(data=AEROMMA.NO2.by.CP, aes(x=datetime.CST, y=MSL_GPS_Altitude), shape = 2) +
    #   geom_point(data=SeaRey.all, aes(x=datetime.CST, y=Altitude_m_MSL, color=datetime.CST, group=datetime.CST), shape=16) +
    #   ylim(0,3000) #+ xlim(0,12)

    NO2 <- ggplot(SeaRey.DC8.NO2, aes(x=NO2, y=Altitude.m, color = time, shape = platform, size = platform, group = interaction(time,platform))) + facet_grid(.~time) +
      geom_rect(aes(xmin = -1, xmax = 11, ymin = 0, ymax = 180), fill = "gray50", color = NA, show.legend = FALSE) +
      geom_point(alpha=0.5, fill="black") +      # geom_point(size = 0.4, alpha=0.5) +
      scale_shape_manual(values = c(21,3)) + ylab("Altitude (m ASL)") + scale_size_manual(values = c(2,0.2)) +
      # scale_color_brewer(palette = "Set1") + 
      scale_color_okabeito(palette = "full") + #colorblind friendly
      # scale_x_continuous(limits = c(0,12), breaks = seq(0,12,by=4)) + 
      coord_cartesian(xlim = c(-0.2, 10.1)) +
      scale_y_continuous(limits = c(0,2000)) + scale_x_continuous(breaks = seq(0,12,by=4)) +
      xlab(expression("NO"[2]*" Concentration (ppb)")) +
      guides(color = guide_legend(position = "inside"), shape = guide_legend(position = "inside"), fill = "none") +
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.title.x = element_text(size=12),
            axis.text.x = element_text(size=11),
            panel.background = element_rect(fill = "white", color = "black"),
            panel.grid = element_line(color = "gray85"),
            strip.text = element_blank(),
            legend.title = element_blank(),
            legend.key = element_rect(fill = "transparent", color = NA),
            # Place legend inside the plot
            legend.position.inside = c(0.02, 0.9),
            legend.justification = c("left", "top"),
            legend.direction = "vertical",                             
            legend.background = element_rect(fill = scales::alpha("white", 0.8),
                                             color = "grey70"),
            legend.key.height = unit(0.35, "lines"),
            legend.key.width  = unit(0.7, "lines"),
            legend.text = element_text(size = 11))
    #   ggsave("Chiwaukee aircraft NO2 profiles.png", width = 2.5, height = 4)
    # ggsave("Chiwaukee aircraft NO2 profiles-2000m.png", width = 4, height = 4)
    # ggsave("Chiwaukee aircraft NO2 profiles-2000m-fill.png", width = 4, height = 4)
    
    
#Combine met sonde data (Temp and RH) with aircraft NO2 data

#Add white space to bottom of the sonde panels to make up for the 

# pad_white <- ggdraw() + theme(plot.background = element_rect(fill = "white", color = NA),
#                               panel.background = element_rect(fill = "white", color = NA))
# 
# row.1 <- plot_grid(e.no.axis, pad_white, nrow = 1, rel_widths = c(6, 1.3)) #allows space on the right side
    
        
    CP.met.NO2 <- cowplot::plot_grid(temp, RH, NO2, align = "v", nrow = 1, rel_widths = c(1.5,1.05,3), axis = "b")
    
    #Add panel labels
    CP.met.NO2 <- ggdraw(CP.met.NO2) +
      draw_plot_label(
        label = c("(a)","(b)","(c)","(d)","(e)"),
        x = c(0.105, 0.295, 0.485, 0.66, 0.835),
        y = c(0.97, 0.97, 0.97, 0.97, 0.97),
        hjust = 0, vjust = 1, size = 12
      )
    
    # ggsave("Chiwaukee vertical T-RH-NO2.png", CP.met.NO2, width = 7, height = 4)
    
    
#Plot SeaRey and DC-8 O3 -----------------------------------------------------------

AEROMMA.O3 <- read.table("AEROMMA-O3-CL_DC8_20230802_R0-no header.txt", sep=",", header=TRUE) #chemiluminescence O3

AEROMMA.O3 <- AEROMMA.O3 %>%
  left_join(., AEROMMA.nav, by="Time_Start") %>%
  dplyr::mutate(datetime.UTC = as.POSIXct("2023-08-02 00:00:00", tz="UTC") + Time_Start, datetime.CST = with_tz(datetime.UTC, tzone = "Etc/GMT+6"),
                O3_CL = ifelse(O3_CL == -9999, NA, O3_CL))    

#Look at the area just near Chiwaukee
AEROMMA.O3.by.CP <- AEROMMA.O3 %>%
  dplyr::filter(Longitude > -87.85 & Longitude < -87.6 & Latitude > 42.4 & Latitude < 42.58 & !is.na(O3_CL) & MSL_GPS_Altitude < 3000) %>%
  dplyr::mutate(time = ifelse(datetime.CST < as.POSIXct("2023-08-02 14:00:00", tzone = "Etc/GMT+6"), "midday", "pm"), platform = "DC-8") %>%
  dplyr::select(platform, datetime.CST, time, MSL_GPS_Altitude, O3_CL) %>%
  dplyr::rename(Altitude.m = MSL_GPS_Altitude, O3 = O3_CL)

SeaRey.O3 <- SeaRey.all %>%
  dplyr::mutate(time = ifelse(datetime.CST < as.POSIXct("2023-08-02 11:00:00", tz = "Etc/GMT+6"), "am",
                              ifelse(datetime.CST < as.POSIXct("2023-08-02 14:00:00", tz = "Etc/GMT+6"), "midday", "pm")), platform = "SeaRey") %>%
  # dplyr::select(platform, datetime.CST, time, Altitude_m_MSL, NO2_ppbv) %>%
  dplyr::select(platform, datetime.CST, time, altitude, ozone) %>%
  dplyr::rename(Altitude.m = altitude, O3 = ozone)
#Calculate 10s-averaged O3 for the SeaRey
SeaRey.O3.10s <- SeaRey.O3 %>%
  dplyr::mutate(datetime.10s = round_date(datetime.CST, unit = "10 seconds")) %>%
  dplyr::group_by(platform, time, datetime.10s) %>%
  dplyr::summarise(Altitude.m = mean(Altitude.m, na.rm = TRUE), O3 = mean(O3, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.nan(O3)) %>%
  dplyr::rename(datetime.CST = datetime.10s)

SeaRey.DC8.O3 <- bind_rows(AEROMMA.O3.by.CP, SeaRey.O3.10s)

#Trim SeaRey data from before takeoff and after landing
SeaRey.DC8.O3 <- SeaRey.DC8.O3 %>%
  dplyr::filter(platform == "DC-8" | platform == "SeaRey" &
                  (time == "am" & datetime.CST >= as.POSIXct("2023-08-02 09:05:27", tz = "Etc/GMT+6") & datetime.CST <= as.POSIXct("2023-08-02 10:49:52", tz="Etc/GMT+6")) |
                  (time == "midday" & datetime.CST >= as.POSIXct("2023-08-02 11:43:34", tz = "Etc/GMT+6") & datetime.CST <= as.POSIXct("2023-08-02 13:23:55", tz="Etc/GMT+6")) |
                  (time == "pm" & datetime.CST >= as.POSIXct("2023-08-02 14:12:48", tz = "Etc/GMT+6") & datetime.CST <= as.POSIXct("2023-08-02 16:56:25", tz="Etc/GMT+6")))
    
    # ggplot() + geom_point(data=AEROMMA.NO2.by.CP, aes(x=NO2_ACES, y=MSL_GPS_Altitude, color=datetime.CST, group=datetime.CST), shape = 2) +
    #   geom_point(data=SeaRey.all, aes(x=NO2_ppbv, y=Altitude_m_MSL, color=datetime.CST, group=datetime.CST), shape=16) +
    #   ylim(0,3000) + xlim(0,12)
    # 
    # ggplot() + geom_point(data=AEROMMA.NO2.by.CP, aes(x=datetime.CST, y=MSL_GPS_Altitude), shape = 2) +
    #   geom_point(data=SeaRey.all, aes(x=datetime.CST, y=Altitude_m_MSL, color=datetime.CST, group=datetime.CST), shape=16) +
    #   ylim(0,3000) #+ xlim(0,12)
    
    SeaRey.O3.plot <- ggplot(SeaRey.DC8.O3, aes(x=O3, y=Altitude.m, color = time, shape = platform, size = platform, group = interaction(time,platform))) + facet_grid(.~time) +
      geom_point(alpha=0.5) +      # geom_point(size = 0.4, alpha=0.5) +
      scale_shape_manual(values = c(1,3)) + scale_color_brewer(palette = "Set1") + ylab("Altitude (m ASL)") + scale_size_manual(values = c(1,0.4)) +
      # scale_x_continuous(limits = c(0,12), breaks = seq(0,12,by=4)) + 
      # scale_x_continuous(breaks = seq(0,12,by=4)) +
      scale_y_continuous(limits = c(0,2000)) + scale_x_continuous(limits = c(54,130)) +
      xlab(expression("O"[3]*" Concentration (ppb)")) +
      guides(color = guide_legend(position = "inside"), shape = guide_legend(position = "inside")) +
      theme(axis.title.x = element_text(size=14),
            axis.text.x = element_text(size=13),
            panel.background = element_rect(fill = "white", color = "black"),
            panel.grid = element_line(color = "gray85"),
            strip.text = element_blank(),
            legend.title = element_blank(),
            legend.key = element_rect(fill = "transparent", color = NA),
            # Place legend inside the plot
            legend.position.inside = c(0.02, 0.9),
            legend.justification = c("left", "top"),
            legend.direction = "vertical",                             
            legend.background = element_rect(fill = scales::alpha("white", 0.8),
                                             color = "grey70"),
            legend.key.height = unit(0.35, "lines"),
            legend.key.width  = unit(0.7, "lines"),
            legend.text = element_text(size = 11))
    # ggsave("Chiwaukee aircraft O3 profiles-2000m-10s means.png", width = 4, height = 4)
    
#Map out DC-8 and SeaRey (500-800 m for midday & 500-1600 m for pm - where there's overlap) tracks
AEROMMA.O3.CP.map <- AEROMMA.O3 %>%
  dplyr::filter(Longitude > -87.85 & Longitude < -87.6 & Latitude > 42.4 & Latitude < 42.58 & !is.na(O3_CL) & MSL_GPS_Altitude < 3000) %>%
  dplyr::mutate(time = ifelse(datetime.CST < as.POSIXct("2023-08-02 14:00:00", tzone = "Etc/GMT+6"), "midday", "pm"), platform = "DC-8")  %>%
  dplyr::select(platform, time,datetime.CST,MSL_GPS_Altitude, Latitude, Longitude, O3_CL) %>%
  dplyr::rename(O3 = O3_CL, Altitude.mASL = MSL_GPS_Altitude) #%>%
  # dplyr::filter((time == "midday" & Altitude.mASL < 1600) | 
  #                 time == "pm" & Altitude.mASL < 1600)

SeaRey.O3.map <- SeaRey.all %>%
  dplyr::mutate(time = ifelse(datetime.CST < as.POSIXct("2023-08-02 11:00:00", tz = "Etc/GMT+6"), "am",
                              ifelse(datetime.CST < as.POSIXct("2023-08-02 14:00:00", tz = "Etc/GMT+6"), "midday", "pm")), platform = "SeaRey")    
#Calculate 10s-averaged O3 for the SeaRey & drop the am flight
SeaRey.O3.map.10s <- SeaRey.O3.map %>%
  dplyr::mutate(datetime.10s = round_date(datetime.CST, unit = "10 seconds")) %>%
  dplyr::group_by(platform, time, datetime.10s) %>%
  dplyr::summarise(Altitude.mASL = mean(altitude, na.rm = TRUE), O3 = mean(ozone, na.rm = TRUE), Latitude = mean(latitude), Longitude = mean(longitude)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.nan(O3) & time != "am") %>%
  dplyr::rename(datetime.CST = datetime.10s) #%>%
  # dplyr::filter((time == "midday" & Altitude.mASL > 500 & Altitude.mASL < 1600) | 
                  # time == "pm" & Altitude.mASL > 500 & Altitude.mASL < 1600)

SeaRey.DC8.O3.map <- bind_rows(AEROMMA.O3.CP.map, SeaRey.O3.map.10s)

#Trim SeaRey data from before takeoff and after landing & add labels for altitude
SeaRey.DC8.O3.map <- SeaRey.DC8.O3.map %>%
  dplyr::filter(platform == "DC-8" | platform == "SeaRey" &
                  (time == "am" & datetime.CST >= as.POSIXct("2023-08-02 09:05:27", tz = "Etc/GMT+6") & datetime.CST <= as.POSIXct("2023-08-02 10:49:52", tz="Etc/GMT+6")) |
                  (time == "midday" & datetime.CST >= as.POSIXct("2023-08-02 11:43:34", tz = "Etc/GMT+6") & datetime.CST <= as.POSIXct("2023-08-02 13:23:55", tz="Etc/GMT+6")) |
                  (time == "pm" & datetime.CST >= as.POSIXct("2023-08-02 14:12:48", tz = "Etc/GMT+6") & datetime.CST <= as.POSIXct("2023-08-02 16:56:25", tz="Etc/GMT+6"))) %>%
  dplyr::mutate(height = ifelse(Altitude.mASL <= 800, "500-800 m", "800-1600 m"))

    # map.O3 <- ggplot(SeaRey.DC8.O3.map, aes(x=Longitude, y=Latitude, color=O3, shape=platform, group=interaction(O3,platform))) + geom_point() + 
    map.O3 <- ggplot(SeaRey.DC8.O3.map, aes(x=Longitude, y=Latitude, color=datetime.CST, shape=platform, group=interaction(datetime.CST,platform))) + geom_point() + 
      facet_grid(height~time) + #scale_color_datetime(name = "Time (CST)", date_labels = "%H:%M", palette = "Spectral") +
      # scale_color_distiller(name = "Time (CST)", date_labels = "%H:%M", palette = "Spectral")
      scale_color_gradientn(colors = rev(brewer.pal(11, "Spectral")), name = "Time (CST)",
        labels = function(x) format(as.POSIXct(x, origin = "1970-01-01"), "%H:%M"))
    # ggsave("DC-8 and SeaRey O3 mapped 500-1600 m.png", width = 6, height = 6)
    # ggsave("DC-8 and SeaRey O3 mapped 500-1600 m-by time.png", width = 6, height = 6)
    
    
#Plot just SeaRey & DC-8 data from very close to each other - by time, altitude, and lat/lon
    
SeaRey.DC8.O3.CP <- SeaRey.DC8.O3.map %>%
  dplyr::filter((platform == "DC-8" & Altitude.mASL < 2000)| (platform == "SeaRey" & Latitude > 42.45 & Longitude > -87.83)) %>% #trim just top raster and nearby
  dplyr::mutate(time.bin = format(round_date(datetime.CST, unit = "30 minutes"), "%H:%M"))


time.list <- c("midday","pm")

    for(i in time.list)
    {
      time.subset <- dplyr::filter(SeaRey.DC8.O3.CP, time == i)
    
      plot <- ggplot(time.subset, aes(x=O3, y=Altitude.mASL, color = time.bin, shape = platform, group = interaction(time.bin,platform))) + #facet_grid(.~time) +
        geom_point() +      # geom_point(size = 0.4, alpha=0.5) +
        scale_shape_manual(values = c(1,3)) + ylab("Altitude (m ASL)") + 
        scale_color_brewer(palette = "Spectral") +
        # scale_color_gradientn(colors = rev(brewer.pal(11, "Spectral")), name = "Time (CST)",
        #                       labels = function(x) format(as.POSIXct(x, origin = "1970-01-01"), "%H:%M")) +
        # scale_x_continuous(limits = c(0,12), breaks = seq(0,12,by=4)) + 
        # scale_x_continuous(breaks = seq(0,12,by=4)) +
        scale_y_continuous(limits = c(0,2000)) + scale_x_continuous(limits = c(54,130)) +
        xlab(expression("O"[3]*" Concentration (ppb)")) + ggtitle(paste0("Ozone near Chiwaukee - ", i)) +
        # guides(color = guide_legend(position = "inside"), shape = guide_legend(position = "inside")) +
        theme(axis.title.x = element_text(size=12),
              axis.text.x = element_text(size=11),
              panel.background = element_rect(fill = "white", color = "black"),
              panel.grid = element_line(color = "gray85"),
              strip.text = element_blank(),
              legend.title = element_blank(),
              legend.key = element_rect(fill = "transparent", color = NA),
              # # Place legend inside the plot
              # legend.position.inside = c(0.02, 0.9),
              # legend.justification = c("left", "top"),
              # legend.direction = "vertical",                             
              legend.background = element_rect(fill = scales::alpha("white", 0.8)),
              # legend.key.height = unit(0.35, "lines"),
              # legend.key.width  = unit(0.7, "lines"),
              legend.text = element_text(size = 11))
      # ggsave(paste0("Chiwaukee aircraft O3 profiles-near Chiwaukee - ", i, ".png"), width = 4, height = 4)
      
      map <- ggplot(time.subset, aes(x=Longitude, y=Latitude, color=Altitude.mASL, shape=platform, group=interaction(Altitude.mASL,platform))) + geom_point() + 
        facet_wrap(~time.bin) + #scale_color_datetime(name = "Time (CST)", date_labels = "%H:%M", palette = "Spectral") +
        scale_color_distiller(palette = "Spectral") + ggtitle(paste0("Chiwaukee offshore - ", i))
      # ggsave(paste0("Chiwaukee aircraft maps - by time-altitude - ", i, ".png"), width = 8, height = 4)
      
      map.O3 <- ggplot(time.subset, aes(x=Longitude, y=Latitude, color=O3, shape=platform, group=interaction(O3,platform))) + geom_point() + 
        facet_wrap(~time.bin) + #scale_color_datetime(name = "Time (CST)", date_labels = "%H:%M", palette = "Spectral") +
        scale_color_distiller(palette = "Spectral") + ggtitle(paste0("Chiwaukee offshore - ", i))
      # ggsave(paste0("Chiwaukee aircraft maps - by time-ozone - ", i, ".png"), width = 8, height = 4)
    }

#Plot maps of SeaRey O3 by altitude and flight

SeaRey.O3.map.10s.all <- SeaRey.O3.map %>%
  dplyr::mutate(datetime.10s = round_date(datetime.CST, unit = "10 seconds")) %>%
  dplyr::group_by(platform, time, datetime.10s) %>%
  dplyr::summarise(Altitude.mASL = mean(altitude, na.rm = TRUE), O3 = mean(ozone, na.rm = TRUE), Latitude = mean(latitude), Longitude = mean(longitude)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.nan(O3)) %>%
  dplyr::rename(datetime.CST = datetime.10s) %>%
  dplyr::mutate(alt.bin = ifelse(Altitude.mASL < 270, "180-250 m", ifelse(Altitude.mASL < 425, "250-425 m", ifelse(Altitude.mASL < 500, "425-500 m", ">500 m"))))

#Determine what altitudes are the centers of each raster pattern
    # ggplot(SeaRey.O3.map.10s.all, aes(x=Altitude.mASL)) + geom_histogram(binwidth = 10) + scale_x_continuous(breaks = seq(0,1600,by=50))
SeaRey.O3.map.10s.all$alt.bin <- factor(SeaRey.O3.map.10s.all$alt.bin, levels = c("180-250 m","250-425 m", "425-500 m", ">500 m"))

SeaRey.O3.sf <- st_as_sf(SeaRey.O3.map.10s.all, coords = c("Longitude","Latitude"), crs = 4326, remove = FALSE)
SeaRey.O3.sf <- st_transform(SeaRey.O3.sf, 3857)

    map.SeaRey.O3 <- ggplot() + annotation_map_tile(type = "cartolight", zoomin = 0) + geom_sf(data = SeaRey.O3.sf, size = 1.5, aes(color = O3)) + facet_grid(time ~ alt.bin) +
      scale_color_gradientn(colors = matlab.like(87), limits = c(60,127), name = "Ozone (ppb)") + 
      # coord_sf(expand = FALSE) +   
      scale_x_continuous(breaks = seq(-87.9, -87.7, by=0.1), labels = label_number(accuracy = 0.1)) +
      scale_y_continuous(breaks = seq(42.3, 42.6, by=0.1), labels = label_number(accuracy = 0.1)) +
      theme(strip.background = element_rect(fill = "white"),
            strip.text = element_text(size = 12))
    # ggsave("SeaRey ozone maps by altitude bin - Aug2.png", width = 8.5, height = 7.5)
    
#Save and export lat/lon/altitude O3 data for SeaRey and DC-8 for Laura to include with the HSRL2 O3 lidar profiles
    
#Look at AEROMMA O3 data in the Chiwaukee spiral and the Gary spiral
AEROMMA.O3.East.spirals <- AEROMMA.O3 %>%
  dplyr::filter(((Longitude > -87.4 & Longitude < -87.25 & Latitude > 41.5 & Latitude < 41.8) |
    (Longitude > -87.85 & Longitude < -87.6 & Latitude > 42.4 & Latitude < 42.58)) &
                  !is.na(O3_CL) & MSL_GPS_Altitude < 2000) %>%
  dplyr::mutate(time = ifelse(datetime.CST < as.POSIXct("2023-08-02 14:00:00", tzone = "Etc/GMT+6"), "midday", "pm"), platform = "DC-8") %>%
  # dplyr::select(platform, datetime.CST, time, MSL_GPS_Altitude, O3_CL) %>%
  dplyr::rename(Altitude.mASL = MSL_GPS_Altitude, O3 = O3_CL) %>%
  dplyr::select(platform,time,datetime.CST,Altitude.mASL,O3,Latitude,Longitude)

    # map <- ggplot(AEROMMA.O3.East.spirals, aes(x=Longitude, y=Latitude, color=Altitude.m, group=Altitude.m)) + geom_point() + 
    #   scale_color_distiller(palette = "Spectral")

SeaRey.O3.map.10s.all <- dplyr::select(SeaRey.O3.map.10s.all, -alt.bin)

SeaRey.AEROMMA.East.O3.for.GMAP.compare <- bind_rows(SeaRey.O3.map.10s.all, AEROMMA.O3.East.spirals)
# write.csv(SeaRey.AEROMMA.East.O3.for.GMAP.compare, "SeaRey-DC8 O3 along GMAP east rasters-Aug2.csv", row.names = FALSE)
# SeaRey.AEROMMA.East.O3.for.GMAP.compare <- read.csv("SeaRey-DC8 O3 along GMAP east rasters-Aug2.csv", header = TRUE)
# SeaRey.AEROMMA.East.O3.for.GMAP.compare <- SeaRey.AEROMMA.East.O3.for.GMAP.compare %>%
#   dplyr::mutate(datetime.CST = as.POSIXct(datetime.CST, tzone = "Etc/GMT+6"))

    ggplot(SeaRey.AEROMMA.East.O3.for.GMAP.compare, aes(x=Latitude, y=Altitude.mASL, color=O3, group=O3)) + geom_point() + facet_grid(time~.) +
      scale_color_gradientn(colors = matlab.like(87), limits = c(38,127), name = "Ozone (ppb)") + xlim(42.2, 42.65) +
      theme(rect = element_rect(fill = "transparent"),
            panel.background = element_rect(fill = "transparent"))
    # ggsave("SeaRey-DC-8 ozone vs latitude - Aug2.png", width = 6, height=6)
    
    # ggplot(data=subset(SeaRey.AEROMMA.East.O3.for.GMAP.compare, time=="pm"), aes(x=Latitude, y=Altitude.mASL, color=datetime.CST, group=datetime.CST)) + 
    #   geom_point() + facet_grid(time~.) + xlim(42.2, 42.65) +
    #   scale_color_gradientn(colors = rev(brewer.pal(11, "Spectral")), name = "Time (CST)",
    #                         labels = function(x) format(as.POSIXct(x, origin = "1970-01-01", tzone = "Etc/GMT+6"), "%H:%M")) #time zone is wrong in plot
    # ggsave("SeaRey-DC-8 time vs latitude - Aug2 pm.png", width = 5.5, height=2)

    
#Plots for +/- 30 min from DC-8 time:    
    # ggplot(data=subset(SeaRey.AEROMMA.East.O3.for.GMAP.compare, datetime.CST > as.POSIXct("2023-08-02 15:11:00", tzone = "Etc/GMT+6") &
    #                      datetime.CST < as.POSIXct("2023-08-02 16:11:00", tzone = "Etc/GMT+6")),
    #        # aes(x=Latitude, y=Altitude.mASL, color=O3, group=O3)) +
    #        aes(x=Longitude, y=Latitude, color=O3, group=O3)) +
    #   geom_point() + facet_grid(time~.) + ggtitle("SeaRey-DC-8 pm 15:11-16:10 CST") +
    #   # xlim(42.2, 42.65) +
    #   scale_color_gradientn(colors = matlab.like(87), limits = c(55,127), name = "Ozone (ppb)")
    # # ggsave("SeaRey-DC-8 time vs latitude - Aug2 1511-1610.png", width = 5.5, height=2)
    # ggsave("SeaRey-DC-8 O3 map - Aug2 1511-1610.png", width = 4.5, height=4)
    # 
    # ggplot(data=subset(SeaRey.AEROMMA.East.O3.for.GMAP.compare, datetime.CST > as.POSIXct("2023-08-02 11:46:00", tzone = "Etc/GMT+6") &
    #                      datetime.CST < as.POSIXct("2023-08-02 12:46:00", tzone = "Etc/GMT+6")),
    #        # aes(x=Latitude, y=Altitude.mASL, color=O3, group=O3)) +
    #        aes(x=Longitude, y=Latitude, color=O3, group=O3)) +
    #   geom_point() + facet_grid(time~.) + ggtitle("SeaRey-DC-8 pm 11:46-12:45 CST") +
    #   # xlim(42.2, 42.65) +
    #   scale_color_gradientn(colors = matlab.like(87), limits = c(55,127), name = "Ozone (ppb)")
    # # ggsave("SeaRey-DC-8 time vs latitude - Aug2 1146-1245.png", width = 5.5, height=2)
    # ggsave("SeaRey-DC-8 O3 map - Aug2 1146-1245.png", width = 4.5, height=4)
    # 
    # ggplot(data=subset(SeaRey.AEROMMA.East.O3.for.GMAP.compare, time=="midday"), aes(x=Latitude, y=Altitude.mASL, color=datetime.CST, group=datetime.CST)) +
    #   geom_point() + facet_grid(time~.) + xlim(42.2, 42.65) +
    #   scale_color_gradientn(colors = rev(brewer.pal(11, "Spectral")), name = "Time (CST)",
    #                         labels = function(x) format(as.POSIXct(x, origin = "1970-01-01"), "%H:%M"))
    # ggsave("SeaRey-DC-8 time vs latitude - Aug2 midday.png", width = 5.5, height=2)
    # 
    # ggplot(SeaRey.AEROMMA.East.O3.for.GMAP.compare, aes(x=Longitude, y=Latitude, color=O3, group = O3)) + geom_point() + facet_grid(time~.) +
    #   scale_color_gradientn(colors = matlab.like(87), limits = c(38,127), name = "Ozone (ppb)") + ylim(42.2, 42.65) + xlim(-87.95, -87.6)
    # ggsave("SeaRey-DC-8 ozone vs lat-long - Aug2.png", width = 4, height=6)
    
##Figures for paper (SeaRey and DC-8 O3 close in time) --------------------------------------------------
#Panels on left of altitude vs latitude & right of lat vs longitude, colors for ozone - midday and pm
# midday.test <- dplyr::filter(SeaRey.AEROMMA.East.O3.for.GMAP.compare, datetime.CST > as.POSIXct("2023-08-02 11:46:00", tz = "Etc/GMT+6") & 
#                              datetime.CST < as.POSIXct("2023-08-02 12:46:00", tz = "Etc/GMT+6"))
    
#Add triangles behind DC-8 data to distinguish the two platforms
AEROMMA.East.O3.for.GMAP.compare <- SeaRey.AEROMMA.East.O3.for.GMAP.compare %>%
  dplyr::filter(platform == "DC-8")
        
    midday.alt <- ggplot() +
      geom_rect(aes(xmin = 42.25, xmax = 42.6, ymin = 0, ymax = 180), fill = "gray50", color = NA, show.legend = FALSE) + 
      geom_point(data=subset(AEROMMA.East.O3.for.GMAP.compare, datetime.CST > as.POSIXct("2023-08-02 11:46:00", tz = "Etc/GMT+6") & 
                               datetime.CST < as.POSIXct("2023-08-02 12:46:00", tz = "Etc/GMT+6")), 
                 aes(x=Latitude, y=Altitude.mASL, group = 1), size = 2) + 
      geom_point(data=subset(SeaRey.AEROMMA.East.O3.for.GMAP.compare, datetime.CST > as.POSIXct("2023-08-02 11:46:00", tz = "Etc/GMT+6") & 
                                    datetime.CST < as.POSIXct("2023-08-02 12:46:00", tz = "Etc/GMT+6")), 
                      aes(x=Latitude, y=Altitude.mASL, color=O3, group=O3)) + 
      xlim(42.25, 42.6) + ylab(NULL) + xlab("Latitude") +
      scale_color_gradientn(colors = matlab.like(87), limits = c(57,122), name = "Ozone (ppb)") +
      theme(axis.title.x = element_blank(),
        # axis.title.x = element_text(size=11),
            axis.text = element_text(size=12),
            panel.background = element_rect(fill = "white", color = "black"),
            panel.grid = element_line(color = "gray85"),
            legend.title = element_text(size = 12, angle = 90, hjust = 0.5),
            legend.text = element_text(size = 10),
            legend.direction = "vertical",
            legend.box = "horizontal",
            legend.title.position = "right")
    
    pm.alt <- ggplot() +
      geom_rect(aes(xmin = 42.25, xmax = 42.6, ymin = 0, ymax = 180), fill = "gray50", color = NA, show.legend = FALSE) + 
      geom_point(data=subset(AEROMMA.East.O3.for.GMAP.compare, datetime.CST > as.POSIXct("2023-08-02 15:11:00", tz = "Etc/GMT+6") & 
                               datetime.CST < as.POSIXct("2023-08-02 16:11:00", tz = "Etc/GMT+6")), 
                 aes(x=Latitude, y=Altitude.mASL, group = 1), size = 2) + 
      geom_point(data=subset(SeaRey.AEROMMA.East.O3.for.GMAP.compare, datetime.CST > as.POSIXct("2023-08-02 15:11:00", tz = "Etc/GMT+6") & 
                                   datetime.CST < as.POSIXct("2023-08-02 16:11:00", tz = "Etc/GMT+6")), 
                      aes(x=Latitude, y=Altitude.mASL, color=O3, group=O3)) + 
      xlim(42.25, 42.6) + ylab(NULL) + xlab("Latitude") +
      scale_color_gradientn(colors = matlab.like(87), limits = c(57,122), name = "Ozone (ppb)") +
      theme(axis.title.x = element_text(size=14),
            axis.text = element_text(size=12),
            panel.background = element_rect(fill = "white", color = "black"),
            panel.grid = element_line(color = "gray85"),
            legend.title = element_text(size = 12, angle = 90, hjust = 0.5),
            legend.text = element_text(size = 10),
            legend.direction = "vertical",
            legend.box = "horizontal",
            legend.title.position = "right")

#Make maps
midday.data <- dplyr::filter(SeaRey.AEROMMA.East.O3.for.GMAP.compare, datetime.CST > as.POSIXct("2023-08-02 11:46:00", tz = "Etc/GMT+6") & 
                               datetime.CST < as.POSIXct("2023-08-02 12:46:00", tz = "Etc/GMT+6"))
midday.O3.sf <- st_as_sf(midday.data, coords = c("Longitude","Latitude"), crs = 4326, remove = FALSE)
midday.O3.transform <- st_transform(midday.O3.sf, 4326)

midday.DC8.data <- dplyr::filter(AEROMMA.East.O3.for.GMAP.compare, datetime.CST > as.POSIXct("2023-08-02 11:46:00", tz = "Etc/GMT+6") & 
                               datetime.CST < as.POSIXct("2023-08-02 12:46:00", tz = "Etc/GMT+6"))
midday.DC8.O3.sf <- st_as_sf(midday.DC8.data, coords = c("Longitude","Latitude"), crs = 4326, remove = FALSE)
midday.DC8.O3.transform <- st_transform(midday.DC8.O3.sf, 4326)

    midday.map <- ggplot() + annotation_map_tile(type = "cartolight", zoomin = 0) + 
      geom_sf(data = midday.DC8.O3.transform, size = 2) + 
      geom_sf(data = midday.O3.transform, size = 1.5, aes(color = O3)) + 
      scale_color_gradientn(colors = matlab.like(87), limits = c(57,122), name = "Ozone (ppb)") + 
      # coord_sf(expand = FALSE) +   
      scale_x_continuous(breaks = seq(-87.9, -87.7, by=0.1), labels = label_number(accuracy = 0.1)) +
      scale_y_continuous(breaks = seq(42.3, 42.6, by=0.1), labels = label_number(accuracy = 0.1)) +
      coord_sf(crs = sf::st_crs(4326), xlim = c(-87.93,-87.63), ylim = c(42.27, 42.62), expand = FALSE) +
      theme(axis.title.x = element_text(size=14),
            axis.text = element_text(size=12),
            legend.position = "none",
            panel.background = element_rect(fill = "white", color = "black"))

pm.data <- dplyr::filter(SeaRey.AEROMMA.East.O3.for.GMAP.compare, datetime.CST > as.POSIXct("2023-08-02 15:11:00", tz = "Etc/GMT+6") & 
                           datetime.CST < as.POSIXct("2023-08-02 16:11:00", tz = "Etc/GMT+6"))
pm.O3.sf <- st_as_sf(pm.data, coords = c("Longitude","Latitude"), crs = 4326, remove = FALSE)
pm.O3.transform <- st_transform(pm.O3.sf, 4326)

pm.DC8.data <- dplyr::filter(AEROMMA.East.O3.for.GMAP.compare, datetime.CST > as.POSIXct("2023-08-02 15:11:00", tz = "Etc/GMT+6") & 
                           datetime.CST < as.POSIXct("2023-08-02 16:11:00", tz = "Etc/GMT+6"))
pm.DC8.O3.sf <- st_as_sf(pm.DC8.data, coords = c("Longitude","Latitude"), crs = 4326, remove = FALSE)
pm.DC8.O3.transform <- st_transform(pm.DC8.O3.sf, 4326)
    
    pm.map <- ggplot() + annotation_map_tile(type = "cartolight", zoomin = 0) + 
      geom_sf(data = pm.DC8.O3.transform, size = 2) + 
      geom_sf(data = pm.O3.transform, size = 1.5, aes(color = O3)) + 
      scale_color_gradientn(colors = matlab.like(87), limits = c(57,122), name = "Ozone (ppb)") + xlab("Longitude") +
      # coord_sf(expand = FALSE) +   
      scale_x_continuous(breaks = seq(-87.9, -87.7, by=0.1), labels = label_number(accuracy = 0.1)) +
      scale_y_continuous(breaks = seq(42.3, 42.6, by=0.1), labels = label_number(accuracy = 0.1)) +
      coord_sf(crs = sf::st_crs(4326), xlim = c(-87.93,-87.63), ylim = c(42.27, 42.62), expand = FALSE) +
      theme(axis.title.x = element_text(size=14),
            axis.text = element_text(size=12),
            legend.position = "none",
            panel.background = element_rect(fill = "white", color = "black"))
  
#Combine plots  
#Drop x-axis from the midday altitude panel
    # midday.alt.no.axis <- midday.alt + theme(axis.title = element_blank())

#Extract shared legend then drop legends (altitude plots)
    legend <- cowplot::get_legend(midday.alt + theme(legend.box.margin = margin(0,0,0,12)))
    
    midday.alt.no.legend <- midday.alt + ggplot2::theme(legend.position="none")
    pm.alt.no.legend <- pm.alt + ggplot2::theme(legend.position="none")
    
#Combine plots
    SeaRey.DC8.alt.plots <- plot_grid(midday.alt.no.legend, pm.alt.no.legend, nrow = 2, rel_heights = c(1,1.045), align = "v", axis = "l")
    axis.altitude <- ggdraw() + draw_label("Altitude (m ASL)", angle=90, size=14) #add shared axis title
    SeaRey.DC8.alt.plots <- plot_grid(axis.altitude, SeaRey.DC8.alt.plots, ncol = 2, rel_widths = c(0.06,1))

    SeaRey.DC8.maps <- plot_grid(midday.map, pm.map, nrow = 2, rel_heights = c(1,1.045), align = "v", axis = "l")
    axis.latitude <- ggdraw() + draw_label("Latitude", angle=90, size=14) #add shared axis title
    SeaRey.DC8.maps <- plot_grid(axis.latitude, SeaRey.DC8.maps, ncol = 2, rel_widths = c(0.06,1))

    SeaRey.DC8.alt.maps <- plot_grid(SeaRey.DC8.alt.plots, SeaRey.DC8.maps, legend, nrow=1, rel_widths = c(1.5,1,0.35))
    SeaRey.DC8.alt.maps <- ggdraw() + draw_plot(SeaRey.DC8.alt.maps) +
      theme(plot.background = element_rect(fill = "white", color = NA))

    #Add panel labels
    SeaRey.DC8.alt.maps <- ggdraw(SeaRey.DC8.alt.maps) +
      draw_plot_label(
        label = c("(a)","(b)","(c)","(d)"),
        x = c(0.1, 0.615, 0.1, 0.615),
        y = c(0.98, 0.98, 0.49, 0.49),
        hjust = 0, vjust = 1, size = 14
      )
    
    ggsave("SeaRey-DC-8 ozone within 30 min - Aug2.png", SeaRey.DC8.alt.maps, width = 8.5, height = 8)
    

    

    
    
    
#Plot acetonitrile profiles near Chiwaukee from AEROMMA -----------------------------------------

Acetonit <- read.table("AEROMMA-NOAALTOF_DC8_20230802_R0-no header.txt", sep=",", header = TRUE)
Acetonit <- Acetonit %>%
  left_join(., AEROMMA.nav, by="Time_Start") %>%
  dplyr::rename(CH3CN = CH3CN_NOAALTOF_ppbv) %>%
  dplyr::mutate(datetime.UTC = as.POSIXct("2023-08-02 00:00:00", tz="UTC") + Time_Start, datetime.CST = with_tz(datetime.UTC, tzone = "Etc/GMT+6"),
                CH3CN = ifelse(CH3CN == -9999, NA, CH3CN))  %>%
  dplyr::select(17,13:15,3)
Acetonit.CP <- Acetonit %>%
  dplyr::filter(Longitude > -87.85 & Longitude < -87.6 & Latitude > 42.4 & Latitude < 42.58 & !is.na(CH3CN) & MSL_GPS_Altitude < 2000) %>%
  dplyr::rename(Altitude.m = MSL_GPS_Altitude) %>%
  dplyr::mutate(spiral = ifelse (datetime.CST < as.POSIXct("2023-08-02 14:00:00", tz = "Etc/GMT+6"), "midday", "pm"))

Acetonit.CP.mean <- Acetonit.CP %>%
  dplyr::mutate(alt.bin = round_any(Altitude.m, 100)) %>%
  dplyr::group_by(spiral, alt.bin) %>%
  dplyr::summarise(CH3.CN = mean(CH3CN, na.rm = TRUE)) %>%
  dplyr::ungroup()

    Acetonit.profile <- ggplot(Acetonit.CP, aes(x=CH3CN, y=Altitude.m, color = spiral, group = spiral)) + 
      geom_point() +      # geom_point(size = 0.4, alpha=0.5) +
      scale_color_brewer(palette = "Paired") + ylab("Altitude (m ASL)") + 
      xlab(expression("Acetonitrile Concentration (ppb)")) +
      theme(axis.title.x = element_text(size=12),
            axis.text.x = element_text(size=11),
            panel.background = element_rect(fill = "white", color = "black"),
            panel.grid = element_line(color = "gray85"),
            strip.text = element_blank(),
            legend.title = element_blank(),
            legend.key = element_rect(fill = "transparent", color = NA),
            # Place legend inside the plot
            # legend.position.inside = c(0.02, 0.9),
            # legend.justification = c("left", "top"),
            # legend.direction = "vertical",                             
            legend.background = element_rect(fill = scales::alpha("white", 0.8)),
            legend.key.height = unit(0.35, "lines"),
            legend.key.width  = unit(0.7, "lines"),
            legend.text = element_text(size = 11))
    Acetonit.profile <- Acetonit.profile + geom_path(data = Acetonit.CP.mean, aes(x=CH3.CN, y=alt.bin, color = spiral, group=spiral), linewidth=1)
    # ggsave("Acetonitrile profile near Chiwaukee Aug2.png", height = 5, width = 4)


#Wind Pro Lidar ----------------------------------------

#August 2 UTC
windpro.data.Aug2 <- nc_open("staqs-halo-vad_chiwaukee_prairie_20230802_r1.cdf")

LIDAR.ws.Aug2 <- ncvar_get(windpro.data.Aug2, "speed")
LIDAR.wd.Aug2 <- ncvar_get(windpro.data.Aug2, "dir")
LIDAR.time.Aug2 <- ncvar_get(windpro.data.Aug2, "times")
LIDAR.altitude.Aug2 <- ncvar_get(windpro.data.Aug2, "height")
LIDAR.snr.Aug2 <- ncvar_get(windpro.data.Aug2, "snr")

LIDAR.ws.Aug2 <- as.data.frame(LIDAR.ws.Aug2)
LIDAR.ws.Aug2 <- LIDAR.ws.Aug2 %>%
  dplyr::mutate(altitude.kmagl = LIDAR.altitude.Aug2) %>%
  pivot_longer(1:283, names_to = "time.index", values_to = "ws")

LIDAR.wd.Aug2 <- as.data.frame(LIDAR.wd.Aug2)
LIDAR.wd.Aug2 <- LIDAR.wd.Aug2 %>%
  dplyr::mutate(altitude.kmagl = LIDAR.altitude.Aug2) %>%
  pivot_longer(1:283, names_to = "time.index", values_to = "wd")

LIDAR.snr.Aug2 <- as.data.frame(LIDAR.snr.Aug2)
LIDAR.snr.Aug2 <- LIDAR.snr.Aug2 %>%
  dplyr::mutate(altitude.kmagl = LIDAR.altitude.Aug2) %>%
  pivot_longer(1:283, names_to = "time.index", values_to = "snr")

LIDAR.winds.Aug2 <- full_join(LIDAR.ws.Aug2, LIDAR.wd.Aug2, by=c("time.index", "altitude.kmagl"))
LIDAR.winds.Aug2 <- full_join(LIDAR.winds.Aug2, LIDAR.snr.Aug2, by=c("time.index", "altitude.kmagl"))
LIDAR.winds.Aug2 <- LIDAR.winds.Aug2 %>%
  dplyr::mutate(hour = rep(LIDAR.time.Aug2, 298), datetime.UTC = as.POSIXct("2023-08-02 00:00:00", tz="UTC") + hour*60*60, 
                datetime.CST = with_tz(datetime.UTC, tzone = "Etc/GMT+6"))

#August 3 UTC
windpro.data.Aug3 <- nc_open("staqs-halo-vad_chiwaukee_prairie_20230803_r1.cdf")

LIDAR.ws.Aug3 <- ncvar_get(windpro.data.Aug3, "speed")
LIDAR.wd.Aug3 <- ncvar_get(windpro.data.Aug3, "dir")
LIDAR.time.Aug3 <- ncvar_get(windpro.data.Aug3, "times")
LIDAR.altitude.Aug3 <- ncvar_get(windpro.data.Aug3, "height")
LIDAR.snr.Aug3 <- ncvar_get(windpro.data.Aug3, "snr")

LIDAR.ws.Aug3 <- as.data.frame(LIDAR.ws.Aug3)
LIDAR.ws.Aug3 <- LIDAR.ws.Aug3 %>%
  dplyr::mutate(altitude.kmagl = LIDAR.altitude.Aug3) %>%
  pivot_longer(1:281, names_to = "time.index", values_to = "ws")

LIDAR.wd.Aug3 <- as.data.frame(LIDAR.wd.Aug3)
LIDAR.wd.Aug3 <- LIDAR.wd.Aug3 %>%
  dplyr::mutate(altitude.kmagl = LIDAR.altitude.Aug3) %>%
  pivot_longer(1:281, names_to = "time.index", values_to = "wd")

LIDAR.snr.Aug3 <- as.data.frame(LIDAR.snr.Aug3)
LIDAR.snr.Aug3 <- LIDAR.snr.Aug3 %>%
  dplyr::mutate(altitude.kmagl = LIDAR.altitude.Aug3) %>%
  pivot_longer(1:281, names_to = "time.index", values_to = "snr")

LIDAR.winds.Aug3 <- full_join(LIDAR.ws.Aug3, LIDAR.wd.Aug3, by=c("time.index", "altitude.kmagl"))
LIDAR.winds.Aug3 <- full_join(LIDAR.winds.Aug3, LIDAR.snr.Aug3, by=c("time.index", "altitude.kmagl"))
LIDAR.winds.Aug3 <- LIDAR.winds.Aug3 %>%
  dplyr::mutate(hour = rep(LIDAR.time.Aug3, 298), datetime.UTC = as.POSIXct("2023-08-03 00:00:00", tz="UTC") + hour*60*60, 
                datetime.CST = with_tz(datetime.UTC, tzone = "Etc/GMT+6"))

#Lidar winds - Aug 2 CST
LIDAR.winds.Aug2.3000m <- bind_rows(LIDAR.winds.Aug2, LIDAR.winds.Aug3)
LIDAR.winds.Aug2.3000m <- LIDAR.winds.Aug2.3000m %>%
  dplyr::mutate(Date = as.Date(datetime.CST, tz = "Etc/GMT+6")) %>%
  dplyr::filter(Date == as.Date("2023-08-02", tz = "Etc/GMT+6")) %>%
   dplyr::mutate(ws = ifelse(is.nan(ws), NA, ws), wd = ifelse(is.nan(wd), NA, wd), 
                altitude.masl = altitude.kmagl*1000 + 182) %>% #assume the lidar is at 182 m - altitude of Chiwaukee trailer + 3 m because on the roof 
  dplyr::select(7,8,10,3,4,5) %>%
  dplyr::filter(altitude.masl < 3000 & snr > 0.008) %>% #apply threshold of 0.008 for snr (per Tim Wagner 11/3/25)
  # dplyr::mutate(ws = ifelse(ws >= 50, NA, ws)) %>% #per Tim Wagner 10/30/25, cut all ws >= 50 m/s - he needs to find the correct way to do this
  dplyr::mutate(ws.knots = ws * 1.94384, u.wind = -abs(ws)*sin(wd*pi/180)) %>% #calculate ws in knots (for wind barbs)
  dplyr::filter(!is.na(ws) & !is.na(wd))

LIDAR.winds.Aug2.3000m.barbs <- LIDAR.winds.Aug2.3000m %>% #selects narrower set of data for plotting
  dplyr::mutate(hour = format(datetime.CST, "%H"), minute = as.numeric(format(datetime.CST, "%M")), 
                # alt.round = round(altitude.masl/50)*50, dist.alt.round = alt.round - altitude.masl) %>% #Plot closest barb to every 100 m and beginning of each hour
                alt.round = round(altitude.masl/100)*100, dist.alt.round = alt.round - altitude.masl) %>% #Plot closest barb to every 100 m and beginning of each hour
  dplyr::group_by(hour, alt.round) %>%
  dplyr::filter(minute == min(minute) | abs(minute - 30) == min(abs(minute - 30))) %>%
  # dplyr::ungroup() %>%
  # dplyr::group_by(hour, alt.round) %>%
  dplyr::filter(abs(dist.alt.round) == min(abs(dist.alt.round))) %>%
  dplyr::ungroup() %>%
  dplyr::filter(altitude.masl >= 250 & (minute <=5 | (minute >= 25 & minute <= 35) | minute >= 55)) %>% #drop altitudes < 250 m - wind speeds are unrealistically low at the lowest two altitudes and only keep time points within 5 minutes of the top of the hour
  dplyr::mutate(datetime.CST = as.POSIXct(round(as.numeric(datetime.CST) / (30*60)) * 30*60, origin = "1970-01-01", tz = "Etc/GMT+6")) #%>%
  # dplyr::filter(!datetime.CST %in% c(as.POSIXct("2023-08-02 07:00:00", tz="Etc/GMT+6"), as.POSIXct("2023-08-02 14:00:00", tz="Etc/GMT+6"), 
  #                                    as.POSIXct("2023-08-02 18:00:00", tz="Etc/GMT+6"))) #drop times with wind sondes


#Add in ground winds
ground.winds <- read.csv("Chiwaukee_Summer_2023_1MinuteData.csv", header = TRUE)
ground.winds <- ground.winds %>%
  dplyr::mutate(datetime.CST = as.POSIXct(DateTime, tz="America/Denver"), Date = as.Date(datetime.CST, tz="America/Denver"), 
                WS.m.s = WS..mph.*0.44704) %>% #convert to metric units (deg C and m/s)
  dplyr::filter(Date == as.Date("2023-08-02")) %>%
  dplyr::select(12,14,10) %>%
  dplyr::rename(wd = WD..deg., ws = WS.m.s) %>%
  dplyr::mutate(wd = ifelse(wd == -999, NA, wd), ws = ifelse(ws == -999, NA, ws), hour = format(datetime.CST, "%H"), minute = as.numeric(format(datetime.CST, "%M")),
                altitude.masl = 120, alt.round = 120, ws.knots = ws * 1.94384, u.wind = -abs(ws)*sin(wd*pi/180)) #site is at 179 m, and the tower is 8.5 m tall - but plot as 100 m to separate from vertical winds

ground.winds.barbs <- ground.winds %>%
  dplyr::group_by(hour) %>%
  dplyr::filter(minute == 0 | minute == 30) %>%
  dplyr::ungroup()

# LIDAR.winds.Aug2.3000m <- bind_rows(LIDAR.winds.Aug2.3000m, ground.winds)                  


#Add in sonde winds
wind.sondes.3000m <- met.sondes %>%
  dplyr::mutate(datetime.UTC = as.POSIXct("2023-08-02 00:00:00", tz="UTC") + Seconds_UTC, datetime.CST = with_tz(datetime.UTC, tzone = "Etc/GMT+6"),
                datetime.CST = if_else(Seconds_UTC < 3250, datetime.CST + days(1), datetime.CST)) %>% #correct date to 8/2 - seconds were entered incorrectly
  dplyr::select(datetime.CST, Altitude_m_MSL, Speed_m_s, Heading_degrees) %>%
  dplyr::filter(Altitude_m_MSL <= 3000 & Speed_m_s != -99999) %>%
  dplyr::mutate(time = as.POSIXct(ifelse(datetime.CST < as.POSIXct("2023-08-02 09:00:00", tz="Etc/GMT+6"), "2023-08-02 07:15:00", 
                              ifelse(datetime.CST < as.POSIXct("2023-08-02 15:00:00", tz="Etc/GMT+6"), "2023-08-02 14:15:00", "2023-08-02 18:15:00")), tz="Etc/GMT+6"),
                # alt.round = round(Altitude_m_MSL/50) * 50, dist.alt.round = Altitude_m_MSL - alt.round) %>%
                alt.round = round(Altitude_m_MSL/100) * 100, dist.alt.round = Altitude_m_MSL - alt.round) %>%
  dplyr::mutate(wd = ifelse(Heading_degrees <= 180, Heading_degrees + 180, Heading_degrees - 180), ws.knots = Speed_m_s * 1.94384, u.wind = -abs(Speed_m_s)*sin(wd*pi/180)) %>%
  dplyr::select(5,2,3,8,9,10,6,7) %>%
  dplyr::rename(ws = Speed_m_s, altitude.masl = Altitude_m_MSL, datetime.CST = time)

wind.sondes.3000m.barbs <- wind.sondes.3000m %>%
  dplyr::group_by(datetime.CST, alt.round) %>%
  dplyr::filter(abs(dist.alt.round) == min(abs(dist.alt.round))) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(datetime.CST, alt.round) %>%
  dplyr::filter(dist.alt.round == max(dist.alt.round)) %>% #just select one altitude where there are two equidistant from the set altitude
  dplyr::ungroup()

# LIDAR.winds.Aug2.3000m <- bind_rows(LIDAR.winds.Aug2.3000m, ground.winds) #use to plot colors (no sonde winds)

LIDAR.winds.Aug2.3000m.barbs <- bind_rows(LIDAR.winds.Aug2.3000m.barbs, ground.winds.barbs, wind.sondes.3000m.barbs) #use to plot wind barbs
LIDAR.winds.Aug2.3000m.barbs <- LIDAR.winds.Aug2.3000m.barbs %>%
  dplyr::filter(datetime.CST > as.POSIXct("2023-08-02 05:50:00", tz="Etc/GMT+6") & datetime.CST < as.POSIXct("2023-08-02 20:10:00", tz="Etc/GMT+6"))


# --- Function to draw wind barbs: correct compass-to-plot angle + feather side (right/clockwise) ---

build_windbarbs_noaa_panel_epoch <- function(data, x, y, dir, spd,
                                             speed_units = c("ms", "knots"),
                                             xlim, ylim,
                                             len_y = 80,
                                             feather_len_y = 40,
                                             short_feather_y = 20,
                                             spacing_y = 15,
                                             barb_angle_deg = 60,
                                             tz_out = "Etc/GMT+6",
                                             plot_width = 7,
                                             plot_height = 1.5) {
  speed_units <- match.arg(speed_units)

  df <- data |>
    dplyr::transmute(.x = {{ x }}, .y = {{ y }}, .dir = {{ dir }}, .spd = {{ spd }}) |>
    tidyr::drop_na(.x, .y, .dir, .spd)

  if (!inherits(df$.x, "POSIXct")) df$.x <- as.POSIXct(df$.x, tz = tz_out)

  df <- df |>
    dplyr::mutate(
      spd_kn   = if (speed_units == "ms") .spd * 1.94384 else .spd,
      spd_kn   = round(spd_kn / 5) * 5,                   # NOAA rounding
      speed_ms = if (speed_units == "ms") .spd else .spd / 1.94384
    )

  xlim_sec <- as.numeric(xlim)
  xmin_sec <- xlim_sec[1]; xmax_sec <- xlim_sec[2]
  rx_sec   <- xmax_sec - xmin_sec
  ry_units <- diff(ylim)
  stopifnot(rx_sec > 0, ry_units > 0)
  sec_per_y <- (rx_sec / ry_units) * (plot_height / plot_width)

  to_time <- function(sec) as.POSIXct(sec, origin = "1970-01-01", tz = tz_out)
  to_sec  <- function(xpos) as.numeric(xpos)

  barb_angle <- barb_angle_deg * pi/180

  pieces <- purrr::pmap(df, function(.x, .y, .dir, .spd, spd_kn, speed_ms, ...) {
    x0s <- to_sec(.x)  # seconds since epoch
    y0  <- .y

    # Calm: circle
    if (spd_kn == 0) {
      return(list(
        calm   = tibble::tibble(x = .x, y = y0, r = 0.035 * len_y/50, speed_ms = speed_ms),
        shafts = tibble::tibble(), barbs = tibble::tibble()
      ))
    }

    # >>> KEY FIX: convert "degrees FROM north, clockwise" to math angle
    # so that 0° is up, 90° right, 180° down, 270° left.
    theta <- (90 - .dir) * pi/180

    # Shaft displacements (seconds in x, Y-units in y)
    dx_s <- len_y * cos(theta) * sec_per_y
    dy_y <- len_y * sin(theta)

    # 10/5 kt decomposition
    s   <- spd_kn
    n10 <- s %/% 10; s <- s - 10 * n10
    n5  <- s %/% 5

    shafts <- tibble::tibble(
      x0 = to_time(x0s),         y0 = y0,
      x1 = to_time(x0s + dx_s),  y1 = y0 + dy_y,
      speed_ms = speed_ms
    )

    # Feathers on the RIGHT side of the shaft (clockwise): phi = theta - barb_angle
    barb_seg <- function(dist_from_tip_y, feather_len_y_use) {
      frac <- (len_y - dist_from_tip_y) / len_y
      xb_s <- x0s + dx_s * frac
      yb   <- y0  + dy_y * frac

      phi  <- theta - barb_angle   # <<< RIGHT side
      fx_s <- feather_len_y_use * cos(phi) * sec_per_y
      fy   <- feather_len_y_use * sin(phi)

      tibble::tibble(
        x0 = to_time(xb_s),         y0 = yb,
        x1 = to_time(xb_s + fx_s),  y1 = yb + fy,
        speed_ms = speed_ms
      )
    }

    barbs <- tibble::tibble()
    pos <- 0
    if (n10 > 0) {
      for (k in seq_len(n10)) {
        barbs <- dplyr::bind_rows(barbs, barb_seg(pos, feather_len_y))
        pos <- pos + spacing_y
      }
    }

    if (n5 == 1) {
      # For a pure 5-kt wind, offset the half-barb from the tip
      if (n10 == 0) {
        barbs <- dplyr::bind_rows(
          barbs,
          barb_seg(spacing_y, short_feather_y)  # <- offset from tip
        )
      } else {
        # If there are 10-kt barbs already, keep using the accumulated pos
        barbs <- dplyr::bind_rows(
          barbs,
          barb_seg(pos, short_feather_y)
        )
      }
    }

    # barbs <- tibble::tibble()
    # pos <- 0
    # if (n10 > 0) for (k in seq_len(n10)) { barbs <- dplyr::bind_rows(barbs, barb_seg(pos, feather_len_y)); pos <- pos + spacing_y }
    # if (n5 == 1) barbs <- dplyr::bind_rows(barbs, barb_seg(pos, short_feather_y))

    list(calm = tibble::tibble(), shafts = shafts, barbs = barbs)
  })

  list(
    calm   = dplyr::bind_rows(purrr::map(pieces, "calm")),
    shafts = dplyr::bind_rows(purrr::map(pieces, "shafts")),
    barbs  = dplyr::bind_rows(purrr::map(pieces, "barbs"))
  )
}

#Apply the function to make the plot

xlim_use <- c(as.POSIXct("2023-08-02 05:45:00", tz="Etc/GMT+6"),
              as.POSIXct("2023-08-02 20:15:00", tz="Etc/GMT+6"))
ylim_use <- c(-50, 2000)

wb <- build_windbarbs_noaa_panel_epoch(
  data = LIDAR.winds.Aug2.3000m.barbs,
  x = datetime.CST,
  # y = altitude.masl,
  y = alt.round,
  dir = wd,
  spd = ws,
  speed_units = "ms",
  xlim = xlim_use,              # <- same limits as plot
  ylim = ylim_use,
  # len_y = 80, feather_len_y = 40, short_feather_y = 20, spacing_y = 15,
  len_y = 120, feather_len_y = 60, short_feather_y = 30, spacing_y = 20,
  tz_out = "Etc/GMT+6",
  plot_width = 7,
  plot_height = 1.5
)

    e <- ggplot() + 
      geom_tile(data = LIDAR.winds.Aug2.3000m, aes(x = datetime.CST, y = altitude.masl, fill = u.wind)) + 
      geom_point(data = ground.winds, aes(x = datetime.CST, y = altitude.masl, color = u.wind), shape = 0, size=0.75) +
      geom_point(data = wind.sondes.3000m, aes(x = datetime.CST, y = altitude.masl, color = u.wind), shape = 0) +
      geom_segment(data = wb$shafts, aes(x = x0, y = y0, xend = x1, yend = y1), linewidth = 0.25, lineend = "round") +
      geom_segment(data = wb$barbs, aes(x = x0, y = y0, xend = x1, yend = y1), linewidth = 0.25, lineend = "round") +
      geom_point(data = wb$calm, aes(x = x, y = y), shape = 21, stroke = 0.3, size = 1, fill = "white", colour = "black") +
      scale_color_distiller(palette = "PRGn", limits = c(-9,9), name = "u wind speed (m/s)") + 
      scale_fill_distiller(palette = "PRGn", limits = c(-9,9), name = "u wind speed (m/s)") +
      scale_x_datetime(limits = xlim_use, date_labels = "%H:%M",
                       breaks = seq(as.POSIXct("2023-08-02 06:00:00", tz="Etc/GMT+6"), as.POSIXct("2023-08-02 18:00:00", tz="Etc/GMT+6"), by="3 hours"),
                       minor_breaks = date_breaks("1 hour"),
                       expand = expansion(mult = 0), timezone = "Etc/GMT+6") +
      # scale_y_continuous(limits = ylim_use, expand = expansion(mult = 0), labels = function(x) sprintf("%4d", x)) +
      scale_y_continuous(limits = ylim_use, expand = expansion(mult = 0), labels = function(x) pad_fig(x, width = 4)) +
      labs(x = NULL, y = "Altitude (m ASL)") +
      theme(axis.title = element_blank(),
            axis.text = element_text(size=10),
            # axis.text.y = element_text(family = "mono", face = 2),
            panel.background = element_rect(fill = "white", color = "black"),
            panel.grid = element_line(color = "gray85"),
            legend.title.position = "right",
            legend.title = element_text(size = 9, angle = 90, hjust = 0.5),
            legend.text = element_text(size = 8),
            legend.direction = "vertical")#,
    # ggsave("Chiwaukee windpro lidar plot - Aug2-u winds.png", width = 6.75, height = 2.5)
    # ggsave("Chiwaukee windpro lidar plot - Aug2-u winds-2000.png", width = 7, height = 1.5)

#Old plot with colored wind barbs
    # e <- ggplot() +
    #     geom_segment(data = wb$shafts,
    #                  aes(x = x0, y = y0, xend = x1, yend = y1, color = speed_ms),
    #                  linewidth = 0.25, lineend = "round") +
    #     geom_segment(data = wb$barbs,
    #                  aes(x = x0, y = y0, xend = x1, yend = y1, color = speed_ms),
    #                  linewidth = 0.25, lineend = "round") +
    #     geom_point(data = wb$calm,
    #                aes(x = x, y = y),
    #                shape = 21, stroke = 0.3, size = 1,
    #                fill = "white", colour = "blue3") +
    #     scale_color_gradientn(colours = matlab.like(256), limits = c(0, 20),
    #                           name = "Wind speed (m/s)") +
    #     scale_x_datetime(limits = xlim_use, date_labels = "%H:%M",
    #                      breaks = seq(as.POSIXct("2023-08-02 00:00:00", tz="Etc/GMT+6"), as.POSIXct("2023-08-02 21:00:00", tz="Etc/GMT+6"), by="3 hours"),
    #                      minor_breaks = date_breaks("1 hour"),
    #                      expand = expansion(mult = 0), timezone = "Etc/GMT+6") +
    #     scale_y_continuous(limits = ylim_use,
    #                        expand = expansion(mult = 0)) +
    #     labs(x = NULL, y = "Altitude (m ASL)") +
    #     # theme_minimal(base_size = 12) +
    #     # theme(panel.grid.minor = element_blank())
    #     theme(axis.title = element_blank(),
    #           axis.text = element_text(size=8),
    #           panel.background = element_rect(fill = "white", color = "black"),
    #           panel.grid = element_line(color = "gray85"),
    #           legend.title.position = "right",
    #           legend.title = element_text(size = 9, angle = 90, hjust = 0.5),
    #           legend.text = element_text(size = 8),
    #           # legend.position = c(0.8, 0.98),
    #           # legend.justification = c("left", "top"),
    #           legend.direction = "vertical")#,
    #           # legend.background = element_rect(fill = scales::alpha("white", 0.8),
    #           #                                  color = "grey70"))
    #           # # legend.key.height = unit(0.35, "lines"),
    #           # legend.key.width  = unit(0.7, "lines"))
    #   # ggsave("Chiwaukee windpro lidar plot - Aug2.png", width = 5, height = 3.5)
    # # ggsave("Chiwaukee windpro lidar plot - Aug2-wide.png", width = 6.75, height = 2.5)
    
    
## Plot turbulence (variance in vertical velocity - w') ------------------------------------------------

turbulence.data.Aug2 <- nc_open("vertical_velocty_variance_20230802.nc")

HALO.turbulence.Aug2 <- ncvar_get(turbulence.data.Aug2, "var")
HALO.intensity.Aug2 <- ncvar_get(turbulence.data.Aug2, "intensity")
HALO.time.Aug2 <- ncvar_get(turbulence.data.Aug2, "time")
HALO.altitude.Aug2 <- ncvar_get(turbulence.data.Aug2, "height")

HALO.turbulence.Aug2 <- as.data.frame(HALO.turbulence.Aug2)
HALO.turbulence.Aug2 <- HALO.turbulence.Aug2 %>%
  dplyr::mutate(time = HALO.time.Aug2) %>%
  pivot_longer(1:400, names_to = "height.index", values_to = "w.variance")

HALO.intensity.Aug2 <- as.data.frame(HALO.intensity.Aug2)
HALO.intensity.Aug2 <- HALO.intensity.Aug2 %>%
  dplyr::mutate(time = HALO.time.Aug2) %>%
  pivot_longer(1:400, names_to = "height.index", values_to = "intensity")

HALO.altitude.Aug2 <- HALO.altitude.Aug2 %>%
  as.data.frame() %>%
  dplyr::mutate(height.index = paste0("V",seq(1,400,by=1)))
colnames(HALO.altitude.Aug2) <- c("Altitude.m", "height.index")

HALO.turbulence.Aug2 <- HALO.turbulence.Aug2 %>%
  full_join(., HALO.altitude.Aug2, by = "height.index") %>%
  full_join(., HALO.intensity.Aug2, by=c("time", "height.index"))
HALO.turbulence.Aug2 <- HALO.turbulence.Aug2 %>%
  dplyr::mutate(datetime.UTC = as.POSIXct("2023-08-02 00:00:00", tz="UTC") + time*60*60, 
                datetime.CST = with_tz(datetime.UTC, tzone = "Etc/GMT+6"))

HALO.turbulence.Aug2.2000m <- HALO.turbulence.Aug2 %>%
  dplyr::mutate(Date = as.Date(datetime.CST, tz = "Etc/GMT+6")) %>%
  dplyr::filter(Date == as.Date("2023-08-02", tz = "Etc/GMT+6")) %>%
  dplyr::mutate(w.variance = ifelse(is.nan(w.variance), NA, w.variance), 
                altitude.masl = Altitude.m + 182) %>% #assume the lidar is at 182 m - altitude of Chiwaukee trailer + 3 m because on the roof 
  dplyr::select(7,9,3,5) %>%
  dplyr::filter(altitude.masl < 2000 & intensity >= 1.005) %>% #apply threshold ??? for intensity??? 
  dplyr::mutate(log.w.variance = log10(w.variance)) %>% #calculate log10 of w' variance (per Tim Wagner's suggestion in an email 1/15/26)
  dplyr::filter(!is.infinite(log.w.variance))

#fit to a regular 1-minute grid and interpolate gaps (every 5 min it was measuring horizontal winds so has gaps)
HALO.turbulence.gridded <- HALO.turbulence.Aug2.2000m %>%
  mutate(datetime.CST.1min = floor_date(datetime.CST, unit = "1 minute")) %>%
  group_by(altitude.masl, datetime.CST.1min) %>%
  summarise(log.w.variance = mean(log.w.variance, na.rm = TRUE), .groups = "drop") %>%
  group_by(altitude.masl) %>%
  arrange(datetime.CST.1min) %>%
  complete(datetime.CST.1min = seq(min(datetime.CST.1min), max(datetime.CST.1min), by = "1 min")) %>%
  # mutate(log.w.variance = na.approx(log.w.variance, x = as.numeric(datetime.CST.1min), na.rm = FALSE)) %>%
  group_modify(~{ #
    g <- .x
    # Identify NA runs
    rle_na <- rle(is.na(g$log.w.variance))
    run_id <- rep(seq_along(rle_na$lengths), rle_na$lengths)
    run_is_na <- rle_na$values[run_id]
    run_len <- rle_na$lengths[run_id]
    
    # Convert threshold into number of bins
    step_secs <- as.numeric(difftime(g$datetime.CST.1min[2], g$datetime.CST.1min[1], units = "secs"))
    max_bins <- floor(as.numeric(dminutes(1), units = "secs") / step_secs)
    
    # Interpolate everywhere, then keep only “short” NA runs interpolated
    interp <- zoo::na.approx(g$log.w.variance, x = as.numeric(g$datetime.CST.1min), na.rm = FALSE)
    
    short_na <- run_is_na & (run_len <= max_bins)
    
    g$log.w.variance <- ifelse(short_na, interp, g$log.w.variance)
    g
  }) %>%
  ungroup() %>%
  dplyr::filter(!is.na(log.w.variance))


    w.var <- ggplot() + 
      geom_tile(data = HALO.turbulence.gridded, aes(x=datetime.CST.1min, y=altitude.masl, fill=log.w.variance)) +
      # scale_fill_viridis_c(option = "D", limits = c(0,3.01E-4), oob = scales::squish, name = "Aerosol", direction = -1) + 
      # scale_fill_distiller(palette = "YlGnBu", limits = c(0,8e-4), direction = 1) +
      # scale_fill_viridis_c(option = "turbo", limits = c(-3,1), oob = scales::oob_squish) +
      scale_fill_viridis_c(option = "viridis", limits = c(-3,1), oob = scales::oob_squish) +
      # scale_fill_gradientn(colors = matlab.like(54), limits = c(-3,1), oob = scales::oob_squish) +
      # scale_color_viridis_c(option = "D", limits = c(7,60), direction = -1) +
      scale_x_datetime(limits = c(as.POSIXct("2023-08-02 05:45:00", tz="America/Denver"), as.POSIXct("2023-08-02 20:15:00", tz="America/Denver")), 
                       minor_breaks = date_breaks("1 hour"), expand = expansion(mult = 0)) +
      # scale_y_continuous(limits = c(0,3000), expand = expansion(mult = 0)) +
      scale_y_continuous(limits = c(0,2000), expand = expansion(mult = 0), labels = function(x) pad_fig(x, width = 4)) +
      labs(fill = "log<sub>10</sub>w' variance") +
      xlab(NULL) + ylab(NULL) + #ylab("Altitude (m ASL)") +
      # guides(color = "none") + #don't show legend for color
      # guides(
      #   fill  = guide_colorbar(barheight = unit(40, "pt"), title.position = "right",
      #     title.theme = element_markdown(size = 9, angle = 90, hjust = 0.5), label.theme = element_text(size = 8)),
      #   color = guide_colorbar(barheight = unit(40, "pt"), title.position = "right",
      #     title.theme = element_markdown(size = 9, angle = 90, hjust = 0.5), label.theme    = element_text(size = 8))) +
      theme(
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "gray85"),
        axis.text = element_text(size = 10),
        # axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.title = element_markdown(size = 9, angle = 90, hjust = 0.5),
        # legend.title = element_text(size = 9, angle = 90, hjust = 0.5),
        legend.text = element_text(size = 8),
        legend.direction = "vertical",
        legend.box = "horizontal",
        legend.title.position = "right"
      )
    # ggsave("Chiwaukee variability in vertical velocity - Aug2.png", width = 6.75, height = 4)


    
    
    
    
# Combine UAS plots into one plot ----------------------------------------------------------------------
    
    #Drop x-axis from PM2.5 plot
    d_no_axis <- d + theme(axis.text.x = element_blank())
    # c_no_axis <- c + theme(axis.text.x = element_blank())
    
    Chiwaukee_UAS_plot <- plot_grid(d_no_axis, c, ncol = 1, rel_heights = c(1.05,2), align = "v", axis = "l")
    # Chiwaukee_UAS_plot <- plot_grid(c_no_axis, d, ncol = 1, rel_heights = c(2,1.1), align = "v", axis = "l")

    #Add panel labels
    Chiwaukee_UAS_plot <- ggdraw(Chiwaukee_UAS_plot) +
      draw_plot_label(
        label = c("(a)","(b)","(c)"),
        x = c(0.08, 0.08, 0.08),
        y = c(0.96, 0.62, 0.31),
        hjust = 0, vjust = 1, size = 12
      )

    #Define shared axes
    y.axis <- grid::textGrob("Altitude (m ASL)", gp = gpar(fontsize=14), rot = 90)

    Chiwaukee_UAS_plot <- gridExtra::grid.arrange(Chiwaukee_UAS_plot, left = y.axis)

    #add x-axis centered on plot
    Chiwaukee_UAS_plot <- ggdraw() + draw_grob(rectGrob(gp = gpar(fill = "white", col = NA)), x = 0, y = 0, width = 1, height = 1) +
      draw_plot(Chiwaukee_UAS_plot, x = 0, y = 0.03, width = 1, height = 0.97) +
      draw_text("Time (CST)", x = 0.4, y = 0.01, size = 14, vjust = 0)

    # ggsave("Chiwaukee UAS plot - O3-PM25.png", Chiwaukee_UAS_plot, width = 7, height = 5)
    ggsave("Chiwaukee UAS plot - O3-PM25-colorblind.png", Chiwaukee_UAS_plot, width = 7, height = 5)

    
#Combine lidar panels into one figure ---------------------------------------------------
#top row = winds (plot e); 2nd row = variability in w' (plot w.var); 3rd row = aerosol lidar (plot b); 4th ozone lidar (plot a)

    #Drop x-axes from the first three panels
    e.no.axis <- e + theme(axis.text.x = element_blank())
    w.var.no.axis <- w.var + theme(axis.text.x = element_blank())
    b.no.axis <- b + theme(axis.text.x = element_blank())
    
    #Add white space to right side of panels where needed (to match axes - row 3 PM2.5 is widest)
    
    pad_white <- ggdraw() + theme(plot.background = element_rect(fill = "white", color = NA),
                                  panel.background = element_rect(fill = "white", color = NA))
    
    row.1 <- plot_grid(e.no.axis, pad_white, nrow = 1, rel_widths = c(6, 1.3)) #allows space on the right side
    row.2 <- plot_grid(w.var.no.axis, pad_white, nrow = 1, rel_widths = c(6, 1.24))
    row.3 <- plot_grid(b.no.axis)
    row.4 <- plot_grid(a, pad_white, nrow = 1, rel_widths = c(6, 1.22))

    #Combine plots
    Chiwaukee_lidar_plot <- plot_grid(row.1, row.2, row.3, row.4,
      ncol = 1, rel_heights = c(1,1,1,1.08), align = "v", axis = "l")

    #Add panel labels
    Chiwaukee_lidar_plot <- ggdraw(Chiwaukee_lidar_plot) +
      draw_plot_label(
        label = c("(a)","(b)","(c)","(d)"),
        x = c(0.07, 0.07, 0.07, 0.07),
        y = c(0.98, 0.735, 0.49, 0.24),
        hjust = 0, vjust = 1, size = 12
      )

    #Define shared axes
    y.axis <- grid::textGrob("Altitude (m ASL)", gp = gpar(fontsize=14), rot = 90)
    # x.axis <- grid::textGrob("Time (CST)", gp = gpar(fontsize=14))

    # Chiwaukee_vertical_plot <- gridExtra::grid.arrange(Chiwaukee_vertical_plot, left = y.axis, bottom = x.axis)
    Chiwaukee_lidar_plot <- gridExtra::grid.arrange(Chiwaukee_lidar_plot, left = y.axis)

    #add x-axis centered on plot
    Chiwaukee_lidar_plot <- ggdraw() + draw_grob(rectGrob(gp = gpar(fill = "white", col = NA)), x = 0, y = 0, width = 1, height = 1) +
      draw_plot(Chiwaukee_lidar_plot, x = 0, y = 0.03, width = 1, height = 0.97) +
      draw_text("Time (CST)", x = 0.4, y = 0.01, size = 14, vjust = 0)

    ggsave("Chiwaukee vertical LIDAR wind-ozone-PM-aerosol plot.png", Chiwaukee_lidar_plot, width = 7, height = 8)
    # ggsave("Chiwaukee vertical LIDAR wind-ozone-PM-aerosol plot-colorblind.png", Chiwaukee_lidar_plot, width = 7, height = 8)
    
    
    
# #Combine panels into one figure - OLD COMBINED PLOT ---------------------------------------------------
# #top row = winds (plot e); 2nd row = ozone lidar (plot a); 3rd row = ozone UAS (plot c); 4th row = aerosol lidar (plot b); & bottom row = PM UAS (plot d)
# 
#     #Extract legend from plot b then drop plot b legend & x-axis labels
#     aerosol.legend <- get_legend(b + theme(legend.position = "right"))
#     # aerosol.legend.plot <- ggdraw(aerosol.legend) + 
#     #   theme(plot.background = element_rect(fill = "white", color = NA),
#     #         panel.background = element_rect(fill = "white", color = NA))
#     
#     b_noleg <- b + theme(legend.position = "none", axis.text.x = element_blank())
#     
#     #Extract legend from plot c then drop plot c legend
#     ozone.legend <- get_legend(c + theme(legend.position = "right"))
# 
#     c_noleg <- c + theme(legend.position = "none", axis.text.x = element_blank())
#     
#     #Extract legend from plot e then drop plot e legend
#     wind.legend <- get_legend(e + theme(legend.position = "right"))
# 
#     e_noleg <- e + theme(legend.position = "none", axis.text.x = element_blank())
#     
#     a <- a + theme(axis.text.x = element_blank()) #drop x-axis labels
#     
#     
#     row.1 <- plot_grid(e_noleg, nrow=1, rel_widths = 7)
#     row.2.3 <- plot_grid(a, c_noleg, ncol=1, rel_widths = 7, rel_heights = c(1,1)) #combine ozone plots
#     row.4.5 <- plot_grid(b_noleg, d, ncol=1, rel_widths = 7, rel_heights = c(1,0.65)) #combine aerosol/PM plots
#     
# 
#     # add legends (with white background)
#     row1.legend <- ggdraw() + draw_grob(wind.legend, x=-0.3, y=0, width = 1, height = 1) + #Put legend in a white box
#       theme(plot.background = element_rect(fill = "white", color = NA),
#             panel.background = element_rect(fill = "white", color = NA))
#     row.1.all <- plot_grid(row.1, row1.legend, nrow = 1, rel_widths = c(6, 2.5))
#     # bottom.wide <- plot_grid(bottom.row, pad_white, nrow = 1, rel_widths = c(5.5, 1.55)) #allows space on the right side
#     
#     row23.legend <- ggdraw() + draw_grob(ozone.legend, x=-0.28, y=0, width = 1, height = 1) + #Put legend in a white box
#       theme(plot.background = element_rect(fill = "white", color = NA),
#             panel.background = element_rect(fill = "white", color = NA))
#     row.23.all <- plot_grid(row.2.3, row23.legend, nrow = 1, rel_widths = c(6, 2.5))
#     
#     row45.legend <- ggdraw() + draw_grob(aerosol.legend, x=0, y=0, width = 1, height = 1) + #Put legend in a white box
#       theme(plot.background = element_rect(fill = "white", color = NA),
#             panel.background = element_rect(fill = "white", color = NA))
#     row.45.all <- plot_grid(row.4.5, row45.legend, nrow = 1, rel_widths = c(6, 2.5))
#     
#     
#     
#     Chiwaukee_vertical_plot <- plot_grid(
#       row.1.all,
#       row.23.all,
#       row.45.all,
#       ncol = 1,
#       rel_heights = c(1,2,1.6),
#       align = "v",
#       axis = "l"
#     )
#     
#     #Add panel labels
#     Chiwaukee_vertical_plot <- ggdraw(Chiwaukee_vertical_plot) +
#       draw_plot_label(
#         label = c("(a)","(b)","(c)","(d)","(e)","(f)"),
#         x = c(0.07, 0.07, 0.07, 0.07, 0.07, 0.07),
#         y = c(0.98, 0.76, 0.54, 0.44, 0.325, 0.115),
#         hjust = 0, vjust = 1, size = 12
#       )
#     
#     #Define shared axes
#     y.axis <- grid::textGrob("Altitude (m ASL)", gp = gpar(fontsize=14), rot = 90)
#     # x.axis <- grid::textGrob("Time (CST)", gp = gpar(fontsize=14))
#     
#     # Chiwaukee_vertical_plot <- gridExtra::grid.arrange(Chiwaukee_vertical_plot, left = y.axis, bottom = x.axis)
#     Chiwaukee_vertical_plot <- gridExtra::grid.arrange(Chiwaukee_vertical_plot, left = y.axis)
#     
#     #add x-axis centered on plot
#     Chiwaukee_vertical_plot <- ggdraw() + draw_grob(rectGrob(gp = gpar(fill = "white", col = NA)), x = 0, y = 0, width = 1, height = 1) +
#       draw_plot(Chiwaukee_vertical_plot, x = 0, y = 0.03, width = 1, height = 0.97) + 
#       draw_text("Time (CST)", x = 0.4, y = 0.01, size = 14, vjust = 0)
#     
#     # ggsave("Chiwaukee vertical wind-ozone-PM-aerosol plot-aligned.png", Chiwaukee_vertical_plot, width = 7, height = 8)
#     ggsave("Chiwaukee vertical wind-ozone-PM-aerosol plot-aligned-spectral.png", Chiwaukee_vertical_plot, width = 7, height = 8)
    
    
    
    
#Make map of ozonesonde tracks --------------------------------------------------
require(sf)
require(ggspatial)
    
O3.sonde.1 <- dplyr::mutate(O3.sonde.1, sonde = "1")
O3.sonde.2 <- dplyr::mutate(O3.sonde.2, sonde = "2")
O3.sonde.3 <- dplyr::mutate(O3.sonde.3, sonde = "3")

O3.sonde.labeled <- bind_rows(O3.sonde.1, O3.sonde.2, O3.sonde.3)

O3.sonde.tracks <- O3.sonde.labeled %>%
  dplyr::mutate(datetime.UTC = as.POSIXct("2023-08-02 00:00:00", tz="UTC") + Seconds_UTC, datetime.CST = as.POSIXct(datetime.UTC, tz="America/Denver"), 
                altitude.m = Altitude_km*1000) %>%
  dplyr::select(datetime.CST, sonde, altitude.m, Latitude_deg, Longitude_deg, Ozone_ppmv) %>%
  dplyr::filter(altitude.m <= 2000 & altitude.m > 0 & Latitude_deg != -99999 & Ozone_ppmv != -99999) %>%
  dplyr::rename(Ozone = Ozone_ppmv)

O3.sonde.tracks.plot <- st_as_sf(O3.sonde.tracks, coords = c("Longitude_deg", "Latitude_deg"), crs = 4326, remove = FALSE)
O3.sonde.tracks.transf <- st_transform(O3.sonde.tracks.plot, 3857)

#Define labels
O3.sonde.labels <- data.frame(lon = c(-87.796,-87.796,-87.807), lat = c(42.519,42.513,42.5175), label = c("am","noon","pm"))
O3.sonde.labels.plot <- st_as_sf(O3.sonde.labels, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
O3.sonde.labels.transf <- st_transform(O3.sonde.labels.plot, 3857) %>%
  mutate(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2]) %>%
  st_drop_geometry()

    a <- ggplot() + annotation_map_tile(type = "cartolight", zoomin = 0) +
      geom_sf(data = O3.sonde.tracks.transf, size = 2, shape = 21, stroke = 0, aes(fill = Ozone)) +
      geom_text(data = O3.sonde.labels.transf, aes(x = x, y = y, label = label), size = 4) +
      # scale_fill_gradientn(colors = matlab.like(87), limits = c(27,113), name = "Ozone") +
      scale_fill_viridis_c(option = "C", limits = c(27,113), direction = -1) +
      theme(axis.text = element_blank(),
            axis.title = element_blank())
    # ggsave("Ozone sonde tracks - Aug2.png", width = 4, height = 4)
    # ggsave("Ozone sonde tracks - Aug2-colorblind friendly.png", width = 4, height = 4)
    
    
  
 
    
# #OLD VERSION - top row = winds (plot e); middle row = ozone lidar (left, plot a) & UAS (right, plot c); bottom row = aerosol lidar (left, plot b) & UAS (right, plot d)
# 
# #Extract legend from plot b then drop plot b legend
# aerosol.legend <- get_legend(b + theme(legend.position = "right"))
# # aerosol.legend.plot <- ggdraw(aerosol.legend) + 
# #   theme(plot.background = element_rect(fill = "white", color = NA),
# #         panel.background = element_rect(fill = "white", color = NA))
# 
# b_noleg <- b + theme(legend.position = "none")
# 
# top.row <- plot_grid(e, nrow=1, rel_widths = 7)
# middle.row <- plot_grid(a, c, nrow=1, rel_widths = c(2.7, 4.3))
# bottom.row <- plot_grid(b_noleg, d, nrow=1, rel_widths = c(2.7, 1.8))
# 
# # # a white placeholder for the bottom right of the plot
# # pad_white <- ggdraw() + theme(plot.background = element_rect(fill = "white", color = NA),
# #                               panel.background = element_rect(fill = "white", color = NA))
# # bottom.right <- plot_grid(aerosol.legend.plot, nrow = 1, rel_widths = 1)
# bottom.right <- ggdraw() + draw_grob(aerosol.legend, x=-0.1, y=0, width = 1, height = 1) + #Put legend in a white box
#   theme(plot.background = element_rect(fill = "white", color = NA),
#         panel.background = element_rect(fill = "white", color = NA))
# bottom.all <- plot_grid(bottom.row, bottom.right, nrow = 1, rel_widths = c(4.5, 2.5))
# # bottom.wide <- plot_grid(bottom.row, pad_white, nrow = 1, rel_widths = c(5.5, 1.55)) #allows space on the right side
# 
# Chiwaukee_vertical_plot <- plot_grid(
#   top.row,
#   middle.row,
#   bottom.all,
#   ncol = 1,
#   rel_heights = c(1,1,1)
# )
# 
# #Add panel labels
# Chiwaukee_vertical_plot <- ggdraw(Chiwaukee_vertical_plot) +
#   draw_plot_label(
#     label = c("(a)","(b)","(c)","(d)","(e)","(f)"),
#     x = c(0.065, 0.065, 0.43, 0.64, 0.065, 0.43),
#     y = c(0.98, 0.65, 0.65, 0.65, 0.32, 0.32),
#     hjust = 0, vjust = 1, size = 12
#   )
# 
# #Define shared axes
# y.axis <- grid::textGrob("Altitude (m ASL)", gp = gpar(fontsize=14), rot = 90)
# x.axis <- grid::textGrob("Time (CST)", gp = gpar(fontsize=14))
# 
# Chiwaukee_vertical_plot <- gridExtra::grid.arrange(Chiwaukee_vertical_plot, left = y.axis, bottom = x.axis)
# 
# ggsave("Chiwaukee vertical wind-ozone-PM-aerosol plot.png", Chiwaukee_vertical_plot, width = 7, height = 8)
    




# ## Build a color map for ws in m/s
# pal     <- matlab.like(256)
# col_fun <- col_numeric(palette = pal, domain = range(LIDAR.winds.Aug2.1500m$ws, na.rm = TRUE))
# LIDAR.winds.Aug2.1500m$col  <- col_fun(LIDAR.winds.Aug2.1500m$ws)
# 
# ## --- Set up an empty time × altitude plot
#     plot(range(LIDAR.winds.Aug2.1500m$datetime.CST), range(c(0,3000)), type = "n", xlab = NULL, ylab = "Altitude (m ASL)", xaxt = "n")
#     
#     axis.POSIXct(1, at = pretty(LIDAR.winds.Aug2.1500m$datetime.CST), format = "%H:%M")
#     
# ## --- Draw barbs in color
#     op <- par(no.readonly = TRUE)
#     for (i in seq_len(nrow(LIDAR.winds.Aug2.1500m))) {
#       par(fg = LIDAR.winds.Aug2.1500m$col[i])          # <- use foreground color
#       windbarbs(
#         cx = LIDAR.winds.Aug2.1500m$datetime.CST[i],
#         cy = LIDAR.winds.Aug2.1500m$altitude.masl[i],
#         direction = LIDAR.winds.Aug2.1500m$wd[i],
#         speed     = LIDAR.winds.Aug2.1500m$ws.knots[i],
#         cex = 0.4
#       )
#     }
#     par(op)
#     
# # Legend (fpr m/s)
#     brks <- pretty(range(LIDAR.winds.Aug2.1500m$ws, na.rm = TRUE), n = 5)
#     legend("topright", title = "Wind speed (m/s)",
#            legend = format(brks), fill = col_fun(brks), bty = "n", cex = 0.9)
    
    

