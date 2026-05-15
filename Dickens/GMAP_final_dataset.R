#### Data cleaning

# Load necessary libraries
library(dplyr)
library(lubridate)

# Read the data from CSV files
geog.time <- read.csv("~/Desktop/GMAP/Code/AGES23_08_1_12_MA01.xlsx - AGES23_08_1_12_MA01.csv", header = TRUE)
ozone.data <- read.csv("~/Desktop/GMAP/AGES23_08_1_12_T400.csv", header = TRUE)
NO2.data <- read.csv("~/Desktop/GMAP/AGES23_08_1_12_T500u.csv", header = TRUE)

# Convert the Time column to POSIXct and format Date
geog.time <- geog.time %>%
  dplyr::mutate(Datetime = as.POSIXct(Time, format = "%m/%d/%Y %H:%M:%S", tz = "America/Chicago"),
                Date = format(as.POSIXct(Time, format = "%m/%d/%Y"), "%b %d"))

# Adjust Datetime by 30 seconds to center intervals on the 30-second mark
geog.time <- geog.time %>%
  dplyr::mutate(Adjusted_Datetime = Datetime + seconds(30))

# Create a new column for one-minute intervals based on adjusted datetime
geog.time <- geog.time %>%
  dplyr::mutate(Interval_Start = floor_date(Adjusted_Datetime, "minute"))

# Calculate u and v wind components
geog.time <- geog.time %>%
  dplyr::mutate(u.wind = -abs(AirMar.Wind.Speed..m.s.) * sin(AirMar.Wind.Direction..TRUE. * pi / 180),
                v.wind = -abs(AirMar.Wind.Speed..m.s.) * cos(AirMar.Wind.Direction..TRUE. * pi / 180))

# Dataframe with averaged wind direction and speed
lat.long.wind.avg <- geog.time %>%
  dplyr::group_by(Interval_Start) %>%
  dplyr::summarise(Date = first(Date),
                   Datetime = median(Datetime),
                   Latitude = median(GPS.Latitude), 
                   Longitude = median(GPS.Longitude), 
                   u.wind.avg = mean(u.wind, na.rm = TRUE),
                   v.wind.avg = mean(v.wind, na.rm = TRUE),
                   Wind_Speed = mean(AirMar.Wind.Speed..m.s., na.rm = TRUE)) %>%
  dplyr::mutate(Wind_Direction = atan2(u.wind.avg, v.wind.avg) * 180 / pi + 180) %>%
  dplyr::ungroup()

# Need to make compatible with NO2 and O3 data
lat.long.wind.avg <- lat.long.wind.avg %>%
  mutate(Datetime = round_date(Datetime, unit = "minute"))

# Process NO2 data
NO2.narrow <- NO2.data %>%
  dplyr::mutate(Datetime = as.POSIXct(Date...Time..Local., "%m/%d/%Y %H:%M", tz="America/Chicago")) %>%
  dplyr::select(Datetime, NO2.Conc)

# Process ozone data
Ozone.narrow <- ozone.data %>%
  dplyr::mutate(Datetime = as.POSIXct(Date...Time..Local., "%m/%d/%Y %H:%M", tz="America/Chicago")) %>%
  dplyr::select(Datetime, O3.Concentration) %>%
  dplyr::rename(O3.Conc = O3.Concentration)

# Combine data by matching on Datetime using lat.long.wind.avg
GMAP.rough.avg <- lat.long.wind.avg %>%
  left_join(NO2.narrow, by = "Datetime") %>%
  left_join(Ozone.narrow, by = "Datetime")

# Change the order of the columns
GMAP.final.avg <- GMAP.rough.avg %>%
  select(Date, Datetime, Latitude, Longitude, NO2.Conc, O3.Conc, Wind_Direction, Wind_Speed)

# Make AM and PM circuits for GMAP.final.avg
GMAP.final.avg <- GMAP.final.avg %>%
  dplyr::mutate(circuit = ifelse(Date == "Aug 01" & Datetime < as.POSIXct("2023-08-01 13:10:00", tz = "America/Chicago"), "AM", 
                                 ifelse(Date == "Aug 02" & Datetime < as.POSIXct("2023-08-02 12:20:00", tz = "America/Chicago"), "AM",
                                        ifelse(Date == "Aug 08" & Datetime < as.POSIXct("2023-08-08 13:20:00", tz = "America/Chicago"), "AM",
                                               ifelse(Date == "Aug 12" & Datetime < as.POSIXct("2023-08-12 10:50:00", tz = "America/Chicago"), "AM", "PM")))))
