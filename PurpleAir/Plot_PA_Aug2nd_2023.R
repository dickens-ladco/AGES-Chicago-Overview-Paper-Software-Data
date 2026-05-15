library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

# 1) Read PA sensor metadata
sensors <- read.csv("PA_sensors_forMap.csv")
sensors <- sensors %>% rename(sensor_index = Sensor_ID)
# --- Input CSV ---
df0 <- read.csv("../data/PA_Aug2023_10min.csv" )

df0 <- df0 %>%
  # Exclude sensors that had few observations on Aug 2nd
  filter(!sensor_index %in% c(124737, 124677,123003)) %>%
  mutate(
    timestamp_UTC     = as.POSIXct(time_stamp, tz = "UTC"),             # parse as UTC
    timestamp_Chicago = with_tz(timestamp_UTC, "America/Chicago")       # convert to Chicago local time
  )

df <- df0 %>%
filter(
  timestamp_Chicago >= ymd_hms("2023-08-02 00:00:00", tz = "America/Chicago"),
  timestamp_Chicago <  ymd_hms("2023-08-03 00:00:00", tz = "America/Chicago")
)

#First, calculate the mean of Channels A &B while ignoring NA. 
# if Channel A is NA, the average will be Channel B value
df$pm_ave <- rowMeans(df[,c("pm2.5_cf_1_a","pm2.5_cf_1_b")], na.rm = TRUE)
#Second, calculate the difference between A & B
df <- df %>% 
  mutate(
    pm2.5_cf_1_a = ifelse(is.na(pm2.5_cf_1_a), 0, pm2.5_cf_1_a),
    pm2.5_cf_1_b = ifelse(is.na(pm2.5_cf_1_b), 0, pm2.5_cf_1_b),
    pm_diff = ifelse(pm2.5_cf_1_a == 0 | pm2.5_cf_1_b == 0, 0, abs(pm2.5_cf_1_b - pm2.5_cf_1_a))
  )

df1 <- df[!is.na(df$pm_ave),]         #Remove NA values from pm_ave
df2 <- df1[!is.na(df1$humidity),]     #further remove NA values from humidity
#

df3 <- df2[(df2$humidity < 100),]     #Exclude RH values >=100%

air <- df3
# Calculate the average value of Channels A&B and add a new column to the dataframe
air <- air[(air$pm_diff < 5),]  #the absolute difference between Channel A and B should be < 4 ug/m3
## See Mousavi and Wu, 2021)
air$PM_Correct <- with(air,
                      ifelse(pm_ave <= 343,
                             0.52*pm_ave - 0.086*humidity + 5.75,
                             0.46*pm_ave + 0.000393 * pm_ave * pm_ave + 2.97))


# --- 1) 24-hour mean by sensor based on Chicago local time ---
daily_means <- air %>%
  mutate(date_local = as.Date(timestamp_Chicago, tz = "America/Chicago")) %>%
  group_by(sensor = sensor_index, date_local) %>%
  summarise(pm25_24h = mean(PM_Correct, na.rm = TRUE),
            n_obs = dplyr::n(),
            .groups = "drop") %>%
  arrange(sensor, date_local)

daily_stats <- air %>%
  mutate(date_local = as.Date(timestamp_Chicago,  tz = "America/Chicago")) %>%
  group_by(sensor_index, date_local) %>%
  summarise(
    pm25_mean   = mean(PM_Correct, na.rm = TRUE),
    pm25_median = median(PM_Correct, na.rm = TRUE),
    pm25_sdev     = sd(PM_Correct, na.rm = TRUE),
    pm25_min    = suppressWarnings(min(PM_Correct, na.rm = TRUE)),
    pm25_max    = suppressWarnings(max(PM_Correct, na.rm = TRUE)),
    pm25_75pctl    = quantile(PM_Correct, 0.75, na.rm = TRUE),
    n_obs       = dplyr::n(),
    .groups = "drop"
  ) %>%
  arrange(sensor_index, date_local)

# --- 3) Join stats with sensor metadata ---
output_data <- daily_stats %>%
  left_join(sensors, by = "sensor_index")

write.csv(output_data, "output.csv", row.names = FALSE)



# --- 2) Time-series plot of pm2.5_alt distinguishing sensors ---


sensor_order <- rev(c(124649, 175445, 124743, 124639, 175455,
                      175465, 175417, 124685, 124747, 151082))

# Okabe–Ito color-blind–safe palette (10 colors)
okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#D55E00",
               "#0072B2", "#F0E442", "#CC79A7", "#999999",
               "#000000", "#999933")

# Assign line types (repeat if necessary)
line_types <- c("solid", "dashed", "dotdash", "twodash", "longdash",
                "dotted", "solid", "dashed", "dotdash", "twodash")
line_types <- c("solid", "solid", "solid", "solid", "solid",
                "solid", "solid", "solid", "solid", "solid")
p <- ggplot(
  air %>%
    filter(!is.na(timestamp_Chicago), !is.na(PM_Correct)) %>%
    mutate(sensor_index = factor(sensor_index, levels = sensor_order)),
  aes(x = timestamp_Chicago,
      y = PM_Correct,
      color = sensor_index,
      linetype = sensor_index)
) +
  geom_line(linewidth = 0.9, alpha = 0.95) +
  labs(
    x = "Local Time",
    y = expression(paste(PurpleAir_PM[2.5], " (", mu, "g ", m^{-3}, ")")),
    color = "Sensor ID",
    linetype = "Sensor ID"
  ) +
  scale_x_datetime(
    date_breaks = "3 hours",
    date_labels = "%H:%M",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    limits = c(0, 20),
    expand = expansion(mult = c(0, 0.04))
  ) +
  scale_color_manual(
    values = okabe_ito,
    guide = guide_legend(ncol = 1, byrow = TRUE)
  ) +
  scale_linetype_manual(
    values = line_types,
    guide = guide_legend(ncol = 1, byrow = TRUE)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.3, colour = "grey80"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 11),
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    plot.margin = margin(10, 16, 10, 16),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.6)
  )

print(p)
ggsave("PurpleAir_linePlot.jpeg", p, width = 6, height = 4, units = "in", dpi = 600)

##box plot


library(dplyr)
library(ggplot2)

sensor_order <- rev(c(124649, 175445, 124743, 124639, 175455,
                      175465, 175417, 124685, 124747, 151082))

# Enforce order and keep only those sensors
air_plot <- air %>%
  filter(sensor_index %in% sensor_order) %>%
  mutate(sensor_index = factor(sensor_index, levels = sensor_order))

p2 <- ggplot(air_plot, aes(x = sensor_index, y = PM_Correct)) +
  geom_boxplot(
    fill = NA,                 # no fill
    color = "darkblue",        # outline & whiskers color
    outlier.shape = 16,
    outlier.size = 1.8,
    width = 0.7
  ) +
  # draw the median explicitly so its color/weight are guaranteed
  stat_summary(
    fun = median, geom = "crossbar", width = 0.7,
    fatten = 0, color = "darkblue", linewidth = 0.6
  ) +
  scale_y_continuous(limits = c(0, 20), expand = c(0, 0)) +
  labs(
    x = "PurpleAir Sensor ID",
    y = expression(paste(PM[2.5], " (", mu, "g ", m^{-3}, ")"))
  ) +
  theme_classic(base_size = 14) +
  theme(
    panel.grid = element_blank(),     # remove grid lines
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.6)
  )

ggsave("PurpleAir_BoxPlot.jpeg", p2, width = 4, height = 4, units = "in", dpi = 600)
