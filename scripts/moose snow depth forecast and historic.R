# library(lubridate)
# #library(here)
# library(readxl)
# library(weathercan)
# library(kiwisR)
# library(tidyhydat)
# library(conflicted)
# library(tidyverse)
# library(zoo)
# library(rforecastca)

#remotes::install_github("rwhale/rforecastca")

conflict_prefer("filter", "dplyr")
#conflict_prefer("here", "here")

#unload package to prevent continuous warnings
#unloadNamespace("conflicted")

####----Preprocess breakup date data-----####

#set path
path <- "C:/Users/beatonan/OneDrive - Government of Ontario/Documents/My Documents/R/FN_Hydro_FY18_19/"

#data import and formatting:
b_date <- read_excel(paste0(path, "import", "breakup_date_ken.xlsx"), 
                     col_types = c("numeric", "date", "text", 
                                   "numeric"))
mse_bdate <- b_date %>%
  mutate(b_doy = yday(date),
         severity = if_else(severity == "1", "severe", "non.severe")) %>%
  filter(river == "moose") %>%
  select(year, severity, b_doy)

#convert severity to a factor
mse_bdate$severity <- as.factor(mse_bdate$severity)
#change factor order for plotting
mse_bdate$severity <- fct_relevel(mse_bdate$severity, "severe","non.severe")

#import Moosonee Data
#may want to define date as date when importing b/c it takes a bit of time to do the conversion
m_ua_day <- read.csv(paste0(path, "import", "moose_dly_clim.csv"))
#cast date column as date
m_ua_day$date <- as.Date(m_ua_day$date)

####----Gather Observed Data for Current Year----####
#NOTE data retrieved using weathercan is 3 weeks old so best to use kiwisR or 
#perhaps could get from data mart (if it is there) but would require a package or code to do so...

##download NRT Moosonee CLIM data from Wiski using kiwisR
#get and store station information for Moosonee
sta <- ki_station_list(hub = 'swmc', search_term = "*Moosonee RCS*")

#get and store time-series information for Moosonee
ts <- ki_timeseries_list(hub = 'swmc', station_id = sta$station_id)

#select day total precip and mean daily air temp  and snow depth time-series
ts.f <- filter(ts, 
               ts_name == "Precip.DayTotal" | 
                 ts_name == "TAir.DayMean" | 
                 ts_name == "SDepth.6hr.P")

#download data from wiski beginning in Oct and store as an object
clim_ls <- ki_timeseries_values(hub = 'swmc', 
                                ts_id = ts.f$ts_id, 
                                start_date = "2018-10-01", 
                                end_date = Sys.Date())

#extract snow depth info and add wy doy
clim_sd <- clim_ls[[3]] %>% 
  group_by(yday(Timestamp)) %>% 
  summarise(SDepth = mean(SDepth, na.rm = T)) %>% 
  mutate(wy_doy = seq(1:n()))

##--Historic fdd from weathercan--##

sd <- m_ua_day %>%
  select(date, month, snow_grnd) %>% 
  mutate(snow_year = if_else(month(date) > 9, year(date) + 1, year(date))) %>%
  group_by(snow_year) %>%
  mutate(doy = yday(date)) %>% 
  filter(snow_year >= 1955) %>% 
#filter out years with bad/no data
  filter(!(snow_year %in% c(1993, 1994, 1995, 1996)))

#join with Moose breakup severity and filter values less than breakup date
sd_jn <- sd %>% 
  rename(year = snow_year) %>% 
  left_join(mse_bdate, by = "year") %>% 
  #filter winter months
  filter(month >= 10 | month <= 5) %>% 
  mutate(wy_doy = seq(1:n())) %>% 
  #filter out data greater than breakup date but exclude data from subsequent snow season
  filter(!(doy >= b_doy & month <= 10)) %>% 
  na.omit %>% 
#filter out bad data that are 0 prior to melt (assume Mar 15)
  filter(!(snow_grnd < 5 & doy < 74))

#find breakup dates that align with max bdate
sd_date <- sd_jn %>% 
  #filter(doy == b_doy - 1)
  filter(snow_grnd == max(snow_grnd))

#set facet labels
facet_labels <- c("severe" = "severe", "non.severe" = "non severe")

#plot observed and historic sd
sd <- ggplot() + 
  geom_line(data = sd_jn, aes(x = as.Date(wy_doy, origin = "2018-10-01"), y = snow_grnd, group = year, col = severity), size = 0.25, alpha = (0.5)) +
  geom_text(data = sd_date, aes(x = as.Date(b_doy + 30, origin = "2018-10-01"), y = snow_grnd, group = year, label = year), col = "black", size = 1.5) +
  geom_line(data = clim_sd, aes(x = as.Date(wy_doy, origin = "2018-10-01"), y = SDepth)) +
  facet_grid(cols = vars(severity), labeller = as_labeller(facet_labels)) +
  scale_x_date(date_labels = "%b", breaks = "month") +
  theme_bw() +
  labs(x = "", y = "snow depth (cm)") +
  ggtitle("Moosonee UA Climate Station") +
  theme(strip.background = element_blank(), legend.position = "none")
#ggsave(filename = here::here("plots", "sd_bdate_severity_obs.png"))

plot(sd)
