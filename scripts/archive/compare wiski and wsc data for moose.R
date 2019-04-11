library(dplyr)
library(ggplot2)
library(lubridate)
library(kiwisR)

mse_wsc <- read.csv(here::here("import", "mse_crs_q.csv"))

mse_wsc <- mse_wsc %>%
  rename(date = Date) %>% 
  mutate(date = ymd(date)) %>% 
  filter(date > as.Date("2017-04-15"), date < as.Date("2017-04-24"))

#Get WISKI Flow Data For Moose River above Moose River Crossing
#download moose river WL from wiski using kiwisR
sta <- ki_station_list(hub = 'swmc', search_term = "Moose River above Moose River*")

#get and store time-series information for Moosonee
ts <- ki_timeseries_list(hub = 'swmc', station_id = sta$station_id)

#Get ts name for Q
ts.q <- filter(ts, ts_name == "Q.DayMean") 

ts_q_15 <- filter(ts, ts_name == "Q.15")

#retrieve Q from WISKI
mse_wiski_15 <- ki_timeseries_values(hub = 'swmc',
                              ts_id = ts_q_15$ts_id,
                              start_date = as.Date("2017-04-15"),
                              end_date = as.Date("2017-04-24"))

#retrieve Q from WISKI
mse_wiski <- ki_timeseries_values(hub = 'swmc',
                                     ts_id = ts.q$ts_id,
                                     start_date = as.Date("2017-04-15"),
                                     end_date = as.Date("2017-04-24"))

#plot daily flows
ggplot() + 
  geom_line(data = mse_wiski, aes(x = as.Date(Timestamp), y = Q), col = "red") +
  geom_line(data = mse_wsc, aes(x = date, y = Value)) +
  theme_bw()

#plot wiski hourly vs wsc daily
ggplot() + 
  geom_line(data = mse_wiski_15, aes(x = Timestamp, y = Q), col = "red") +
  geom_line(data = mse_wsc, aes(x = as.POSIXct(date), y = Value)) +
  theme_bw()


####----Gather WISKI Data for Current Year----####

#get and store station information for Moosonee
sta <- ki_station_list(hub = 'swmc', search_term = "*Moosonee*")

#get and store time-series information for Moosonee
ts <- ki_timeseries_list(hub = 'swmc', station_id = sta$station_id)

#select day total precip and mean daily air temp time-series
ts.f <- filter(ts, ts_name == "SDepth.6hr.P")

#download data from wiski beginning in Oct and store as an object
clim_ls <- ki_timeseries_values(hub = 'swmc',
                                ts_id = ts.f$ts_id,
                                start_date = "2018-10-01",
                                end_date = Sys.Date())

qw1 <- read.csv('http://dd.weather.gc.ca/hydrometric/csv/ON/daily/ON_02DD024_daily_hydrometric.csv',as.is=T)[,c(2,7)]

#DL daily data
m_ua_day <- weather_dl(station_ids = 4168,
                       start = "2018-10-01",
                       end = "2019-02-21",
                       interval = "day",
                       tz_disp = "EST")