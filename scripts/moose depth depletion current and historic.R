library(lubridate)
library(here)
library(readxl)
library(weathercan)
library(kiwisR)
library(tidyhydat)
library(conflicted)
library(tidyverse)
library(zoo)
library(rforecastca)

#remotes::install_github("rwhale/rforecastca")

conflict_prefer("filter", "dplyr")
conflict_prefer("here", "here")

#unload package to prevent continuous warnings
#unloadNamespace("conflicted")

####----Preprocess breakup date data-----####

#data import and formatting:
b_date <- read_excel(here::here("import", "breakup_date_ken.xlsx"), 
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
m_ua_day <- read.csv(here::here("import", "moose_dly_clim.csv"))
#cast date column as date
m_ua_day$date <- as.Date(m_ua_day$date)

##download NRT Moosonee CLIM data from Wiski using kiwisR
#get and store station information for Moosonee
sta <- ki_station_list(hub = 'swmc', search_term = "*Moosonee RCS*")

#get and store time-series information for Moosonee
ts <- ki_timeseries_list(hub = 'swmc', station_id = sta$station_id)

#select day total precip and mean daily air temp  and snow depth time-series
ts.f <- filter(ts, ts_name == "SDepth.6hr.P")

#download data from wiski beginning in Oct and store as an object
clim_ls <- ki_timeseries_values(hub = 'swmc', 
                                ts_id = ts.f$ts_id, 
                                start_date = "2018-03-01",
                                end_date = "2018-05-01")
                                #end_date = Sys.Date())

#calculate daily cumulative sd depletion
clim_sd <- clim_ls %>%
  mutate(Timestamp = as.Date(Timestamp, format = "%Y-%m-%d")) %>% 
  group_by(Timestamp) %>% 
  summarise(SDepth = mean(SDepth, na.rm = T)) %>%
  mutate(depl = diff(c(SDepth[1],SDepth)),
         depl_neg = if_else(depl < 0, abs(depl), 0),
         cum_depl = cumsum(depl_neg)) %>% 
  na.omit() 

#ggplot(clim_sd, aes(x = Timestamp, y = cum_depl)) + geom_line()

msc_sdepl <- m_ua_day %>%
  select(date, year, month, day, snow_grnd) %>%
  filter(month >= 3 & month < 6 & year >= 1955) %>% 
  filter(!(year %in% c(1993:1996, 1998))) %>% 
  group_by(year) %>% 
  mutate(depl = diff(c(snow_grnd[1],snow_grnd))) %>% 
  na.omit() %>% 
  mutate(depl_neg = if_else(depl < 0, abs(depl), as.integer(0)),
         cum_depl = cumsum(depl_neg)) %>% 
  select(date, year, cum_depl)

#ggplot(msc_sdepl, aes(x = date, y = cum_depl)) + geom_line()


#join with depth depletion with bu severity

#join historic flow with breakup severity
depl_jn <- msc_sdepl  %>% 
  left_join(mse_bdate, by = "year") %>% 
  mutate(doy = yday(date)) %>% 
  filter(doy <= b_doy) 

#find breakup dates that align with max depletion
depl_date <- depl_jn %>% 
  group_by(year) %>% 
  filter(doy == b_doy)
  #slice(which.max(cum_depl))

#set facet labels
facet_labels <- c("severe" = "severe", "non.severe" = "non severe")

#plot sd depl
sdepl_plot <- ggplot() + 
  geom_line(data = clim_sd, aes(x = as.Date(yday(Timestamp), origin = "2018-01-01"), y = cum_depl)) +
  geom_line(data = depl_jn, aes(x = as.Date(yday(date), origin = "2018-01-01"), y = cum_depl, group = year, col = severity), size = 0.25, alpha = (0.25)) +
  geom_text(data = depl_date, aes(x = as.Date(yday(date), origin = "2018-01-01"), y = cum_depl, group = year, label = year), col = "black", size = 2, alpha = 0.75) +
  facet_grid(cols = vars(severity), labeller = as_labeller(facet_labels)) +
  scale_x_date(date_labels = "%b-%d", breaks = "2 weeks") +
  theme_bw() +
  labs(x = "", y = "cumulative snow depth depletion (cm)") +
  ggtitle("Moosonee UA Climate Station") +
  theme(strip.background = element_blank(), legend.position = "none")
#save figure to disk
#ggsave(filename = here::here("plots", "mse_sd_depl_bdate_severity.png"))

plot(sdepl_plot)
