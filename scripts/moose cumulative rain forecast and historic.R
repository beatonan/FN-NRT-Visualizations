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

#set path
path <- "C:/Users/beatonan/OneDrive - Government of Ontario/Documents/My Documents/R/FN_Hydro_FY18_19"


#unload package to prevent continuous warnings
#unloadNamespace("conflicted")

####----Preprocess breakup date data-----####

#data import and formatting:
b_date <- read_excel(paste0(path, "import/", "breakup_date_ken.xlsx"), 
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
m_ua_day <- read.csv(paste0(path, "import/", "moose_dly_clim.csv"))
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

#select day total precip and mean daily air temp time-series
ts.f <- filter(ts, ts_name == "Precip.DayTotal" | ts_name == "TAir.DayMean")

#download data from wiski and store as an object
clim_ls <- ki_timeseries_values(hub = 'swmc', 
                                ts_id = ts.f$ts_id, 
                                start_date = Sys.Date() - 9, 
                                end_date = Sys.Date())

#change to following lines when running during breakup 
#start_date = "2019-03-01", 
#end_date = Sys.Date())

#combine precip and tair into 1 df 
clim <- clim_ls[[1]] %>% 
  left_join(clim_ls[[2]], by = "Timestamp") %>% 
  select(-Units.x, Units.y) %>% 
  #calculate cumulative rain
  mutate(
    rain = case_when(
      TAir >= 0 ~ Precip,
      TAir < 0 ~ 0),
    cum_rain = cumsum(rain),
    #add dummy doy
    dummy_doy = 60:69
  )

# ####----Gather AFFES Forecast---####
# 
# source(here::here("scripts", "shield_scraper.r"))
# 
# # Get AFFES forecast with Moosonee coordinates
# moose_affes <- shield_scraper(latitude = 51.2833, longitude = -80.6)
# 
# #NOTE: need to add observed cumulative rain to date and calculate rain vs snow from temp
# 
# #calculate forecast cumulative pcp
# moose_affes <- moose_affes %>% 
#   #create dummy doy for testing purposes starting on March 11th (doy 70)
#   mutate(rain = case_when(
#     TMP >= 0 ~ APCP,
#     TMP < 0 ~ 0),
#     cum_rain = max(clim$cum_rain) + cumsum(rain),
#     #dum_doy = c(70:74)
#     dum_doy = c(70:71))

####----Gather NAEFS Forecasts----####

#view available parameters output by the naefs model
#params <- rfca_naefs_parameters()

#gather 0 zulu forecast using rforecastca 
moose_naefs <- rfca_naefs_forecast(
  forecast_time = "00",
  forecast_location = "Moosonee",
  parameter = c("APCP-SFC", "TMP-SFC")
)

#calculate forecast percentiles from ensembles
naefs_q <- moose_naefs %>% 
  group_by(Parameter, Timestamp) %>% 
  summarise(q90 = quantile(Values, 0.90),
            q66 = quantile(Values, 0.66),
            q50 = quantile(Values, 0.50),
            q44 = quantile(Values, 0.44),
            q10 = quantile(Values, 0.10))

#convert from cumulative pcp to daily pcp
naefs_dly_pcp <- naefs_q %>%
  filter(Parameter != "TMP-SFC") %>% 
  arrange(Timestamp) %>% 
  mutate_at(vars(matches("q")), funs(c(.[1], diff(.)))) 

#combine daily pcp with daily mean temp
naefs <- naefs_q %>% 
  filter(Parameter == "TMP-SFC") %>%
  bind_rows(naefs_dly_pcp) %>% 
  #rearrange data so temp and pcp are unique columns
  gather(percentile, value, -Timestamp, -Parameter) %>% 
  spread(key = Parameter, value = value) %>%
  #rename columns to remove "-"
  rename("TMP" = "TMP-SFC", "PCP" = "APCP-SFC") %>% 
  mutate(
    rain = case_when(
      TMP >= 0 ~ PCP,
      TMP < 0 ~ 0)) %>%
  arrange(Timestamp) %>% 
  group_by(percentile) %>% 
  mutate(cum_rain = cumsum(rain)) %>% 
  select(Timestamp, percentile, cum_rain) %>% 
  spread(percentile, cum_rain) %>% 
  ungroup()

# #plot forecast cumulative rain  
# naefs %>% 
#   ggplot() +
#   geom_ribbon(aes(x = Timestamp, ymax = q90, ymin = q10), alpha = 0.25) +
#   geom_ribbon(aes(x = Timestamp, ymax = q66, ymin = q44), alpha = 0.50) +
#   geom_line(aes(x = Timestamp, y = q50), linetype = "dashed") + 
#   labs(title = "Forecast at Moosonee UA Climate Station", x = "", y = "cumulative rainfall (mm)") +
#   theme_bw()

####----Calculate Historic Rain Accumulation----####

#calculate accumulated rainfall
rain_acc <- m_ua_day %>% 
  ungroup() %>%
  select(date, total_rain) %>%
  mutate(month = month(date),
         year = year(date),
         doy = yday(date)) %>%
  filter(month >= 3 & month < 6 & year >= 1955) %>%
  group_by(year) %>%
  mutate(total_rain = replace(total_rain, is.na(total_rain),0)) %>% 
  mutate(cum_rain = cumsum(total_rain)) 

#join with Moose breakup severity and filter values less than breakup date
rain_acc_jn <- rain_acc %>% 
  left_join(mse_bdate, by = "year") %>% 
  filter(doy <= b_doy) %>% 
  filter(max(cum_rain) > 0) 

#find breakup dates that align with max depletion 
b_date_rain_acc <- rain_acc_jn %>% 
  filter(doy >= b_doy)

#create dummy observations

obs_dummy <- data.frame("dummy_doy" = c(60:70), "dummy_rain" = 0)
obs_dummy$dummy_rain[11] <- 1

#create dummy forecast:

#calculate percentiles from forecast ensemble
naefs_dummy <- moose_naefs %>% 
  filter(Parameter == "APCP-SFC") %>% 
  group_by(Parameter, Timestamp) %>% 
  mutate(q90 = quantile(Values, 0.90),
         q66 = quantile(Values, 0.66),
         q50 = quantile(Values, 0.50),
         q44 = quantile(Values, 0.44),
         q10 = quantile(Values, 0.10),
         doy = yday(Timestamp)) %>% 
  ungroup() %>% 
  select(doy, q90:q10) %>% 
  group_by(doy) %>% 
  summarise_all(funs((mean(.)/6) + 1)) %>% 
  mutate(dummy_doy = c(70:86))

#set facet labels
facet_labels <- c("severe" = "severe", "non.severe" = "non severe")

#plot cumulative rainfall
cum_rain_plot <- ggplot() + 
  geom_line(data = rain_acc_jn, aes(x = as.Date(doy, origin = "2018-01-01"), y = cum_rain, group = year, col = severity), size = 0.25, alpha = (0.25)) +
  geom_text(data = b_date_rain_acc, aes(x = as.Date(b_doy, origin = "2018-01-01"), cum_rain, group = year, label = year), col = "black", size = 2, alpha = 0.75) +
  geom_line(data = obs_dummy, aes(x = as.Date(dummy_doy, origin = "2018-01-01"), y = dummy_rain)) +
  #geom_point(data = moose_affes, aes(x = as.Date(dum_doy, origin = "2018-01-01"), y = cum_rain), shape = 1) +
  #geom_line(data = moose_affes, aes(x = as.Date(dum_doy, origin = "2018-01-01"), y = cum_rain), linetype = "dotted") +
  geom_ribbon(data = naefs_dummy, aes(x = as.Date(dummy_doy, origin = "2018-01-01"), ymax = q90, ymin = q10), alpha = 0.25) +
  geom_ribbon(data = naefs_dummy, aes(x = as.Date(dummy_doy, origin = "2018-01-01"), ymax = q66, ymin = q44), alpha = 0.50) +
  geom_line(data = naefs_dummy, aes(x = as.Date(dummy_doy, origin = "2018-01-01"), y = q50), linetype = "dashed") +
  geom_vline(xintercept = as.Date(70, origin = "2018-01-01"), linetype = "dotted") +
  geom_vline(xintercept = as.Date(86, origin = "2018-01-01"), linetype = "dotted") +
  geom_text(data = obs_dummy, aes(x = as.Date(63, origin = "2018-01-01"), y = 60, label = "observed"), size = 2) +
  geom_text(data = obs_dummy, aes(x = as.Date(78, origin = "2018-01-01"), y = 60, label = "forecast"), size = 2) +
  facet_grid(cols = vars(severity), labeller = as_labeller(facet_labels)) +
  scale_x_date(date_labels = "%b-%d", breaks = "2 weeks") +
  theme_bw() +
  labs(x = "", y = "cumulative rainfall (mm)", caption = "*rain defined as daily precipitation when mean air temperature is > 0 degrees Celcius") +
  ggtitle("Moosonee UA Climate Station") +
  theme(strip.background = element_blank(), legend.position = "none")
#ggsave(filename = here::here("plots", "mse_cum_rainfall_bdate_severity.png"))

plot(cum_rain_plot)
