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

#set path
path <- "C:/Users/beatonan/OneDrive - Government of Ontario/Documents/My Documents/R/FN_Hydro_FY18_19/"

conflict_prefer("filter", "dplyr")
#conflict_prefer("here", "here")

#define functions

calc_fdd <- function(x) {
  x <- na.approx(x)
  neg_temp <- case_when(
    x <= 0 ~ x,
    x > 0 ~ 0,
    is.na(x) ~ 0)
  fdd <- abs(cumsum(neg_temp))
}

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

#download data from wiski beginning in Oct and store as an object
clim_ls <- ki_timeseries_values(hub = 'swmc', 
                                ts_id = ts.f$ts_id, 
                                start_date = "2018-10-01", 
                                end_date = Sys.Date())

#combine precip and tair into 1 df 
clim_fdd <- clim_ls[[1]] %>% 
  left_join(clim_ls[[2]], by = "Timestamp") %>% 
  select(-Units.x, Units.y) %>% 
  #calculate fdd
  mutate(
    fdd = calc_fdd(TAir),
    wy_doy = seq(1:n())
  )

# ####----Gather AFFES Forecast---####
# 
# source(here::here("scripts", "shield_scraper.r"))
# 
# # Get AFFES forecast with Moosonee coordinates
# moose_affes <- shield_scraper(latitude = 51.2833, longitude = -80.6)
# 
# ##--calculate fdd from AFFES forecast--##
# 
# fdd_affes <- moose_affes %>% 
#   mutate(
#     fdd = max(clim_fdd$fdd) + calc_fdd(TMP),
#     wy_doy = max(clim_fdd$wy_doy) + row_number() - 1
#   )

##--Forecast fdd from NAEFS--##

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
fdd_naefs <- moose_naefs %>% 
  filter(Parameter == "TMP-SFC") %>% 
  select(-Parameter) %>%
  group_by(Timestamp) %>% 
  summarise(q90 = quantile(Values, 0.90),
            q66 = quantile(Values, 0.66),
            q50 = quantile(Values, 0.50),
            q44 = quantile(Values, 0.44),
            q10 = quantile(Values, 0.10)) %>%
  group_by(year(Timestamp), yday(Timestamp)) %>% 
  summarise_all(mean, na.rm = T) %>% 
  mutate_at(vars(matches("q")), funs(max(clim_fdd$fdd) + calc_fdd(.))) %>% 
  mutate(wy_doy = max(clim_fdd$wy_doy) + row_number() - 1)

##--Historic fdd from weathercan--##

fdd <- m_ua_day %>%
  mutate(snow_year = if_else(month(date) > 9, year(date) + 1, year(date))) %>%
  group_by(snow_year) %>%
  #filter(mean_temp < 0) %>%
  mutate(
    mean_temp = replace_na(mean_temp, 0),
    neg_temp = case_when(
      mean_temp <= 0 ~ mean_temp,
      mean_temp > 0 ~ 0),
    fdd = abs(cumsum(neg_temp))) %>%
  #mutate(fdd = replace(fdd, fdd < 1000, NA)) %>%
  #rename(year = snow_year, date_time = date) %>%
  select(date, snow_year, mean_temp, neg_temp, fdd) %>% 
  mutate(month = month(date),
         doy = yday(date)) %>%
  filter(snow_year >= 1955) %>% 
  #remove years with bad data
  filter(!(snow_year %in% c(1993:1996, 1998)))


#join with Moose breakup severity and filter values less than breakup date
fdd_jn <- fdd %>% 
  rename(year = snow_year) %>% 
  left_join(mse_bdate, by = "year") %>% 
  #filter winter months
  filter(month >= 10 | month <= 5) %>% 
  #filter out data greater than breakup date but exclude data from subsequent snow season
  filter(!(doy >= b_doy & month <= 10)) %>% 
  mutate(wy_doy = seq(1:n())) %>% 
  na.omit

#find breakup dates that align with max fdd
fdd_date <- fdd_jn %>% 
  filter(doy == b_doy - 1)

#set facet labels
facet_labels <- c("severe" = "severe", "non.severe" = "non severe")

#plot cumulative fdd
fdd_plot <- ggplot() + 
  geom_line(data = fdd_jn, aes(x = as.Date(wy_doy, origin = "2018-10-01"), y = fdd, group = year, col = severity), size = 0.25, alpha = (0.5)) +
  geom_text(data = fdd_date, aes(x = as.Date(b_doy + 60, origin = "2018-10-01"), y = fdd, group = year, label = year), col = "black", size = 2, alpha = 0.75) +
  geom_line(data = clim_fdd, aes(x = as.Date(wy_doy, origin = "2018-10-01"), y = fdd)) +
  #geom_point(data = fdd_affes, aes(x = as.Date(wy_doy, origin = "2018-10-01"), y = fdd), shape = 1) +
  #geom_line(data = fdd_affes, aes(x = as.Date(wy_doy, origin = "2018-10-01"), y = fdd), col = "grey") +
  geom_ribbon(data = fdd_naefs, aes(x = as.Date(wy_doy, origin = "2018-10-01"), ymax = q90, ymin = q10), alpha = 0.25) +
  geom_ribbon(data = fdd_naefs, aes(x = as.Date(wy_doy, origin = "2018-10-01"), ymax = q66, ymin = q44), alpha = 0.50) +
  geom_line(data = fdd_naefs, aes(x = as.Date(wy_doy, origin = "2018-10-01"), y = q50), linetype = "dashed") +
  facet_grid(cols = vars(severity), labeller = as_labeller(facet_labels)) +
  scale_x_date(date_labels = "%b", breaks = "month") +
  geom_vline(xintercept = as.Date(max(clim_fdd$wy_doy), origin = "2018-10-01"), linetype = "dotted") +
  geom_vline(xintercept = as.Date(max(fdd_naefs$wy_doy), origin = "2018-10-01"), linetype = "dotted") +
  geom_text(data = clim_fdd, aes(x = as.Date(max(clim_fdd$wy_doy)/2, origin = "2018-10-01"), y = 2500, label = "observed"), size = 3) +
  geom_text(data = clim_fdd, aes(x = as.Date(max(clim_fdd$wy_doy) + 5, origin = "2018-10-01"), y = 2500, label = "forecast"), size = 3, angle = 90) +
  theme_bw() +
  labs(x = "", y = expression("cumulative degree days of freezing "( degree*C))) +
  ggtitle("Moosonee UA Climate Station") +
  theme(strip.background = element_blank(), legend.position = "none")
#ggsave(filename = here::here("plots", "mse_cum_fdd_bdate_severity.png"))

plot(fdd_plot)



