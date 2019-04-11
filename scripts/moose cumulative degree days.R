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

path <- "C:/Users/beatonan/OneDrive - Government of Ontario/Documents/My Documents/R/FN_Hydro_FY18_19"

#function for calculating positive degree days of warming

calc_pdd <- function(x) {
  x <- na.approx(x)
  pos_temp <- case_when(
    x >= 0 ~ x,
    x < 0 ~ 0,
    is.na(x) ~ 0)
  pdd <- cumsum(pos_temp)
}

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

#select day total precip and mean daily air temp time-series
ts.f <- filter(ts, ts_name == "TAir.DayMean")

#download data from wiski and store as an object
clim_ls <- ki_timeseries_values(hub = 'swmc', 
                                ts_id = ts.f$ts_id, 
                                start_date = Sys.Date() - 9, 
                                end_date = Sys.Date())

#change to following lines when running during breakup 
#start_date = "2019-03-01", 
#end_date = Sys.Date())



#calculate cumulative positive degree days
wiski_pdd <- clim_ls %>%
  select(-Units) %>% 
  #calculate cumulative positive degree days
    mutate(pdd = calc_pdd(TAir)) %>%
    #add dummy doy
    mutate(dummy_doy = 60:69)

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
#   mutate(pdd = calc_pdd(TMP) %>%  
#     #create dummy doy for testing purposes starting on March 11th (doy 70)
#     dum_doy = c(70:71))

####----Gather NAEFS Forecasts----####

#view available parameters output by the naefs model
#params <- rfca_naefs_parameters()

#gather 0 zulu forecast using rforecastca 
moose_naefs <- rfca_naefs_forecast(
  forecast_time = "00",
  forecast_location = "Moosonee",
  parameter = c("TMP-SFC")
)

#calculate forecast percentiles from ensembles
naefs_q <- moose_naefs %>% 
  group_by(Parameter, Timestamp) %>% 
  summarise(q90 = quantile(Values, 0.90),
            q66 = quantile(Values, 0.66),
            q50 = quantile(Values, 0.50),
            q44 = quantile(Values, 0.44),
            q10 = quantile(Values, 0.10))

#rearrange and calc pdd for naefs forecast
naefs_pdd <- naefs_q %>% 
  arrange(Timestamp) %>% 
  #rearrange data so temp is in a unique columns
  gather(percentile, value, -Timestamp, -Parameter) %>% 
  spread(key = Parameter, value = value) %>%
  #rename columns to remove "-"
  rename("TMP" = "TMP-SFC") %>% 
  group_by(percentile) %>% 
  mutate(pdd = calc_pdd(TMP)) %>% 
  select(Timestamp, percentile, pdd) %>% 
  spread(percentile, pdd) %>% 
  ungroup()

# #plot forecast cumulative pdd
# naefs_pdd %>%
#   ggplot() +
#   geom_ribbon(aes(x = Timestamp, ymax = q90, ymin = q10), alpha = 0.25) +
#   geom_ribbon(aes(x = Timestamp, ymax = q66, ymin = q44), alpha = 0.50) +
#   geom_line(aes(x = Timestamp, y = q50), linetype = "dashed") +
#   labs(title = "Forecast at Moosonee UA Climate Station", x = "", y = expression("cumulative positive degree days "( degree*C))) +
#   theme_bw()

####----Calculate Historic PDD----####

#calculate pdd
msc_pdd <- m_ua_day %>% 
  ungroup() %>%
  select(date, mean_temp) %>%
  mutate(month = month(date),
         year = year(date),
         doy = yday(date)) %>%
  filter(month >= 3 & month < 6 & year >= 1955) %>%
  group_by(year) %>%
  #calculate % NA for each year
  mutate(perc_na = is.na(mean_temp)/length(mean_temp)) %>% 
  #did not use calc_pdd function as the naapprox with the groups causes issues
  mutate(pos_temp = case_when(
      mean_temp >= 0 ~ mean_temp,
      mean_temp < 0 ~ 0,
      is.na(mean_temp) ~ 0),
    pdd = cumsum(pos_temp))

#join with Moose breakup severity and filter values less than breakup date
pdd_jn <- msc_pdd %>% 
  left_join(mse_bdate, by = "year") %>% 
  filter(doy <= b_doy) %>% 
  filter(max(pdd) > 0) 

#find breakup dates that align with max depletion 
b_date_pdd <- pdd_jn %>% 
  filter(doy >= b_doy)

#create dummy observations

obs_dummy <- data.frame("dummy_doy" = c(60:70), "dummy_pdd" = 0)
obs_dummy$dummy_pdd[11] <- 1

#create dummy forecast:

#calculate percentiles from forecast ensemble
naefs_dummy <- moose_naefs %>% 
  filter(Parameter == "TMP-SFC") %>% 
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
pdd_plot <- ggplot() + 
  geom_line(data = pdd_jn, aes(x = as.Date(doy, origin = "2018-01-01"), y = pdd, group = year, col = severity), size = 0.25, alpha = (0.25)) +
  geom_text(data = b_date_pdd, aes(x = as.Date(b_doy, origin = "2018-01-01"), pdd, group = year, label = year), col = "black", size = 2, alpha = 0.75) +
  geom_line(data = obs_dummy, aes(x = as.Date(dummy_doy, origin = "2018-01-01"), y = dummy_pdd)) +
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
  labs(x = "", y = expression("cumulative positive degree days "( degree*C))) +
  ggtitle("Moosonee UA Climate Station") +
  theme(strip.background = element_blank(), legend.position = "none")
#ggsave(filename = here::here("plots", "mse_cum_pdd_bdate_severity.png"))

plot(pdd_plot)

# #check % NA for each year
# perc_na <- m_ua_day %>% 
#   ungroup() %>%
#   select(date, mean_temp) %>%
#   mutate(month = month(date),
#          year = year(date),
#          doy = yday(date)) %>%
#   filter(month >= 3 & month < 6 & year >= 1955) %>%
#   group_by(year) %>%
#   #calculate % NA for each year
#   mutate(na = is.na(mean_temp)) %>% 
#   summarize(n = n(),
#             na = sum(na),
#             percent_na = (na/n)*100)
# 
# #percent NA > 25
# perc_na_25 <- perc_na %>% 
#   filter(percent_na > 25)
# 
# #graph % NA
# perc_na %>% 
#   ggplot(aes(x = year, y = percent_na)) + 
#   geom_bar(stat = "identity", fill = "white", colour = "grey") +
#   geom_text(data = perc_na_25, aes(x = year, y = percent_na, label = year), angle = 90, size = 4) +
#   labs(title = "Percent of missing MSC daily mean air temperature at Moosonee UA Station",
#        x = "",
#        y = "percent missing") +
#   theme_bw()
# ggsave(filename = here::here("plots", "percent_missing_msc_temp.png"))
# 
# #check 2012 mean_temp
# chk_2012 <- m_ua_day %>% 
#   select(date, year, mean_temp) %>% 
#   filter(year == 2012)
# #NOTE: quite a bit of missinng data in March and April so good that it is not displaying
