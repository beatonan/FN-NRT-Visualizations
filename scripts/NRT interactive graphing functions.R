#--------------conditions report functions-----------------#

####----Setup----####
#load libraries
library(here)
library(readxl)
library(weathercan)
library(kiwisR)
library(tidyhydat)
library(conflicted)
library(tidyverse)
library(zoo)
library(rforecastca)
library(plotly)
library(crosstalk)
library(lubridate)
library(DT)
library(flexdashboard)

conflict_prefer("filter", "dplyr")
conflict_prefer("here", "here")
conflict_prefer("yday", "lubridate")
conflict_prefer("month", "lubridate")
conflict_prefer("year", "lubridate")

#set path
path <- "C:/Users/beatonan/OneDrive - Government of Ontario/Documents/My Documents/R/FN_Hydro_FY18_19/"

#get utils functions
source(paste0(path, "scripts/river conditions report utils.R"))


####----Data Import and formatting----####

####----Breakup Date and Severity Data----####

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

#Attawapiskat Breakup Date and Severity

att_bdate <- b_date %>%
  mutate(b_doy = yday(date),
         severity = if_else(severity == "1", "severe", "non.severe")) %>%
  filter(river == "attawap") %>%
  select(year, severity, b_doy)

#convert severity to a factor
att_bdate$severity <- as.factor(att_bdate$severity)
#change factor order for plotting
att_bdate$severity <- fct_relevel(att_bdate$severity, "severe","non.severe")

#Albany Breakup Date and Severity

alb_bdate <- b_date %>%
  mutate(b_doy = yday(date),
         severity = if_else(severity == "1", "severe", "non.severe")) %>%
  filter(river == "albany") %>%
  select(year, severity, b_doy)

#convert severity to a factor
alb_bdate$severity <- as.factor(alb_bdate$severity)
#change factor order for plotting
alb_bdate$severity <- fct_relevel(alb_bdate$severity, "severe","non.severe")

####----MSC Data----####

#import Moosonee Data
#may want to define date as date when importing b/c it takes a bit of time to do the conversion
m_ua_day <- read.csv(paste0(path, "import/", "moose_dly_clim.csv"))
#cast date column as date
m_ua_day$date <- as.Date(m_ua_day$date)

####----WSC Data----####

import_wsc <- function(csv, parameter) {
  
  #parameter <- quo(parameter)
  
  #import
  dat <- read.csv(here::here("import", csv))
  #convert to date
  dat$Date <- as.Date(dat$Date)
  #munge
  dat <- dat %>%
    #select(date = Date, parameter = Value) %>%
    select(date = Date, Value) %>%
    #rename(parameter = Value) %>% 
    mutate(month = month(date),
           year = year(date),
           doy = yday(date)) %>%
    filter(month >= 3 & month < 7 & year >= 1955) 
  
  colnames(dat)[2] <- parameter
  
  return(dat)
  
}

#import flow data:

mse_crs_q <- import_wsc("mse_crs_q.csv", parameter = "flow")

alb_hat_q <- import_wsc("alb_hat_q.csv", parameter = "flow")

att_muk_q <- import_wsc("att_muk_q.csv", parameter = "flow")

#import level data:

mse_crs_lvl <- import_wsc("mse_crs_lvl.csv", parameter = "level")

alb_hat_lvl <- import_wsc("alb_hat_lvl.csv", parameter = "level")

att_muk_lvl <- import_wsc("att_muk_lvl.csv", parameter = "level")

####----FDD Plot----####

fdd_plot <- function() {

####----Gather WISKI Data for Current Year----####

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
                                start_date = paste0(year(Sys.Date()) - 1, "-10-01"),
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

clim_date <- clim_fdd %>% 
  filter(Timestamp == max(Timestamp)) %>% 
  mutate(year = year(Timestamp))

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

# ####----Gather NAEFS Forecasts----####
# 
# #view available parameters output by the naefs model
# #params <- rfca_naefs_parameters()
# 
# #gather 0 zulu forecast using rforecastca 
# moose_naefs <- rfca_naefs_forecast(
#   forecast_time = "00",
#   forecast_location = "Moosonee",
#   parameter = c("APCP-SFC", "TMP-SFC")
# )
# 
# #calculate forecast percentiles from ensembles
# fdd_naefs <- moose_naefs %>% 
#   filter(Parameter == "TMP-SFC") %>% 
#   select(-Parameter) %>%
#   group_by(Timestamp) %>% 
#   summarise(q90 = quantile(Values, 0.90),
#             q66 = quantile(Values, 0.66),
#             q50 = quantile(Values, 0.50),
#             q44 = quantile(Values, 0.44),
#             q10 = quantile(Values, 0.10)) %>%
#   group_by(year(Timestamp), yday(Timestamp)) %>% 
#   summarise_all(mean, na.rm = T) %>% 
#   mutate_at(vars(matches("q")), funs(max(clim_fdd$fdd) + calc_fdd(.))) %>% 
#   mutate(wy_doy = max(clim_fdd$wy_doy) + row_number() - 1)

####----Gather MSC Historic Data----####

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
  na.omit %>% 
  ungroup() %>% 
  #attempt to sort year for display in filter box
  mutate(year = as.numeric(year)) %>% 
  arrange(year) %>% 
  mutate(year = as.factor(year)) 
  

#find breakup dates that align with max fdd
fdd_date <- fdd_jn %>% 
  filter(doy == b_doy - 1) %>% 
  ungroup()

#set facet labels
facet_labels <- c("severe" = "severe", "non.severe" = "non severe")

####----Generate Plot----####

fdd_jn3 <- SharedData$new(fdd_jn, ~year, "Highlight a year")

#plot cumulative fdd
gg_fdd <- ggplot() + 
  geom_line(data = fdd_jn3, aes(x = as.Date(wy_doy, origin = "2018-10-01"), y = fdd, group = year, col = severity), size = 0.25, alpha = (0.75)) +
  geom_text(data = fdd_date, aes(x = as.Date(b_doy + 60, origin = "2018-10-01"), y = fdd, group = year, label = year), col = "black", size = 2, alpha = 0.75) +
  geom_line(data = clim_fdd, aes(x = as.Date(wy_doy, origin = "2018-10-01"), y = fdd)) +
  geom_text(data = clim_date, aes(x = as.Date(wy_doy, origin = "2018-10-01"), y = fdd, label = year), col = "black", size = 2.5, alpha = 1) +
  #geom_point(data = fdd_affes, aes(x = as.Date(wy_doy, origin = "2018-10-01"), y = fdd), shape = 1) +
  #geom_line(data = fdd_affes, aes(x = as.Date(wy_doy, origin = "2018-10-01"), y = fdd), col = "grey") +
  #geom_ribbon(data = fdd_naefs, aes(x = as.Date(wy_doy, origin = "2018-10-01"), ymax = q90, ymin = q10), alpha = 0.25) +
  #geom_ribbon(data = fdd_naefs, aes(x = as.Date(wy_doy, origin = "2018-10-01"), ymax = q66, ymin = q44), alpha = 0.50) +
  #geom_line(data = fdd_naefs, aes(x = as.Date(wy_doy, origin = "2018-10-01"), y = q50), linetype = "dashed") +
  facet_grid(cols = vars(severity), labeller = as_labeller(facet_labels)) +
  scale_x_date(date_labels = "%b", breaks = "month") +
  #geom_vline(xintercept = as.Date(max(clim_fdd$wy_doy), origin = "2018-10-01"), linetype = "dotted") +
  #geom_vline(xintercept = as.Date(max(fdd_naefs$wy_doy), origin = "2018-10-01"), linetype = "dotted") +
  #geom_text(data = clim_fdd, aes(x = as.Date(max(clim_fdd$wy_doy)/2, origin = "2018-10-01"), y = 2500, label = "observed"), size = 3) +
  #geom_text(data = clim_fdd, aes(x = as.Date(max(clim_fdd$wy_doy) + 5, origin = "2018-10-01"), y = 2500, label = "forecast"), size = 3, angle = 90) +
  theme_bw() +
  #labs(x = "", y = expression("cumulative degree days of freezing "( degree*C))) +
  labs(x = "", y = "cumulative degree days of freezing (degrees C)") +
  ggtitle("Moosonee UA Climate Station") +
  theme(strip.background = element_blank(), legend.position = "none")
#ggsave(filename = here::here("plots", "mse_cum_fdd_bdate_severity.png"))

select <- highlight(
  ggplotly(gg_fdd, tooltip = "year"), 
  selectize = TRUE, persistent = FALSE
)

#fdd_int <- bscols(filter, select)

return(select)

}

####----Snow Depth Plot----####

sd_plot <- function(clim_id) {
  
  ####----Gather WISKI Data for Current Year----####
  
  sta <- ki_station_list(hub = 'swmc')
  
  station_name <- sta$station_name
  
  sta <- sta %>% 
    filter(grepl(clim_id, station_no))
  
  #get and store time-series information for Moosonee
  ts <- ki_timeseries_list(hub = 'swmc', station_id = sta$station_id)
  
  #select day total precip and mean daily air temp  and snow depth time-series
  ts.f <- filter(ts,ts_name == "SDepth.DayMean")
  
  #download data from wiski beginning in Oct and store as an object
  clim_ls <- ki_timeseries_values(hub = 'swmc',
                                  ts_id = ts.f$ts_id,
                                  start_date = paste0(year(Sys.Date()) - 1, "-10-01"),
                                  end_date = Sys.Date())
  
  clim_ls$Timestamp <- as.Date(clim_ls$Timestamp)
  
  #extract snow depth info and add wy doy
  clim_sd <- clim_ls %>%
    mutate(wy_doy = seq(1:n()))
  
  #extract date for geom_text() currrent year
  clim_date <- clim_sd %>% 
    filter(wy_doy == max(wy_doy)) %>% 
    mutate(year = year(Sys.Date()))
  
  #ggplot(xka, aes(x = Timestamp, y = SDepth)) + geom_line() + theme_bw()
  
  
  ####----Gather MSC Historic Data----####
  
  
  #get station ID
  msc_sta <- stations
  
  msc_sta <- msc_sta %>% 
    filter(grepl(clim_id, TC_id), interval == "day") #%>% 
  #pull(station_id)
  
  #DL daily data
  m_ua_day <- weather_dl(station_ids = msc_sta$station_id,
                         start = "1955-01-01",
                         end = "2018-06-01",
                         interval = "day",
                         tz_disp = "EST")
  
  #reformat table
  sd <- m_ua_day %>%
    select(date, month, snow_grnd) %>% 
    mutate(snow_year = if_else(month(date) > 9, year(date) + 1, year(date))) %>%
    group_by(snow_year) %>%
    mutate(doy = yday(date), month = as.numeric(month)) 
  
  #join with Moose breakup severity and filter values less than breakup date
  sd_jn <- sd %>% 
    rename(year = snow_year) %>% 
    left_join(mse_bdate, by = "year") %>% 
    #filter winter months
    filter(month >= 10 | month <= 5) %>% 
    mutate(wy_doy = seq(1:n())) #%>% 
  #filter out data greater than breakup date but exclude data from subsequent snow season
  #na.omit 
  #filter out bad data that are 0 prior to melt (assume Mar 15)
  #filter(!(snow_grnd < 5 & doy < 74)) 
  
  #find percent of missing data for each year and pull years with > 50% data
  miss_yr <- sd_jn %>%
    group_by(year) %>%
    summarise(n = n(), is_na = sum(is.na(snow_grnd)), perc_missing = is_na/n) %>% 
    filter(perc_missing > 0.5) %>% 
    pull(year)
  
  #filter out years with > 50% missing data 
  sd_jn <- sd_jn %>%
    filter(!(year %in% miss_yr)) %>% 
    na.omit
  
  #find breakup dates that align with max bdate
  sd_date <- sd_jn %>% 
    #filter(doy == b_doy - 1)
    slice(which.max(snow_grnd)) %>% 
    #filter(snow_grnd == max(snow_grnd)) %>% 
    ungroup()
  
  #ungroup for use in plotly
  sd_jn <- sd_jn %>% 
    ungroup()
  
  ####----Generate Plot----####
  
  #set facet labels
  facet_labels <- c("severe" = "severe", "non.severe" = "non severe")
  
  sd_jn3 <- SharedData$new(sd_jn, ~year, "Highlight a year")
  
  gg_sd <- ggplot() + 
    geom_line(data = sd_jn3, aes(x = as.Date(wy_doy, origin = "2018-10-01"), y = snow_grnd, group = year, col = severity), size = 0.25, alpha = 0.75) +
    geom_text(data = sd_date, aes(x = as.Date(wy_doy, origin = "2018-10-01"), y = snow_grnd, group = year, label = year), col = "black", size = 2, alpha = 0.75) +
    geom_line(data = clim_sd, aes(x = as.Date(wy_doy, origin = "2018-10-01"), y = SDepth)) +
    geom_text(data = clim_date, aes(x = as.Date(wy_doy, origin = "2018-10-01"), y = SDepth, label = year), col = "black", size = 2.5, alpha = 1) +
    facet_grid(cols = vars(severity), labeller = as_labeller(facet_labels)) +
    scale_x_date(date_labels = "%b", breaks = "month") +
    theme_bw() +
    labs(x = "", y = "snow depth (cm)") +
    ggtitle(paste0(sta$station_name, " Climate Station")) +
    #ggtitle("Moosonee UA Climate Station") +
    theme(strip.background = element_blank(), legend.position = "none")
  
  select <- highlight(
    ggplotly(gg_sd, tooltip = "year"), 
    selectize = TRUE, persistent = FALSE
  )
  
  return(select)
  
}

####----PDD Plot----####

pdd_plot <- function() {

####----Gather WISKI Data for Current Year----####

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
                                  start_date = paste0(year(Sys.Date()), "-03-01"),
                                  end_date = Sys.Date())

  #calculate cumulative positive degree days
  wiski_pdd <- clim_ls %>%
    select(-Units) %>%
    #calculate cumulative positive degree days
    mutate(pdd = calc_pdd(TAir),
           doy = yday(Timestamp)) 
  
  wiski_date <- wiski_pdd %>% 
    filter(Timestamp == max(Timestamp)) %>% 
    mutate(year = year(Timestamp))

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
              q75 = quantile(Values, 0.75),
              q66 = quantile(Values, 0.66),
              q50 = quantile(Values, 0.50),
              q25 = quantile(Values, 0.25),
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
    ungroup() %>% 
    mutate(date = as.Date(Timestamp)) %>% 
    select(-Timestamp) %>% 
    group_by(date) %>% 
    summarise_at(vars(q10:q90), function(x){mean(x) + wiski_date$pdd}) %>% 
    mutate(doy = yday(date))
    

  # #plot forecast cumulative pdd
  # naefs_pdd %>%
  #   ggplot() +
  #   geom_ribbon(aes(x = Timestamp, ymax = q90, ymin = q10), alpha = 0.25) +
  #   geom_ribbon(aes(x = Timestamp, ymax = q66, ymin = q44), alpha = 0.50) +
  #   geom_line(aes(x = Timestamp, y = q50), linetype = "dashed") +
  #   labs(title = "Forecast at Moosonee UA Climate Station", x = "", y = expression("cumulative positive degree days "( degree*C))) +
  #   theme_bw()
  
  ####----Gather MSC Historic PDD----####
  
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
    filter(doy >= b_doy) %>% 
    ungroup()
  
  #Ungroup table for plotly
  pdd_jn <- pdd_jn %>% 
    ungroup()
  
####----Generate Plot----####
  
  #set facet labels
  facet_labels <- c("severe" = "severe", "non.severe" = "non severe")

  pdd_jn3 <- SharedData$new(pdd_jn, ~year, "Highlight a year")
  
  #plot cumulative positive degree days
  gg_pdd <- ggplot() + 
    geom_line(data = pdd_jn3, aes(x = as.Date(doy, origin = "2018-01-01"), y = pdd, group = year, col = severity), size = 0.25, alpha = 0.75) +
    geom_text(data = b_date_pdd, aes(x = as.Date(b_doy, origin = "2018-01-01"), pdd, group = year, label = year), col = "black", size = 2, alpha = 0.75) +
    geom_text(data = wiski_date, aes(x = as.Date(doy, origin = "2018-01-01"), y = pdd + 1, label = year), col = "black", size = 2.5, alpha = 1) +
    geom_line(data = wiski_pdd, aes(x = as.Date(doy, origin = "2018-01-01"), y = pdd)) +
    #geom_point(data = moose_affes, aes(x = as.Date(dum_doy, origin = "2018-01-01"), y = cum_rain), shape = 1) +
    #geom_line(data = moose_affes, aes(x = as.Date(dum_doy, origin = "2018-01-01"), y = cum_rain), linetype = "dotted") +
    #geom_ribbon(data = naefs_pdd, aes(x = as.Date(doy, origin = "2018-01-01"), ymax = q90, ymin = q10), alpha = 0.25) +
    #geom_ribbon(data = naefs_pdd, aes(x = as.Date(doy, origin = "2018-01-01"), ymax = q66, ymin = q44), alpha = 0.50) +
    geom_ribbon(data = naefs_pdd, aes(x = as.Date(doy, origin = "2018-01-01"), ymax = q75, ymin = q25), alpha = 0.50) +
    geom_line(data = naefs_pdd, aes(x = as.Date(doy, origin = "2018-01-01"), y = q50), linetype = "dotted") +
    geom_vline(xintercept = as.Date(max(wiski_pdd$doy), origin = "2018-01-01"), linetype = "dotted") +
    geom_vline(xintercept = as.Date(max(naefs_pdd$doy), origin = "2018-01-01"), linetype = "dotted") +
    #geom_text(data = wiski_pdd, aes(x = as.Date(((max(doy) - 60)/2), origin = "2018-03-01"), y = -2, label = "observed"), size = 2, angle = 0) +
    #geom_text(data = naefs_pdd, aes(x = as.Date(((max(naefs_pdd$doy) - max(wiski_pdd$doy))/2) + max(wiski_pdd$doy) - 60, origin = "2018-03-01"), y = -2, label = "forecast"), size = 2, angle = 0) +
    facet_grid(cols = vars(severity), labeller = as_labeller(facet_labels)) +
    scale_x_date(date_labels = "%b-%d", breaks = "2 weeks") +
    theme_bw() +
    #labs(x = "", y = expression("cumulative positive degree days "( degree*C))) +
    labs(x = "", y = "cumulative positive degree days (degree C)") +
    ggtitle("Moosonee UA Climate Station") +
    theme(strip.background = element_blank(), legend.position = "none")
  #ggsave(filename = here::here("plots", "mse_cum_pdd_bdate_severity.png"))

  
# plt_tst %>% 
#     add_segments(x = "2018-03-10", xend = "2018-03-10", y = 0, yend = 85)
    
# plot_ly() %>% 
# add_segments(x = 5, xend = 5, y = 0, yend = 5)

  select <- highlight(
    ggplotly(gg_pdd, tooltip = "year"), 
    selectize = TRUE, persistent = FALSE
  )
  
  return(select)
  
}

####----Cumulative Rain Plot----####


  cum_rain_plot <- function() {

####----Gather WISKI Data for Current----#### 
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
                                start_date = paste0(year(Sys.Date()), "-03-01"),
                                end_date = Sys.Date())

#combine precip and tair into 1 df
wiski_cum_rain <- clim_ls[[1]] %>%
  left_join(clim_ls[[2]], by = "Timestamp") %>%
  select(-Units.x, Units.y) %>%
  #calculate cumulative rain
  mutate(
    rain = case_when(
      TAir >= 0 ~ Precip,
      TAir < 0 ~ 0),
    cum_rain = cumsum(rain),
    #add  doy
    doy = yday(Timestamp)
  )

wiski_date <- wiski_cum_rain %>% 
  filter(Timestamp == max(Timestamp)) %>% 
  mutate(year = year(Timestamp))

# # ####----Gather AFFES Forecast---####
# # 
# # source(here::here("scripts", "shield_scraper.r"))
# # 
# # # Get AFFES forecast with Moosonee coordinates
# # moose_affes <- shield_scraper(latitude = 51.2833, longitude = -80.6)
# # 
# # #NOTE: need to add observed cumulative rain to date and calculate rain vs snow from temp
# # 
# # #calculate forecast cumulative pcp
# # moose_affes <- moose_affes %>% 
# #   #create dummy doy for testing purposes starting on March 11th (doy 70)
# #   mutate(rain = case_when(
# #     TMP >= 0 ~ APCP,
# #     TMP < 0 ~ 0),
# #     cum_rain = max(clim$cum_rain) + cumsum(rain),
# #     #dum_doy = c(70:74)
# #     dum_doy = c(70:71))
# 
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
            q75 = quantile(Values, 0.75),
            q66 = quantile(Values, 0.66),
            q50 = quantile(Values, 0.50),
            q44 = quantile(Values, 0.44),
            q25 = quantile(Values, 0.25),
            q10 = quantile(Values, 0.10))

# #convert from cumulative pcp to daily pcp
# naefs_dly_pcp <- naefs_q %>%
#   filter(Parameter != "TMP-SFC") %>%
#   arrange(Timestamp) %>%
#   mutate_at(vars(matches("q")), funs(c(.[1], diff(.)))) %>% 
  # mutate(Timestamp = as.Date(Timestamp)) %>% 
  # group_by(Timestamp) %>% 
  # summarise_at(vars(q50), mean, na.rm = T)

# #combine daily pcp with daily mean temp
# naefs_cum_rain <- naefs_q %>%
#   filter(Parameter == "TMP-SFC") %>%
#   mutate_at(vars(q90:q10), function(x){x = .$q50}) %>% 
#   #select(Parameter, Timestamp, q50) %>% 
#   # mutate(Timestamp = as.Date(Timestamp)) %>% 
#   # group_by(Timestamp) %>% 
#   # summarise_at(vars(q50), mean, na.rm = T) %>% 
#   bind_rows(naefs_dly_pcp) %>%
#   #rearrange data so temp and pcp are unique columns
#   gather(percentile, value, -Timestamp, -Parameter) %>%
#   spread(key = Parameter, value = value) %>%
#   #rename columns to remove "-"
#   rename("TMP" = "TMP-SFC", "PCP" = "APCP-SFC") %>%
#   mutate(
#     rain = case_when(
#       TMP >= 0 ~ PCP,
#       TMP < 0 ~ 0)) %>%
#   arrange(Timestamp) %>%
#   group_by(percentile) %>%
#   mutate(cum_rain = cumsum(rain)) %>%
#   select(Timestamp, percentile, cum_rain) %>%
#   spread(percentile, cum_rain) %>%
#   ungroup() %>% 
#   mutate(date = as.Date(Timestamp)) %>% 
#   select(-Timestamp) %>% 
#   group_by(date) %>% 
#   summarise_at(vars(q10:q90), function(x){mean(x) + wiski_date$cum_rain}) %>% 
#   mutate(doy = yday(date))
# 
# #plot forecast cumulative rain
# naefs_cum_rain %>%
#   ggplot() +
#   geom_ribbon(aes(x = date, ymax = q75, ymin = q25), alpha = 0.25) +
#   #geom_ribbon(aes(x = date, ymax = q66, ymin = q44), alpha = 0.50) +
#   geom_line(aes(x = date, y = q50), linetype = "dashed") +
#   labs(title = "Forecast at Moosonee UA Climate Station", x = "", y = "cumulative rainfall (mm)") +
#   theme_bw()

####----Gather MSC Historic Cumulative Rain----####

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
  filter(doy >= b_doy) %>% 
  ungroup()

#ungroup for plotly
rain_acc_jn <- rain_acc_jn %>% 
  ungroup()

# #create dummy observations
# 
# obs_dummy <- data.frame("dummy_doy" = c(60:70), "dummy_rain" = 0)
# obs_dummy$dummy_rain[11] <- 1
# 
# #create dummy forecast:
# 
# #calculate percentiles from forecast ensemble
# naefs_dummy <- moose_naefs %>% 
#   filter(Parameter == "APCP-SFC") %>% 
#   group_by(Parameter, Timestamp) %>% 
#   mutate(q90 = quantile(Values, 0.90),
#          q66 = quantile(Values, 0.66),
#          q50 = quantile(Values, 0.50),
#          q44 = quantile(Values, 0.44),
#          q10 = quantile(Values, 0.10),
#          doy = yday(Timestamp)) %>% 
#   ungroup() %>% 
#   select(doy, q90:q10) %>% 
#   group_by(doy) %>% 
#   summarise_all(funs((mean(.)/6) + 1)) %>% 
#   mutate(dummy_doy = c(70:86))

####----Generate Plot----####

rain_acc_jn3 <- SharedData$new(rain_acc_jn, ~year, "Highlight a year")

#set facet labels
facet_labels <- c("severe" = "severe", "non.severe" = "non severe")

#plot cumulative rainfall
gg_cum_rain <- ggplot() + 
  geom_line(data = rain_acc_jn3, aes(x = as.Date(doy, origin = "2018-01-01"), y = cum_rain, group = year, col = severity), size = 0.25, alpha = (0.75)) +
  geom_text(data = b_date_rain_acc, aes(x = as.Date(b_doy, origin = "2018-01-01"), cum_rain, group = year, label = year), col = "black", size = 2, alpha = 0.75) +
  geom_text(data = wiski_date, aes(x = as.Date(doy, origin = "2018-01-01"), y = cum_rain, label = year), col = "black", size = 2.5, alpha = 1) +
  geom_line(data = wiski_cum_rain, aes(x = as.Date(doy, origin = "2018-01-01"), y = cum_rain)) +
  #geom_point(data = moose_affes, aes(x = as.Date(dum_doy, origin = "2018-01-01"), y = cum_rain), shape = 1) +
  #geom_line(data = moose_affes, aes(x = as.Date(dum_doy, origin = "2018-01-01"), y = cum_rain), linetype = "dotted") +
  #geom_ribbon(data = naefs_cum_rain, aes(x = as.Date(doy, origin = "2018-01-01"), ymax = q90, ymin = q10), alpha = 0.25) +
  #geom_ribbon(data = naefs_cum_rain, aes(x = as.Date(doy, origin = "2018-01-01"), ymax = q66, ymin = q44), alpha = 0.50) +
  #geom_line(data = naefs_cum_rain, aes(x = as.Date(doy, origin = "2018-01-01"), y = q50), linetype = "dotted") +
  geom_vline(xintercept = as.Date(max(wiski_cum_rain$doy), origin = "2018-01-01"), linetype = "dotted") +
  #geom_vline(xintercept = as.Date(max(naefs_cum_rain$doy), origin = "2018-01-01"), linetype = "dotted") +
  #geom_text(data = wiski_cum_rain, aes(x = as.Date(((max(doy) - 60)/2), origin = "2018-03-01"), y = -2, label = "observed"), size = 2, angle = 90) +
  #geom_text(data = naefs_cum_rain, aes(x = as.Date(((max(naefs_cum_rain$doy) - max(wiski_cum_rain$doy))/2), origin = "2018-03-01"), y = -2, label = "forecast"), size = 2, angle = 90) +
  facet_grid(cols = vars(severity), labeller = as_labeller(facet_labels)) +
  scale_x_date(date_labels = "%b-%d", breaks = "2 weeks") +
  theme_bw() +
  labs(x = "", y = "cumulative rainfall (mm)", caption = "*rain defined as daily precipitation when mean air temperature is > 0 degrees Celcius") +
  ggtitle("Moosonee UA Climate Station") +
  theme(strip.background = element_blank(), legend.position = "none")
#ggsave(filename = here::here("plots", "mse_cum_rainfall_bdate_severity.png"))

select <- highlight(
  ggplotly(gg_cum_rain, tooltip = "year"), 
  selectize = TRUE, persistent = FALSE
)

return(select)

}

####----Snow Depth Depletion----####

depl_plot <- function() {

  ####----Gather WISKI Data for Current Year----####

  ##download NRT Moosonee CLIM data from Wiski using kiwisR
  #get and store station information for Moosonee
  sta <- ki_station_list(hub = 'swmc', search_term = "*Moosonee RCS*")

  #get and store time-series information for Moosonee
  ts <- ki_timeseries_list(hub = 'swmc', station_id = sta$station_id)

  #select day total precip and mean daily air temp  and snow depth time-series
  ts.f <- filter(ts, ts_name == "SDepth.6hr.P")

  #download data from wiski beginning in Mar and store as an object
  clim_ls <- ki_timeseries_values(hub = 'swmc',
                                  ts_id = ts.f$ts_id,
                                  start_date = paste0(year(Sys.Date()), "-03-01"),
                                  end_date = Sys.Date())

  #calculate daily cumulative sd depletion
  wiski_sdepl <- clim_ls %>%
    mutate(Timestamp = as.Date(Timestamp, format = "%Y-%m-%d")) %>%
    group_by(Timestamp) %>%
    summarise(SDepth = mean(SDepth, na.rm = T)) %>%
    mutate(depl = diff(c(SDepth[1],SDepth)),
           depl_neg = if_else(depl < 0, abs(depl), 0),
           cum_depl = cumsum(depl_neg)) %>%
    na.omit()
  
  wiski_date <- wiski_sdepl %>% 
    filter(Timestamp == max(Timestamp)) %>% 
    mutate(year = year(Timestamp))
  
  #ggplot(clim_sd, aes(x = Timestamp, y = cum_depl)) + geom_line()
  
  ####----Gather MSC Historic Depth Depletion----#### 
  
  msc_sdepl <- m_ua_day %>%
    select(date, year, month, day, snow_grnd) %>%
    filter(month >= 3 & month < 6 & year >= 1955) %>% 
    filter(!(year %in% c(1993:1996, 1998))) %>% 
    group_by(year) %>% 
    #replace 0 values with NA as there are instances of random zero values in ts that 
    #cause highly inflated depletion values
    mutate(depth_na  = if_else(snow_grnd == 0, NA_integer_, snow_grnd),
           #interpolate between NA values
           depth_int = na.approx(depth_na, rule = 2),
           depl = diff(c(depth_int[1],depth_int)),
           depl_neg = if_else(depl < 0, abs(depl), as.double(0)),
           cum_depl = cumsum(depl_neg)) %>% 
    select(date, year, cum_depl)
  
  #ggplot(msc_sdepl, aes(x = date, y = cum_depl)) + geom_line()
  
  #join with depth depletion with bu severity
  depl_jn <- msc_sdepl  %>% 
    left_join(mse_bdate, by = "year") %>% 
    mutate(doy = yday(date)) %>% 
    filter(doy <= b_doy) 
  
  #find breakup dates that align with max depletion
  depl_date <- depl_jn %>% 
    group_by(year) %>% 
    filter(doy == b_doy) %>% 
    ungroup()
  #slice(which.max(cum_depl))
  
  #ungroup joined depletion table for plotly 
  depl_jn <- depl_jn %>% 
    ungroup()
  
  ####----Generate Plot----####
  
  #set facet labels
  facet_labels <- c("severe" = "severe", "non.severe" = "non severe")
  
  depl_jn3 <- SharedData$new(depl_jn, ~year, "Highlight a year")
  
  #plot sd depl
  gg_sdepl <- ggplot() + 
    geom_line(data = depl_jn3, aes(x = as.Date(yday(date), origin = "2018-01-01"), y = cum_depl, group = year, col = severity), size = 0.25, alpha = (0.75)) +
    geom_text(data = wiski_date, aes(x = as.Date(yday(Timestamp), origin = "2018-01-01"), y = cum_depl, label = year), col = "black", size = 2.5, alpha = 1) +
    geom_text(data = depl_date, aes(x = as.Date(yday(date), origin = "2018-01-01"), y = cum_depl, group = year, label = year), col = "black", size = 2, alpha = 0.75) +
    geom_line(data = wiski_sdepl, aes(x = as.Date(yday(Timestamp), origin = "2018-01-01"), y = cum_depl)) +
    facet_grid(cols = vars(severity), labeller = as_labeller(facet_labels)) +
    scale_x_date(date_labels = "%b-%d", breaks = "2 weeks") +
    theme_bw() +
    labs(x = "", y = "cumulative snow depth depletion (cm)") +
    ggtitle("Moosonee UA Climate Station") +
    theme(strip.background = element_blank(), legend.position = "none")
  #save figure to disk
  #ggsave(filename = here::here("plots", "mse_sd_depl_bdate_severity.png"))
  
  select <- highlight(
    ggplotly(gg_sdepl, tooltip = "year"), 
    selectize = TRUE, persistent = FALSE
  )
  
  return(select)

}

####----Cumulative Rain + Snow Depth Depletion----####

cum_rain_depl_plot <- function() {
  
  ####----Gather WISKI Data for Current Year----####

  ##--WISKI Cumulative Snow Depletion--##
  #get and store station information for Moosonee
  sta <- ki_station_list(hub = 'swmc', search_term = "*Moosonee RCS*")

  #get and store time-series information for Moosonee
  ts <- ki_timeseries_list(hub = 'swmc', station_id = sta$station_id)

  #select day total precip and mean daily air temp  and snow depth time-series
  ts.f <- filter(ts, ts_name == "SDepth.DayMean")

  #download data from wiski beginning in Oct and store as an object
  clim_ls <- ki_timeseries_values(hub = 'swmc',
                                  ts_id = ts.f$ts_id,
                                  start_date = paste0(year(Sys.Date()), "-03-01"),
                                  end_date = Sys.Date())
  #end_date = Sys.Date())

  #interpolate missing SDepth Values
  clim_ls$SDepth <- na.approx(clim_ls$SDepth, rule = 2)

  #calculate daily cumulative sd depletion from WISKI data
  wiski_sdepl <- clim_ls %>%
    mutate(Timestamp = as.Date(Timestamp, format = "%Y-%m-%d")) %>%
    mutate(depl = diff(c(SDepth[1],SDepth)),
           depl_neg = if_else(depl < 0, abs(depl), 0),
           cum_depl = cumsum(depl_neg)) %>%
    na.omit() %>%
    select(Timestamp, cum_depl)
  

  ##--WISKI Cumulative Rain--##

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
                                  start_date = paste0(year(Sys.Date()), "-03-01"),
                                  end_date = Sys.Date())

  #combine precip and tair into 1 df
  wiski_rain_acc <- clim_ls[[1]] %>%
    left_join(clim_ls[[2]], by = "Timestamp") %>%
    select(-Units.x, Units.y) %>%
    mutate(Timestamp = as.Date(Timestamp, format = "%Y-%m-%d")) %>%
    #calculate cumulative rain
    mutate(
      rain = case_when(
        TAir >= 0 ~ Precip,
        TAir < 0 ~ 0),
      cum_rain = cumsum(rain)) %>%
    select(Timestamp, cum_rain)

  #combine wiski data
  wiski_jn <- wiski_rain_acc %>%
    left_join(wiski_sdepl, by = "Timestamp") %>%
    mutate(cum_rain_sd  = cum_rain + (cum_depl))
  
  wiski_date <- wiski_jn %>% 
    filter(Timestamp == max(Timestamp)) %>% 
    mutate(year = year(Timestamp))
  
  ####----Gather MSC Historic Cumulative Rain and Snow Depth Depletion----####
  
  #calculate daily cumulative sd depletion from historic MSC data
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
  
  #----Calculate Historic Rain Accumulation----#
  
  #calculate accumulated rainfall
  msc_rain_acc <- m_ua_day %>% 
    ungroup() %>%
    select(date, total_rain) %>%
    mutate(month = month(date),
           year = year(date),
           doy = yday(date)) %>%
    filter(month >= 3 & month < 6 & year >= 1955) %>%
    #filter out years with bad/no data
    filter(!(year %in% c(1993, 1994, 1995, 1996))) %>% 
    group_by(year) %>%
    mutate(total_rain = replace(total_rain, is.na(total_rain),0)) %>% 
    mutate(cum_rain = cumsum(total_rain)) %>% 
    select(date, year, cum_rain)
  
  ##----Combine depth depletion and cumulative rainfall----##
  
  #combine msc data
  msc_jn <- msc_rain_acc %>% 
    left_join(msc_sdepl, by = c("year", "date")) %>% 
    #mutate(cum_rain = replace_na(cum_rain, 0),
    #cum_depl = replace_na(cum_depl, 0)) %>% 
    mutate(cum_rain_sd  = cum_rain + (cum_depl),
           doy = yday(date)) %>% 
    left_join(mse_bdate, by = "year") %>% 
    filter(doy <= b_doy) %>% 
    #remove outliers that are causing plotting issues
    filter(!(year %in% c(1998, 2005))) %>%
    filter(!(doy < 74 & cum_depl > 1000)) %>% 
    filter(!(date == "1997-05-01")) 
  
  #interpolate missing SDepth Values
  msc_jn$cum_rain_sd <- na.approx(msc_jn$cum_rain_sd, rule = 2)
  
  #find breakup dates that align with max bdate
  cum_date <- msc_jn %>% 
    group_by(year) %>% 
    mutate(doy = yday(date)) %>% 
    filter(doy == b_doy - 1) %>%  # -1 so that 1997 will show up because its last day was removed as it was causing issues
    ungroup()
    #filter(doy == b_doy - 1)
  #filter(snow_grnd == max(snow_grnd))
  
  #ungroup data for plotly
  
  msc_jn <- msc_jn %>% 
    ungroup()
  
  ####----Generate Plots----####
  
  #set facet labels
  facet_labels <- c("severe" = "severe", "non.severe" = "non severe")
  
  msc_jn3 <- SharedData$new(msc_jn, ~year, "Highlight a year")
  
  #plot cumulative rainfall
  gg_cum_rain_sdepl <- ggplot() + 
    geom_line(data = msc_jn3, aes(x = as.Date(yday(date), origin = "2018-01-01"), y = cum_rain_sd, group = year, col = severity), size = 0.25, alpha = (0.75)) +
    geom_text(data = cum_date, aes(x = as.Date(b_doy, origin = "2018-01-01"), cum_rain_sd, group = year, label = year), col = "black", size = 2, alpha = 0.75) +
    geom_line(data = wiski_jn, aes(x = as.Date(yday(Timestamp), origin = "2018-01-01"), y = cum_rain_sd)) +
    geom_text(data = wiski_date, aes(x = as.Date(yday(Timestamp), origin = "2018-01-01"), y = cum_rain_sd, label = year), col = "black", size = 2.5, alpha = 1) +
    facet_grid(cols = vars(severity), labeller = as_labeller(facet_labels)) +
    scale_x_date(date_labels = "%b-%d", breaks = "2 weeks") +
    theme_bw() +
    labs(x = "", y = "cumulative rainfall + snow depth depletion (mm)") +
    ggtitle("Moosonee UA Climate Station") +
    theme(strip.background = element_blank(), legend.position = "none")
  #ggsave(filename = here::here("plots", "mse_cum_rainfall_sd_bdate_severity.png"))
  
  select <- highlight(
    ggplotly(gg_cum_rain_sdepl, tooltip = "year"), 
    selectize = TRUE, persistent = FALSE
  )
  
  return(select)
  
  
}

####----Moose above Moose Flow----####

mse_04LG004_q <- function() {
  
  ####----Gather WISKI Flow Data----####
  
  #Get WISKI Flow Data For Moose River above Moose River Crossing
  #download moose river WL from wiski using kiwisR
  sta <- ki_station_list(hub = 'swmc', search_term = "Moose River above Moose River*")
  
  #get and store time-series information for Moosonee
  ts <- ki_timeseries_list(hub = 'swmc', station_id = sta$station_id)
  
  #Get list of time-series names for WWP limits
  ts.limit <- filter(ts, ts_name == "WWP_AlarmLevel_High Limit")
  
  #Get ts name for Q
  ts.q <- filter(ts, ts_name == "Q.DayMean") 
  
  #retrieve WWP limits from WISKI
  lst.limit <- ki_timeseries_values(hub = 'swmc', 
                                    ts_id = ts.limit$ts_id,
                                    start_date = as.Date("1900-01-01"), 
                                    end_date = as.Date("1900-01-01"))
  
  #retrieve Q from WISKI
  lst.q <- ki_timeseries_values(hub = 'swmc',
                                ts_id = ts.q$ts_id,
                                start_date = paste0(year(Sys.Date()), "-03-01"),
                                end_date = Sys.Date())
  
  limit <- lst.limit[["Moose River above Moose River (Q)"]][["Q"]]
  
  wiski_date <- lst.q %>% 
    filter(Timestamp == max(Timestamp)) %>% 
    mutate(year = year(Timestamp))
  
  ####----Gather WSC Historic Flow----####
  
  #join historic flow with breakup severity
  q_jn <- mse_crs_q %>% 
    left_join(mse_bdate, by = "year") %>% 
    #filter(doy <= b_doy) %>% 
    filter(max(flow) > 0) 
  
  #find breakup dates that align with max flow
  q_date <- q_jn %>% 
    #filter(doy == b_doy - 1)
    group_by(year) %>% 
    slice(which.max(flow)) %>% 
    ungroup()
  
  #ungroup for plotly
  q_jn <- q_jn %>% 
    ungroup()
  
  ####----Generate Plot----####
  
  #set facet labels
  facet_labels <- c("severe" = "severe", "non.severe" = "non severe")
  
  #create table of WWP limits
  #limits <- data.frame(limit = "high limit", value = c(3000, 5000, 6000),  )
  
  blank = data.frame(type = "high", y = limit)
  
  q_jn3 <- SharedData$new(q_jn, ~year, "Highlight a year")
  
  #plot Q
  hyd_dat_plot <- ggplot() + 
    geom_line(data = lst.q, aes(x = as.Date(yday(Timestamp), origin = "2018-01-01"), y = Q)) +
    geom_text(data = wiski_date, aes(x = as.Date(yday(Timestamp), origin = "2018-01-01"), y = Q, label = year), col = "black", size = 2.5, alpha = 1) +
    geom_line(data = q_jn3, aes(x = as.Date(doy, origin = "2018-01-01"), y = flow, group = year, col = severity), size = 0.25, alpha = (0.75)) +
    geom_text(data = q_date, aes(x = as.Date(doy, origin = "2018-01-01"), y = flow, group = year, label = year), col = "black", size = 2, alpha = 0.75) +
    geom_hline(yintercept = limit, linetype = "dashed", size = 0.25, alpha = 0.5) +
    #geom_hline(yintercept = limit, linetype = "dashed") +
    geom_text(data = blank, aes(x = as.Date(80, origin = "2018-01-01")), y = limit, label = "WWP High Limit", size = 3, vjust = -0.8) +
    #geom_text(data = blank, aes(x = as.Date(80, origin = "2018-01-01")), y = limit, label = "WWP High Limit", size = 2, vjust = -0.8) +
    facet_grid(cols = vars(severity), labeller = as_labeller(facet_labels)) +
    scale_x_date(date_labels = "%b-%d", breaks = "3 weeks") +
    theme_bw() +
    labs(x = "", y = "flow (cms)", caption = "*year is positioned at peak flow, not breakup date") +
    ggtitle("Moosonee above Moose River Crossing (04LG004)") +
    theme(strip.background = element_blank(), legend.position = "none")
  
  select <- highlight(
    ggplotly(hyd_dat_plot, tooltip = "year"), 
    selectize = TRUE, persistent = FALSE
  )
  
  return(select)
  
}

####----Albany River at Hat Island Flow----####

alb_04HA001_q <- function() {

####----Gather WISKI Flow and Alarm Data----####
  
#Get WISKI Flow Data For Attawapisak River Below Muketei
#download moose river WL from wiski using kiwisR
sta <- ki_station_list(hub = 'swmc', search_term = "Albany River near Hat Island*")

#get and store time-series information for Moosonee
ts <- ki_timeseries_list(hub = 'swmc', station_id = sta$station_id)

#Get list of time-series names for WWP limits
ts.limit <- dplyr::filter(ts, ts_name == "WWP_AlarmLevel_High Limit")

#Get ts name for Q
ts.q <- dplyr::filter(ts, ts_name == "Q.DayMean") 

#retrieve WWP limits from WISKI
lst.limit <- ki_timeseries_values(hub = 'swmc', 
                                  ts_id = ts.limit$ts_id,
                                  start_date = as.Date("1900-01-01"), 
                                  end_date = as.Date("1900-01-01"))

#retrieve Q from WISKI
lst.q <- ki_timeseries_values(hub = 'swmc',
                              ts_id = ts.q$ts_id,
                              start_date = paste0(year(Sys.Date()), "-03-01"),
                              end_date = Sys.Date())

wiski_date <- lst.q %>% 
  filter(Timestamp == max(Timestamp)) %>% 
  mutate(year = year(Timestamp))

limit <- lst.limit[["Albany River near Hat Island (Q)"]][["Q"]]

####---Gather WSC Historic Flow----####

#join historic flow with breakup severity
q_jn <- alb_hat_q %>% 
  left_join(alb_bdate, by = "year")  
  #filter(doy <= b_doy) %>% 
  #filter(max(flow) > 0) 

#find breakup dates that align with max flow
q_date <- q_jn %>% 
  #filter(doy == b_doy - 1)
  group_by(year) %>% 
  slice(which.max(flow)) %>% 
  ungroup() 

####----Generate Plot----####

#set facet labels
facet_labels <- c("severe" = "severe", "non.severe" = "non severe")

#create table of WWP limits
#limits <- data.frame(limit = "high limit", value = c(3000, 5000, 6000),  )

blank = data.frame(type = "high", y = limit)

#create highlight graph

q_jn3 <- SharedData$new(q_jn, ~year, "Highlight a year")


#plot Q: Modified
flow_plot <- ggplot() + 
  geom_line(data = lst.q, aes(x = yday(Timestamp), y = Q)) +
  geom_text(data = wiski_date, aes(x = yday(Timestamp), y = Q, label = year), col = "black", size = 2.5, alpha = 1) +
  #geom_line(data = q_jn3, aes(x = as.Date(doy, origin = "2018-01-01"), y = flow, group = year, col = severity), size = 0.25) +
  geom_line(data = q_jn3, aes(x = doy, y = flow, group = year, col = severity), size = 0.25, alpha = 0.75) +
  #geom_text(data = q_date, aes(x = as.Date(doy, origin = "2018-01-01"), y = flow, group = year, label = year), col = "black", size = 2, alpha = 0.75) +
  geom_text(data = q_date, aes(x = doy, y = flow, group = year, label = year), col = "black", size = 2, alpha = 0.75) +
  geom_hline(yintercept = limit, linetype = "dashed", size = 0.25, alpha = 0.5) +
  #geom_text(data = blank, aes(x = as.Date(80, origin = "2018-01-01")), y = limit, label = "WWP High Limit", size = 2, vjust = -0.8) +
  geom_text(data = blank, aes(x = 80), y = limit, label = "WWP High Limit", size = 3, vjust = -0.8) +
  facet_grid(cols = vars(severity), labeller = as_labeller(facet_labels)) +
  #scale_x_date(date_labels = "%b", breaks = "month") +
  theme_bw() +
  scale_x_continuous(breaks = c(60, 91, 121, 152), labels = c("Mar", "Apr", "May", "Jun")) +
  labs(x = "", y = "flow (cms)", caption = "*year is positioned at peak flow, not breakup date") +
  ggtitle("Albany River Near Hat Island (04HA001)") +
  theme(strip.background = element_blank(), legend.position = "none")

select <- highlight(
  ggplotly(flow_plot, tooltip = "year"), 
  selectize = TRUE, persistent = FALSE
)

return(select)

}

####----Attawapiskat Below Muketei Flow----####

att_04FC001_q <- function() {
  
  ####----Gather WISKI Flow and Alarm Data----####
  
  #Get WISKI Flow Data For Attawapisak River Below Muketei
  #download moose river WL from wiski using kiwisR
  sta <- ki_station_list(hub = 'swmc', search_term = "Attawapiskat River below Muketei*")
  
  #get and store time-series information for Moosonee
  ts <- ki_timeseries_list(hub = 'swmc', station_id = sta$station_id)
  
  #Get list of time-series names for WWP limits
  ts.limit <- dplyr::filter(ts, ts_name == "WWP_AlarmLevel_High Limit")
  
  #Get ts name for Q
  ts.q <- dplyr::filter(ts, ts_name == "Q.DayMean") 
  
  #retrieve WWP limits from WISKI
  lst.limit <- ki_timeseries_values(hub = 'swmc', 
                                    ts_id = ts.limit$ts_id,
                                    start_date = as.Date("1900-01-01"), 
                                    end_date = as.Date("1900-01-01"))
  
  #retrieve Q from WISKI
  lst.q <- ki_timeseries_values(hub = 'swmc',
                                ts_id = ts.q$ts_id,
                                start_date = paste0(year(Sys.Date()), "-03-01"),
                                end_date = Sys.Date())
  
  wiski_date <- lst.q %>% 
    filter(Timestamp == max(Timestamp)) %>% 
    mutate(year = year(Timestamp))


  limit <- lst.limit[["Attawapiskat River below Muketei R. (Q)"]][["Q"]]
  
  #lst.limit[which(endsWith(names(lst.limit), '(Q)'))]
  
  ####---Gather WSC Historic Flow----####
  
  #join historic flow with breakup severity
  q_jn <- att_muk_q %>% 
    left_join(att_bdate, by = "year") %>% 
    #filter(doy <= b_doy) %>% 
    filter(max(flow) > 0) 
  
  #find breakup dates that align with max flow
  q_date <- q_jn %>% 
    #filter(doy == b_doy - 1)
    group_by(year) %>% 
    slice(which.max(flow)) %>% 
    ungroup() %>% 
    filter(!(year == "1996"))
  
  ####----Generate Plot----####
  
  #set facet labels
  facet_labels <- c("severe" = "severe", "non.severe" = "non severe")
  
  #create table of WWP limits
  #limits <- data.frame(limit = "high limit", value = c(3000, 5000, 6000),  )
  
  blank = data.frame(type = "high", y = limit)
  
  #create highlight graph
  
  q_jn3 <- SharedData$new(q_jn, ~year, "Highlight a year")
  
  #note: months are manually delineated in some of the flow and lvl graphs to try to 
  #fix a problem with the conversion to plotly but this was not the issue.  using as.Date(doy)...should work as well
  #plot Q: Modified
  flow_plot <- ggplot() + 
    geom_line(data = lst.q, aes(yday(Timestamp), y = Q)) +
    geom_text(data = wiski_date, aes(x = yday(Timestamp), y = Q, label = year), col = "black", size = 2.5, alpha = 1) +
    #geom_line(data = q_jn3, aes(x = as.Date(doy, origin = "2018-01-01"), y = flow, group = year, col = severity), size = 0.25) +
    geom_line(data = q_jn3, aes(x = doy, y = flow, group = year, col = severity), size = 0.25, alpha = 0.75) +
    #geom_text(data = q_date, aes(x = as.Date(doy, origin = "2018-01-01"), y = flow, group = year, label = year), col = "black", size = 2, alpha = 0.75) +
    geom_text(data = q_date, aes(x = doy, y = flow, group = year, label = year), col = "black", size = 2, alpha = 0.75) +
    geom_hline(yintercept = limit, linetype = "dashed", size = 0.25, alpha = 0.5) +
    #geom_text(data = blank, aes(x = as.Date(80, origin = "2018-01-01")), y = limit, label = "WWP High Limit", size = 2, vjust = -0.8) +
    geom_text(data = blank, aes(x = 80), y = limit, label = "WWP High Limit", size = 3, vjust = -0.8) +
    facet_grid(cols = vars(severity), labeller = as_labeller(facet_labels)) +
    #scale_x_date(date_labels = "%b", breaks = "month") +
    theme_bw() +
    scale_x_continuous(breaks = c(60, 91, 121, 152), labels = c("Mar", "Apr", "May", "Jun")) +
    labs(x = "", y = "flow (cms)", caption = "*year is positioned at peak flow, not breakup date") +
    ggtitle("Attawapiskat River Below Muktie (04FC001)") +
    theme(strip.background = element_blank(), legend.position = "none")
  
  select <- highlight(
    ggplotly(flow_plot, tooltip = "year"), 
    selectize = TRUE, persistent = FALSE
  )
  
  return(select)
  
}

####----Moose above Moose Level----####

mse_04LG004_lvl <- function() {
  
  ####----Gather WISKI LVL Data----####
  
  #Get WISKI Flow Data For Moose River above Moose River Crossing
  #download moose river WL from wiski using kiwisR
  sta <- ki_station_list(hub = 'swmc', search_term = "Moose River above Moose River*")
  
  #get and store time-series information for Moosonee
  ts <- ki_timeseries_list(hub = 'swmc', station_id = sta$station_id)
  
  #Get list of time-series names for WWP limits
  ts.limit <- filter(ts, ts_name == "WWP_AlarmLevel_High Limit")
  
  #Get ts name for Q
  ts.q <- filter(ts, ts_name == "LVL.DayMean") 
  
  #retrieve WWP limits from WISKI
  lst.limit <- ki_timeseries_values(hub = 'swmc', 
                                    ts_id = ts.limit$ts_id,
                                    start_date = as.Date("1900-01-01"), 
                                    end_date = as.Date("1900-01-01"))
  
  #retrieve lvl from WISKI
  lst.lvl <- ki_timeseries_values(hub = 'swmc',
                                ts_id = ts.q$ts_id,
                                start_date = paste0(year(Sys.Date()), "-03-01"),
                                end_date = Sys.Date())
  
  wiski_date <- lst.lvl %>% 
    filter(Timestamp == max(Timestamp)) %>% 
    mutate(year = year(Timestamp))

  
  limit <- lst.limit[["Moose River above Moose River (LVL)"]][["LVL"]]
  
  ####----Gather WSC Historic Flow----####
  
  #join historic flow with breakup severity
  q_jn <- mse_crs_lvl %>% 
    left_join(mse_bdate, by = "year") 
    #filter(doy <= b_doy) %>% 
    #filter(max(level) > 0) 
  
  #find breakup dates that align with max flow
  q_date <- q_jn %>% 
    #filter(doy == b_doy - 1)
    group_by(year) %>% 
    slice(which.max(level)) %>% 
    ungroup()
  
  #ungroup for plotly
  q_jn <- q_jn %>% 
    ungroup()
  
  ####----Generate Plot----####
  
  #set facet labels
  facet_labels <- c("severe" = "severe", "non.severe" = "non severe")
  
  #create table of WWP limits
  #limits <- data.frame(limit = "high limit", value = c(3000, 5000, 6000),  )
  
  blank = data.frame(type = "high", y = limit)
  
  q_jn3 <- SharedData$new(q_jn, ~year, "Highlight a year")
  
  #plot Q
  hyd_dat_plot <- ggplot() + 
    geom_line(data = lst.lvl, aes(x = as.Date(yday(Timestamp), origin = "2018-01-01"), y = LVL)) +
    geom_text(data = wiski_date, aes(x = as.Date(yday(Timestamp), origin = "2018-01-01"), y = LVL, label = year), col = "black", size = 2.5, alpha = 1) +
    geom_line(data = q_jn3, aes(x = as.Date(doy, origin = "2018-01-01"), y = level, group = year, col = severity), size = 0.25, alpha = (0.75)) +
    geom_text(data = q_date, aes(x = as.Date(doy, origin = "2018-01-01"), y = level, group = year, label = year), col = "black", size = 2, alpha = 0.75) +
    geom_hline(yintercept = limit, linetype = "dashed", size = 0.25, alpha = 0.5) +
    #geom_hline(yintercept = limit, linetype = "dashed") +
    geom_text(data = blank, aes(x = as.Date(80, origin = "2018-01-01")), y = limit, label = "WWP High Limit", size = 3, vjust = -0.8) +
    #geom_text(data = blank, aes(x = as.Date(80, origin = "2018-01-01")), y = limit, label = "WWP High Limit", size = 2, vjust = -0.8) +
    facet_grid(cols = vars(severity), labeller = as_labeller(facet_labels)) +
    scale_x_date(date_labels = "%b-%d", breaks = "3 weeks") +
    theme_bw() +
    labs(x = "", y = "level (m)", caption = "*year is positioned at peak level, not breakup date") +
    ggtitle("Moosonee above Moose River Crossing (04LG004)") +
    theme(strip.background = element_blank(), legend.position = "none")
  
  select <- highlight(
    ggplotly(hyd_dat_plot, tooltip = "year"), 
    selectize = TRUE, persistent = FALSE
  )
  
  return(select)
  
}

####----Albany River at Hat Island Level----####

alb_04HA001_lvl <- function() {
  
  ####----Gather WISKI LVL and Alarm Data----####
  
  #Get WISKI Flow Data For Attawapisak River Below Muketei
  #download moose river WL from wiski using kiwisR
  sta <- ki_station_list(hub = 'swmc', search_term = "Albany River near Hat Island*")
  
  #get and store time-series information for Moosonee
  ts <- ki_timeseries_list(hub = 'swmc', station_id = sta$station_id)
  
  #Get list of time-series names for WWP limits
  ts.limit <- dplyr::filter(ts, ts_name == "WWP_AlarmLevel_High Limit")
  
  #Get ts name for Q
  ts.q <- dplyr::filter(ts, ts_name == "LVL.DayMean") 
  
  #retrieve WWP limits from WISKI
  lst.limit <- ki_timeseries_values(hub = 'swmc', 
                                    ts_id = ts.limit$ts_id,
                                    start_date = as.Date("1900-01-01"), 
                                    end_date = as.Date("1900-01-01"))
  
  #retrieve LVL from WISKI
  lst.lvl <- ki_timeseries_values(hub = 'swmc',
                                ts_id = ts.q$ts_id,
                                start_date = paste0(year(Sys.Date()), "-03-01"),
                                end_date = Sys.Date())
  wiski_date <- lst.lvl %>% 
    filter(Timestamp == max(Timestamp)) %>% 
    mutate(year = year(Timestamp))

  
  limit <- lst.limit[["Albany River near Hat Island (LVL)"]][["LVL"]]
  
  ####---Gather WSC Historic Flow----####
  
  #join historic flow with breakup severity
  q_jn <- alb_hat_lvl %>% 
    left_join(alb_bdate, by = "year")  
  #filter(doy <= b_doy) %>% 
  #filter(max(flow) > 0) 
  
  #find breakup dates that align with max flow
  q_date <- q_jn %>% 
    #filter(doy == b_doy - 1)
    group_by(year) %>% 
    slice(which.max(level)) %>% 
    ungroup() 
  
  ####----Generate Plot----####
  
  #set facet labels
  facet_labels <- c("severe" = "severe", "non.severe" = "non severe")
  
  #create table of WWP limits
  #limits <- data.frame(limit = "high limit", value = c(3000, 5000, 6000),  )
  
  blank = data.frame(type = "high", y = limit)
  
  #create highlight graph
  
  q_jn3 <- SharedData$new(q_jn, ~year, "Highlight a year")
  
  
  #plot Q: Modified
  level_plot <- ggplot() + 
    geom_line(data = lst.lvl, aes(x = yday(Timestamp), y = LVL)) +
    geom_text(data = wiski_date, aes(x = yday(Timestamp), y = LVL, label = year), col = "black", size = 2.5, alpha = 1) +
    #geom_line(data = q_jn3, aes(x = as.Date(doy, origin = "2018-01-01"), y = flow, group = year, col = severity), size = 0.25) +
    geom_line(data = q_jn3, aes(x = doy, y = level, group = year, col = severity), size = 0.25, alpha = 0.75) +
    #geom_text(data = q_date, aes(x = as.Date(doy, origin = "2018-01-01"), y = flow, group = year, label = year), col = "black", size = 2, alpha = 0.75) +
    geom_text(data = q_date, aes(x = doy, y = level, group = year, label = year), col = "black", size = 2, alpha = 0.75) +
    geom_hline(yintercept = limit, linetype = "dashed", size = 0.25, alpha = 0.5) +
    #geom_text(data = blank, aes(x = as.Date(80, origin = "2018-01-01")), y = limit, label = "WWP High Limit", size = 2, vjust = -0.8) +
    geom_text(data = blank, aes(x = 80), y = limit, label = "WWP High Limit", size = 3, vjust = -0.8) +
    facet_grid(cols = vars(severity), labeller = as_labeller(facet_labels)) +
    #scale_x_date(date_labels = "%b", breaks = "month") +
    theme_bw() +
    scale_x_continuous(breaks = c(60, 91, 121, 152), labels = c("Mar", "Apr", "May", "Jun")) +
    labs(x = "", y = "level (m)", caption = "*year is positioned at peak level, not breakup date") +
    ggtitle("Albany River Near Hat Island (04HA001)") +
    theme(strip.background = element_blank(), legend.position = "none")
  
  select <- highlight(
    ggplotly(level_plot, tooltip = "year"), 
    selectize = TRUE, persistent = FALSE
  )
  
  return(select)
  
}

####----Attawapiskat Below Muketei Level----####

att_04FC001_lvl <- function() {
  
  ####----Gather WISKI LVL and Alarm Data----####
  
  #Get WISKI Flow Data For Attawapisak River Below Muketei
  #download moose river WL from wiski using kiwisR
  sta <- ki_station_list(hub = 'swmc', search_term = "Attawapiskat River below Muketei*")
  
  #get and store time-series information for Moosonee
  ts <- ki_timeseries_list(hub = 'swmc', station_id = sta$station_id)
  
  #Get list of time-series names for WWP limits
  ts.limit <- dplyr::filter(ts, ts_name == "WWP_AlarmLevel_High Limit")
  
  #Get ts name for Q
  ts.q <- dplyr::filter(ts, ts_name == "LVL.DayMean") 
  
  #retrieve WWP limits from WISKI
  lst.limit <- ki_timeseries_values(hub = 'swmc', 
                                    ts_id = ts.limit$ts_id,
                                    start_date = as.Date("1900-01-01"), 
                                    end_date = as.Date("1900-01-01"))
  
  #retrieve Q from WISKI
  lst.lvl <- ki_timeseries_values(hub = 'swmc',
                                ts_id = ts.q$ts_id,
                                start_date = paste0(year(Sys.Date()), "-03-01"),
                                end_date = Sys.Date())
  
  wiski_date <- lst.lvl %>% 
    filter(Timestamp == max(Timestamp)) %>% 
    mutate(year = year(Timestamp))


  limit <- lst.limit[["Attawapiskat River below Muketei R. (LVL)"]][["LVL"]]
  
  ####---Gather WSC Historic Flow----####
  
  #join historic flow with breakup severity
  q_jn <- att_muk_lvl %>% 
    left_join(att_bdate, by = "year") #%>% 
    #filter(doy <= b_doy) %>% 
    #filter(max(level) > 0) 
  
  #find breakup dates that align with max flow
  q_date <- q_jn %>% 
    #filter(doy == b_doy - 1)
    group_by(year) %>% 
    slice(which.max(level)) %>% 
    ungroup() 
  
  ####----Generate Plot----####
  
  #set facet labels
  facet_labels <- c("severe" = "severe", "non.severe" = "non severe")
  
  #create table of WWP limits
  #limits <- data.frame(limit = "high limit", value = c(3000, 5000, 6000),  )
  
  blank = data.frame(type = "high", y = limit)
  
  #create highlight graph
  
  q_jn3 <- SharedData$new(q_jn, ~year, "Highlight a year")
  
  
  #plot LVL: Modified
  level_plot <- ggplot() + 
    geom_line(data = lst.lvl, aes(x = yday(Timestamp), y = LVL)) +
    geom_text(data = wiski_date, aes(x = yday(Timestamp), y = LVL, label = year), col = "black", size = 2.5, alpha = 1) +
    #geom_line(data = q_jn3, aes(x = as.Date(doy, origin = "2018-01-01"), y = flow, group = year, col = severity), size = 0.25) +
    geom_line(data = q_jn3, aes(x = doy, y = level, group = year, col = severity), size = 0.25, alpha = 0.75) +
    #geom_text(data = q_date, aes(x = as.Date(doy, origin = "2018-01-01"), y = flow, group = year, label = year), col = "black", size = 2, alpha = 0.75) +
    geom_text(data = q_date, aes(x = doy, y = level, group = year, label = year), col = "black", size = 2, alpha = 0.75) +
    geom_hline(yintercept = limit, linetype = "dashed", size = 0.25, alpha = 0.5) +
    #geom_text(data = blank, aes(x = as.Date(80, origin = "2018-01-01")), y = limit, label = "WWP High Limit", size = 2, vjust = -0.8) +
    geom_text(data = blank, aes(x = 80), y = limit, label = "WWP High Limit", size = 3, vjust = -0.8) +
    facet_grid(cols = vars(severity), labeller = as_labeller(facet_labels)) +
    #scale_x_date(date_labels = "%b", breaks = "month") +
    theme_bw() +
    scale_x_continuous(breaks = c(60, 91, 121, 152), labels = c("Mar", "Apr", "May", "Jun")) +
    labs(x = "", y = "level (m)", caption = "*year is positioned at peak level, not breakup date") +
    ggtitle("Attawapiskat River Below Muktie (04FC001)") +
    theme(strip.background = element_blank(), legend.position = "none")
  
  select <- highlight(
    ggplotly(level_plot, tooltip = "year"), 
    selectize = TRUE, persistent = FALSE
  )
  
  return(select)
  
}

####----Plot NAEFS Forecast Temp and Precip----####

plot_naefs_forecast <- function(location){
  
  #gather 0 zulu forecast using rforecastca 
  naefs <- rfca_naefs_forecast(
    forecast_time = "00",
    forecast_location = location,
    parameter = c("APCP-SFC", "TMP-SFC")
  )
  
  #calculate percentiles from forecast ensemble
  naefs_stat <- naefs %>% 
    group_by(Parameter, Timestamp) %>% 
    mutate(q90 = quantile(Values, 0.90),
           q66 = quantile(Values, 0.66),
           q50 = quantile(Values, 0.50),
           q44 = quantile(Values, 0.44),
           q10 = quantile(Values, 0.10),
           # add column specifying the horizonal line location
           hline = case_when(Parameter == "APCP-SFC" ~ NA_real_,
                             Parameter == "TMP-SFC" ~ 0))

  #set facet labels
  facet_labels <- c(
    "APCP-SFC" = "Accumulated Precipitation (mm)",
    "TMP-SFC" = "Air Temperature (C)")
  
  #plot forecast temp and precip with median and percentiles of probability  
  naefs_plot <- naefs_stat %>% 
    ggplot() +
    geom_ribbon(aes(x = Timestamp, ymax = q90, ymin = q10), alpha = 0.25) +
    geom_ribbon(aes(x = Timestamp, ymax = q66, ymin = q44), alpha = 0.50) +
    geom_line(aes(x = Timestamp, y = q50), linetype = "dashed") + 
    geom_hline(data = naefs_stat, aes(yintercept = hline), col = "red") +
    facet_grid(rows = vars(Parameter), scales = "free", 
               labeller = as_labeller(facet_labels)) +
    labs(title = paste0("NAEFS Forecast at " , location, " Climate Station"), x = "", y = "") +
    theme_bw() +
    scale_x_datetime(date_labels = "%b-%d", date_breaks = "3 days")
  #ggsave(filename = here::here("plots", "naefs_pcp_tmp_forecast.png"))
  
  naefs_plot <- ggplotly(naefs_plot)
  
  return(naefs_plot)
  
}

####----Generate Map Function----####

####----Gather WSC Level Station Locations----####

create_sta_map <- function() {

  #gather all hydat station metadata
  hy_sta <- hy_stations()
  
  #filter for level stations in ontario
  ON_Q_stn_POR <- hy_stations() %>%
    filter(PROV_TERR_STATE_LOC == "ON") %>%
    pull(STATION_NUMBER) %>%
    hy_stn_data_range() %>%
    filter(DATA_TYPE == "H") %>%
    left_join(hy_sta)
  
  #list stations used in report 
  wsc_report_ls <- c("04LG004", "04HA001", "04FC001")
  
  #join with data range for displaying from-to
  wsc_report <- hy_stations() %>%
    filter(STATION_NUMBER %in% wsc_report_ls) %>% 
    pull(STATION_NUMBER) %>%
    hy_stn_data_range() %>%
    filter(DATA_TYPE == "H") %>%
    left_join(hy_sta)
  
  ####----Gather ECCC Climate Station Locations----####
  
  #get table with all EC climate locations (as of Jan 2018 according to documentation):
  ec_clim_loc <- ec_climate_locations_all
  
  # get stations in Ontario
  ec_clim_loc_on <- ec_clim_loc %>% 
    filter(province == "ONTARIO")
  
  #list ECCC stations
  msc_report_ls <- c("MOOSONEE RCS", "NAGAGAMI", "TIMMINS", "KAPUSKASING CDA")
  
  #filter metadata table for stations
  msc_report <- ec_clim_loc_on %>%
    filter(name %in% msc_report_ls) 
  
  
  #############----Generate Map-----#############
  
  sta_map <- leaflet() %>% 
    addProviderTiles("OpenStreetMap.Mapnik", group = "Map") %>%
    # addProviderTiles("Esri.WorldImagery", group = "Satellite") %>% 
    # addProviderTiles("Esri.WorldShadedRelief", group = "Relief") %>%
    addCircleMarkers(data=wsc_report, ~LONGITUDE,~LATITUDE,
                     color = "blue", radius=5, stroke=T, weight = 10,
                     fillOpacity = 0.2, opacity = 0.5,
                     popup =~ paste0(STATION_NAME, sep = " - ", STATION_NUMBER," (",Year_from, "-", Year_to, "); ", 
                                     "STATUS: ", HYD_STATUS, "; REALTIME: ", REAL_TIME),
                     group = "WSC Level Stations in Report") %>% 
    addCircleMarkers(data=ON_Q_stn_POR, ~LONGITUDE,~LATITUDE,
                     color = "blue", radius=3, stroke=FALSE,
                     fillOpacity = 0.8, opacity = 0.8, 
                     popup =~ paste0(STATION_NAME, sep = " - ", STATION_NUMBER," (",Year_from, "-", Year_to, "); ", 
                                     "STATUS: ", HYD_STATUS, "; REALTIME: ", REAL_TIME),
                     group = "All WSC Level Stations") %>%
    addCircleMarkers(data=msc_report, ~longitude,~latitude,
                     color = "red", radius=5, stroke=T, weight = 10,
                     fillOpacity = 0.2, opacity = 0.5, popup =~ paste0(name, " ",first_year, "-", last_year),
                     group = "ECCC Climate Stations in Report") %>%
    addCircleMarkers(data=ec_clim_loc_on, ~longitude,~latitude,
                     color = "red", radius=3, stroke=FALSE,
                     fillOpacity = 0.8, opacity = 0.8, popup =~ paste0(name, " ",first_year, "-", last_year),
                     group = "All ECCC Climate Stations") %>%
    addLayersControl(
      overlayGroups = c("WSC Level Stations in Report", "All WSC Level Stations", "ECCC Climate Stations in Report", "All ECCC Climate Stations"),
      options = layersControlOptions(collapsed = F))
  
  return(sta_map)

}

