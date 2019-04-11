#--------------moose river conditions functions-----------------#

####----Setup----####
#set path
path <- "C:/Users/beatonan/OneDrive - Government of Ontario/Documents/My Documents/R/FN_Hydro_FY18_19/"

#get utils functions
source(paste0(path, "scripts/river conditions report utils.R"))

####----Data Import and formatting----####

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

#import moose flow
moose_flow <- read.csv(here::here("import", "moose_flow"))
#convert to date
moose_flow$Date <- as.Date(moose_flow$Date)

moose_flow <- moose_flow %>%
  select(date = Date, flow = Value) %>%
  mutate(month = month(date),
         year = year(date),
         doy = yday(date)) %>%
  filter(month >= 3 & month < 6 & year >= 1955)

####----FDD Plot----####

fdd_plot <- function() {

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
gg_fdd <- ggplot() + 
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

gg_fdd

}

####----Snow Depth Plot----####

sd_plot <- function () {

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
gg_sd <- ggplot() + 
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

gg_sd

}

####----PDD Plot----####

pdd_plot <- function() {

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
  gg_pdd <- ggplot() + 
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
  
  gg_pdd
  
}

####----Cumulative Rain Plot----####

cum_rain_plot <- function() {

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
gg_cum_rain <- ggplot() + 
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

gg_cum_rain

}

####----Snow Depth Depletion----####

depl_plot <- function() {

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
  gg_sdepl <- ggplot() + 
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
  
  gg_sdepl

}

####----Cumulative Rain + Snow Depth Depletion----####

cum_rain_depl_plot <- function() {
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
  
  #interpolate missing SDepth Values
  clim_ls$SDepth <- na.approx(clim_ls$SDepth, rule = 2)
  
  #calculate daily cumulative sd depletion from WISKI data
  wiski_sdepl <- clim_ls %>%
    mutate(Timestamp = as.Date(Timestamp, format = "%Y-%m-%d")) %>% 
    group_by(Timestamp) %>% 
    summarise(SDepth = mean(SDepth, na.rm = T)) %>%
    mutate(depl = diff(c(SDepth[1],SDepth)),
           depl_neg = if_else(depl < 0, abs(depl), 0),
           cum_depl = cumsum(depl_neg)) %>% 
    na.omit() %>% 
    select(Timestamp, cum_depl)
  
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
  
  ####----Cumulative Rain----####
  
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
                                  start_date = as.Date("2018-03-01"), 
                                  end_date = as.Date("2018-05-01"))
  
  #change to following lines when running during breakup 
  #start_date = "2019-03-01", 
  #end_date = Sys.Date())
  
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
  
  ####----Combine depth depletion and cumulative rainfall----####
  
  #combine msc data
  msc_jn <- msc_rain_acc %>% 
    left_join(msc_sdepl, by = c("year", "date")) %>% 
    #mutate(cum_rain = replace_na(cum_rain, 0),
    #cum_depl = replace_na(cum_depl, 0)) %>% 
    mutate(cum_rain_sd  = cum_rain + (cum_depl*10),
           doy = yday(date)) %>% 
    left_join(mse_bdate, by = "year") %>% 
    filter(doy <= b_doy) %>% 
    #remove outliers that are causing plotting issues
    filter(!(year %in% c(1998, 2005))) %>%
    filter(!(doy < 74 & cum_depl > 1000)) %>% 
    filter(!(date == "1997-05-01")) 
  
  
  #combine wiski data
  wiski_jn <- wiski_rain_acc %>% 
    left_join(wiski_sdepl, by = "Timestamp") %>% 
    mutate(cum_rain_sd  = cum_rain + (cum_depl*10)) 
  
  
  #interpolate missing SDepth Values
  msc_jn$cum_rain_sd <- na.approx(msc_jn$cum_rain_sd, rule = 2)
  
  #find breakup dates that align with max bdate
  cum_date <- msc_jn %>% 
    group_by(year) %>% 
    mutate(doy = yday(date)) %>% 
    filter(doy == b_doy - 1) # -1 so that 1997 will show up because its last day was removed as it was causing issues
  #filter(doy == b_doy - 1)
  #filter(snow_grnd == max(snow_grnd))
  
  #set facet labels
  facet_labels <- c("severe" = "severe", "non.severe" = "non severe")
  
  #plot cumulative rainfall
  gg_cum_rain_sdepl <- ggplot() + 
    geom_line(data = msc_jn, aes(x = as.Date(yday(date), origin = "2018-01-01"), y = cum_rain_sd, group = year, col = severity), size = 0.25, alpha = (0.25)) +
    geom_text(data = cum_date, aes(x = as.Date(b_doy, origin = "2018-01-01"), cum_rain_sd, group = year, label = year), col = "black", size = 2, alpha = 0.75) +
    geom_line(data = wiski_jn, aes(x = as.Date(yday(Timestamp), origin = "2018-01-01"), y = cum_rain_sd)) +
    facet_grid(cols = vars(severity), labeller = as_labeller(facet_labels)) +
    scale_x_date(date_labels = "%b-%d", breaks = "2 weeks") +
    theme_bw() +
    labs(x = "", y = "cumulative rainfall + snow depth depletion (mm)") +
    ggtitle("Moosonee UA Climate Station") +
    theme(strip.background = element_blank(), legend.position = "none")
  #ggsave(filename = here::here("plots", "mse_cum_rainfall_sd_bdate_severity.png"))
  
  gg_cum_rain_sdepl
  
}

####----Flow----####

flow_plot <- function() {
  
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
                                start_date = as.Date("2018-03-01"), 
                                end_date = as.Date("2018-05-01"))
  #end_date = Sys.Date())
  
  limit <- lst.limit[[2]]$Q
  
  
  #join historic flow with breakup severity
  q_jn <- moose_flow %>% 
    left_join(mse_bdate, by = "year") %>% 
    #filter(doy <= b_doy) %>% 
    filter(max(flow) > 0) 
  
  #find breakup dates that align with max flow
  q_date <- q_jn %>% 
    #filter(doy == b_doy - 1)
    group_by(year) %>% 
    slice(which.max(flow))
  
  #set facet labels
  facet_labels <- c("severe" = "severe", "non.severe" = "non severe")
  
  #create table of WWP limits
  #limits <- data.frame(limit = "high limit", value = c(3000, 5000, 6000),  )
  
  blank = data.frame(type = "high", y = limit)
  
  #plot Q
  gg_flow <- ggplot() + 
    geom_line(data = lst.q, aes(x = as.Date(Timestamp, origin = "2018-01-01"), y = Q)) +
    geom_line(data = q_jn, aes(x = as.Date(doy, origin = "2018-01-01"), y = flow, group = year, col = severity), size = 0.25, alpha = (0.25)) +
    geom_text(data = q_date, aes(x = as.Date(doy, origin = "2018-01-01"), y = flow, group = year, label = year), col = "black", size = 2, alpha = 0.75) +
    geom_hline(yintercept = limit, linetype = "dashed") +
    geom_text(data = blank, aes(x = as.Date(80, origin = "2018-01-01")), y = limit, label = "WWP High Limit", size = 2, vjust = -0.8) +
    facet_grid(cols = vars(severity), labeller = as_labeller(facet_labels)) +
    scale_x_date(date_labels = "%b-%d", breaks = "3 weeks") +
    theme_bw() +
    labs(x = "", y = "flow (cms)", caption = "*year is positioned at peak flow, not breakup date") +
    ggtitle("Moosonee above Moose River Crossing (04LG004)") +
    theme(strip.background = element_blank(), legend.position = "none")
  #ggsave(filename = here::here("plots", "mse_q_bdate_severity.png"))
  
  gg_flow
  
}


