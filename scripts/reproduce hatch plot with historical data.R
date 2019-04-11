library(lubridate)
library(here)
library(readxl)
library(weathercan)
library(kiwisR)
library(tidyhydat)
library(conflicted)
library(tidyverse)
library(zoo)

conflict_prefer("filter", "dplyr")
conflict_prefer("here", "here")

####----Preprocess breakup date data-----####

#data import and formatting:
b_date <- read_excel(here::here("import", "breakup_date_ken.xlsx"), 
                     col_types = c("numeric", "date", "text", 
                                   "numeric"))
alb_bdate <- b_date %>%
  mutate(b_doy = yday(date),
         severity = if_else(severity == "1", "severe", "non.severe")) %>%
  filter(river == "albany") %>%
  select(year, severity, b_doy)

#convert severity to a factor
alb_bdate$severity <- as.factor(alb_bdate$severity)
#change factor order for plotting
alb_bdate$severity <- fct_relevel(alb_bdate$severity, "severe","non.severe")

#import Moosonee Data
#may want to define date as date when importing b/c it takes a bit of time to do the conversion
m_ua_day <- read.csv(here::here("import", "moose_dly_clim.csv"))
#cast date column as date
m_ua_day$date <- as.Date(m_ua_day$date)

####----Calculate Snow Depletion and Plot with Severity-----####

#calculate daily snowpack depletion beginning March 1st (with interpolation)
snow_dep <- m_ua_day %>% 
  ungroup() %>%
  select(date, snow_grnd) %>%
  mutate(month = month(date),
         year = year(date),
         doy = yday(date)) %>%
  filter(month >= 3 & month < 6 & year >= 1955) %>%
  group_by(year) %>%
  mutate(snow_grnd_og = snow_grnd,
         snow_grnd = replace(snow_grnd, snow_grnd <= 0, NA),
         snow_grnd = na.approx(snow_grnd, na.rm = F),
         snow_grnd = replace_na(snow_grnd, 0))  %>% 
  #na.omit() %>% 
  arrange(date) %>%
  mutate(dly_diff = c(NA, diff(snow_grnd))) %>%
  na.omit() %>%
  mutate(dly_dep = replace(dly_diff, dly_diff > 0, 0)) %>%
  #na.omit() %>%
  mutate(cum_depl = abs(cumsum(dly_dep)))

#join with Albany breakup severity
snow_dep_jn <- snow_dep %>% 
  left_join(alb_bdate, by = "year") %>% 
#remove years that have 0 cumulative depth depletion
  filter(max(cum_depl) > 0) %>%
  filter(doy <= b_doy)

#find breakup dates that align with max depletion 
b_date_depl <- snow_dep_jn %>% 
  #mutate(doy = if_else(b_doy < doy, b_doy, doy)) %>% 
  filter(doy >= b_doy)
  
#plot cumulative depth depletion 
snow_dep_jn %>%
  #filter(year == "1956") %>% 
  ggplot(aes(x = as.Date(doy, origin = "2018-01-01"), y = cum_depl, group = year, col = severity)) + 
  geom_line(size = 0.25) +
  facet_grid(cols = vars(severity)) +
  geom_text(data = b_date_depl, aes(x = as.Date(b_doy, origin = "2018-01-01"), cum_depl, group = year, label = year), col = "black", size = 3) +
  theme_bw() +
  labs(x = "", y = "cumulative snow depletion (cm)") +
  ggtitle("Moosonee UA Climate Station") +
  theme(strip.background = element_blank(),strip.text.x = element_blank())
ggsave(filename = here::here("plots", "cum_dly_depth_depletion.png"))
           
####-----Snow Depletion Rate Prior to Breakup----####

#calculate snow depletion rate for breakup date - x days
snow_dep_rate <- snow_dep_jn %>% 
  group_by(year, severity) %>% 
  filter(doy <= b_doy) %>%
  select(year, doy, cum_depl, severity) %>% 
  filter(doy == max(doy) | doy == (max(doy) - 14)) %>%
  summarise(dep_rate = (max(cum_depl) - min(cum_depl))/14)

#plot snow depletion rate vs year with severity
ggplot(snow_dep_rate, aes(x = year, y = dep_rate, col = severity)) + 
  geom_point() +
  labs(x = "", y = "snow depth depletion rate 14 days prior to breakup") +
  theme_bw() +
  ggtitle("Moosonee UA Climate Station")
ggsave(filename = here::here("plots", "depth_depletion_rate_14_day.png"))

####----Rain accumulation----####

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

#join with Albany breakup severity and filter values less than breakup date
rain_acc_jn <- rain_acc %>% 
  left_join(alb_bdate, by = "year") %>% 
  filter(doy <= b_doy) %>% 
  filter(max(cum_rain) > 0) 

#find breakup dates that align with max depletion 
b_date_rain_acc <- rain_acc_jn %>% 
  filter(doy >= b_doy)

#plot cumulative rainfall
rain_acc_jn %>%
  #filter(year == "1956") %>% 
  ggplot(aes(x = as.Date(doy, origin = "2018-01-01"), y = cum_rain, group = year, col = severity)) + 
  geom_line(size = 0.25) +
  facet_grid(cols = vars(severity)) +
  geom_text(data = b_date_rain_acc, aes(x = as.Date(b_doy, origin = "2018-01-01"), cum_rain, group = year, label = year), col = "black", size = 3) +
  theme_bw() +
  labs(x = "", y = "cumulative rainfall (mm)") +
  ggtitle("Moosonee UA Climate Station") +
  theme(strip.background = element_blank(),strip.text.x = element_blank())
ggsave(filename = here::here("plots", "cum_rainfall_bdate_severity.png"))

#calculate rainfall rate for breakup date - x days
rain_acc_rate <- rain_acc_jn %>% 
  group_by(year, severity) %>% 
  filter(doy <= b_doy) %>%
  select(year, doy, cum_rain, severity) %>% 
  filter(doy == max(doy) | doy == (max(doy) - 14)) %>%
  summarise(acc_rate = (max(cum_rain) - min(cum_rain))/14)

#plot rain accumulation rate vs year with severity
ggplot(rain_acc_rate, aes(x = year, y = acc_rate, col = severity)) + 
  geom_point() +
  labs(x = "", y = "rain accumulation rate 14 days prior to breakup") +
  theme_bw() +
  ggtitle("Moosonee UA Climate Station")
ggsave(filename = here::here("plots", "rain_accumulation_rate_14_day.png"))

####-----rain accumulation and snow depletion----####

melt <- rain_acc_jn %>% 
  ungroup() %>% 
  select(date, cum_rain, severity) %>% 
  left_join(snow_dep_jn, by = c("date", "severity")) %>% 
  select(date, year, doy, b_doy, cum_rain, cum_depl, severity) %>% 
  mutate(cum_melt = cum_rain + cum_depl)

#find breakup dates that align with max depletion 
b_date_melt <- melt %>% 
  group_by(year) %>% 
  #mutate(doy = if_else(b_doy < doy, b_doy, doy)) %>% 
  filter(doy == max(doy))

#plot cumulative melt
melt %>%
  #filter(year == "1956") %>% 
  ggplot(aes(x = as.Date(doy, origin = "2018-01-01"), y = cum_melt, group = year, col = severity)) + 
  geom_line(size = 0.25) +
  facet_grid(cols = vars(severity)) +
  geom_text(data = b_date_melt, aes(x = as.Date(b_doy, origin = "2018-01-01"), cum_melt, group = year, label = year), col = "black", size = 2.8) +
  theme_bw() +
  geom_segment(aes(y = 150, x = as.Date("2018-04-18"), xend = as.Date("2018-05-15"), yend = 150), col = "black", linetype = "dotted") +
  geom_segment(aes(y = 0, x = as.Date("2018-03-19"), xend = as.Date("2018-04-18"), yend = 150), col = "black", linetype = "dotted") +
  geom_vline(xintercept = as.Date("2018-04-28"), linetype = "dashed") +
  labs(x = "", y = "cumulative rainfall + snowmelt (mm)", 
       title = "Moosonee UA Climate Station", 
       caption = "*mm of water equivalent from snow depth assumes a snow density of 10% for comparison with Hatch tool") +
  theme(strip.background = element_blank(),strip.text.x = element_blank()) 
ggsave(filename = here::here("plots", "cum_melt_bdate_severity.png"))

melt_rnk <- melt %>% 
  ungroup() %>% 
  group_by(doy, severity) %>% 
  summarise(q05 = quantile(cum_melt, 0.05, na.rm = T),
            q50 = quantile(cum_melt, 0.50, na.rm = T),
            q90 = quantile(cum_melt, 0.95, na.rm = T)) %>% 
  gather(variable, value, -severity, -doy)
  

ggplot(melt_rnk, aes(x = as.Date(doy, origin = "2018-01-01"), y = value, group = variable, col = severity)) + 
  geom_line() +
  facet_grid(cols = vars(severity)) +
  theme_bw()

melt %>%
  #filter(year == "1956") %>% 
  ggplot(aes(x = as.Date(doy, origin = "2018-01-01"), y = cum_melt, group = severity, col = severity)) + 
  stat_smooth() +
  #facet_grid(cols = vars(severity)) +
  #geom_text(data = b_date_melt, aes(x = as.Date(b_doy, origin = "2018-01-01"), cum_melt, group = year, label = year), col = "black", size = 2.8) +
  theme_bw() 

  

####------Flow Criteria------#####

#import flow data at hat island and cast date column
hat_q <- read.csv(here::here("import", "alb_hat_q.csv"))
hat_q$Date <- as.Date(hat_q$Date)

#Threshold flow rate of 4750

#-Hydat Data Analysis------------####

#generate annual max flow March-May
hat_dly_max_WF <- hat_q  %>%
  mutate(year = year(Date),
         month = month(Date),
         doy = yday(Date)) %>%
  filter(month >= 3 & month <= 5) %>%
  group_by(year) %>%
  filter(Value == max(Value)) %>%
  distinct(year, .keep_all = T) %>% 
  left_join(alb_bdate, by = "year")

#create df for rectangle df and ggplot object

d = data.frame(xmin = (as.Date(min(hat_dly_max_WF$doy, na.rm = T), origin = "2018-01-01")), 
               xmax = as.Date("2018-04-28"),
               ymin = 4750,
               ymax = max(hat_dly_max_WF$Value, na.rm = T),
               label = "Hatch Tool Flood Zone")

rect <- geom_rect(data = d, aes(xmin = xmin, xmax = xmax,
                                ymin = ymin, ymax = ymax), alpha = 0.25, size = 0) 

rect_txt <- geom_text(data = d, aes(x = xmin + (xmax - xmin)/2, y = ymin + (ymax - ymin)/2, label = label), size = 4) 

#plot daily max flow vs breakup date
ggplot() +
  geom_text(data = hat_dly_max_WF, aes(x = as.Date(doy, origin = "2018-01-01"), y = Value, label = year), size = 2, col = "grey25", hjust = 1, vjust = -1) +
  geom_point(data = hat_dly_max_WF, aes(x = as.Date(doy, origin = "2018-01-01"), y = Value, label = year, col = severity, shape = severity), size = 2) +
  geom_hline(yintercept = 4750, linetype = "dashed") +
  geom_vline(xintercept = as.Date("2018-04-28"), linetype = "dashed") +
  theme(axis.text = element_text(size = 15)) +
  rect +
  rect_txt +
  labs(title = "Albany River at Hat Island (04HA001)", x = "breakup date", y = "max daily flow (cms)") +
  theme_bw()
ggsave(filename = here::here("plots", "hat_dly_max_flow_severity_bu_date.png"))

#plot daily max flow vs max flow date
ggplot() +
  geom_text(data = hat_dly_max_WF, aes(x = as.Date(yday(Date), origin = "2018-01-01"), y = Value, label = year, col = severity), size = 3, position = position_jitter(height = 0.9)) +
  geom_hline(yintercept = 4750, linetype = "dashed") +
  geom_vline(xintercept = as.Date("2018-04-28"), linetype = "dashed") +
  theme(axis.text = element_text(size = 15)) +
  rect +
  rect_txt +
  labs(title = "Albany River at Hat Island (04HA001)", x = "date of max daily flow", y = "max daily flow (cms)") +
  theme_bw()
ggsave(filename = here::here("plots", "hat_dly_max_flow_severity_doy.png"))

#add max flow confusion matrix classification column
hat_dly_max_WF_cls  <- hat_dly_max_WF %>% 
  as.tibble() %>% 
  mutate(class = case_when(
    b_doy < 118 & Value > 4750 & severity == "severe" ~ "true positive",
    !(b_doy < 118 & Value > 4750) & severity == "non.severe" ~ "true negative",
    b_doy < 118 & Value > 4750 & severity == "non.severe" ~ "false positive",
    !(b_doy < 118 & Value > 4750) & severity == "severe" ~ "false negative")) %>%
  select(-Symbol) %>% 
  na.omit() 

#set class as factor
hat_dly_max_WF_cls$class <- as_factor(hat_dly_max_WF_cls$class)

summary(hat_dly_max_WF_cls)

#plot daily max flow vs breakup date with conf matrix classification
ggplot() +
  #geom_text(data = hat_dly_max_WF, aes(x = as.Date(doy, origin = "2018-01-01"), y = Value, label = year, col = severity), size = 3, position = position_jitter(height = 0.9)) +
  geom_point(data = hat_dly_max_WF_cls, aes(x = as.Date(b_doy, origin = "2018-01-01"), y = Value, col = severity, shape = class), size = 2.5) +
  geom_hline(yintercept = 4750, linetype = "dashed") +
  geom_vline(xintercept = as.Date("2018-04-28"), linetype = "dashed") +
  theme(axis.text = element_text(size = 15)) +
  rect +
  rect_txt +
  labs(title = "Albany River at Hat Island (04HA001)", x = "breakup date", y = "max daily flow (cms)") +
  theme_bw() +
  scale_shape_manual(values = c(15, 16, 17, 8))
ggsave(filename = here::here("plots", "hat_dly_max_flow_severity_bu_date_class.png"))

#generate monthly flow summaries from hydat data (not necessary for Hatch tool Analysis)
hat_WF <- alb_hat_q %>%
  mutate(snow_year = if_else(month(Date) > 9, year(Date) + 1, year(Date)),
         month = month(Date, label = T, abbr = T)) %>% 
  group_by(snow_year, month) %>%
  summarise(avgWF = mean(Value, na.rm = T),
            maxWF = max(Value, na.rm = T),
            minWF = min(Value, na.rm = T)) %>%
  filter(month %in% c("Jan", "Mar", "Apr", "Sep","Nov")) %>%
  gather(key = variable, value = value, -snow_year, -month) %>%
  unite(temp, month, variable) %>%
  spread(temp, value) %>%
  rename(year = snow_year)

#3 day rate of increase in flow of >= 700 m3/day

#create a vector toaccount for missing values in the lag calculation
x <- numeric()
x[1:2] <- NA

#calculate rolling rate of change
roll_rate <- alb_hat_q %>% 
  ungroup() %>%
  select(Date, Value) %>%
  mutate(month = month(Date),
         year = year(Date),
         doy = yday(Date)) %>%
  filter(month >= 3 & month < 6 & year >= 1955) %>%
  group_by(year) %>%
  mutate(diff_3_day = c(x, diff(Value, lag = 2)),
         inc_3_day = replace(diff_3_day, diff_3_day < 0, 0),
         rate_inc_3 = round(inc_3_day/3, 2))

#calculate rolling rate of change -does not work b/c rollapply uses all values between lag
# roll_rate2 <- alb_hat_q %>% 
#   ungroup() %>%
#   select(Date, Value) %>%
#   mutate(month = month(Date),
#          year = year(Date),
#          doy = yday(Date)) %>%
#   filter(month >= 3 & month < 6 & year >= 1955) %>%
#   group_by(year) %>%
#   mutate(diff_3_day = c(x, rollapply(Value, 3, mean)))

#join with breakup severity
roll_rate_jn <- left_join(hat_bdate, roll_rate, by = "year")

#plot all 3 day increases
ggplot(roll_rate_jn, aes(x = Date, y = rate_inc_3, col = severity)) +
  geom_point() +
  geom_hline(yintercept = 700, linetype = "dashed") +
  geom_hline(yintercept = 700, linetype = "dashed") +
  labs(x = "", y = "3 day rate of flow increase (cms)", 
       title = "Albany River at Hat Island (04HA001)" ) +
  theme_bw()
ggsave(filename = here::here("plots", "hat_3_day_increase_severity_year_all.png"))

#calculate max 3 day increase
max_roll_rate <- roll_rate_jn %>%
  group_by(year) %>% 
  filter(rate_inc_3 == max(rate_inc_3, na.rm = T)) %>% 
  filter(rate_inc_3 > 50)

#plot max 3 day increases by year
ggplot(max_roll_rate, aes(x = Date, y = rate_inc_3, label = year, col = severity)) +
  geom_text(size = 3) +
  geom_hline(yintercept = 700, linetype = "dashed") +
  labs(x = "", y = "max annual 3 day rate of flow increase (cms)", 
       title = "Albany River at Hat Island (04HA001)" ) +
  theme_bw()
ggsave(filename = here::here("plots", "hat_3_day_increase_severity_year.png"))

#plot max 3 day increases by doy
max_roll_rate %>%
  #filter(year != "2012") %>% 
  ggplot(aes(x = as.Date(doy.x, origin = "2018-01-01"), y = rate_inc_3, label = year,col = severity)) +
    geom_text(size = 3) +
    geom_hline(yintercept = 700, linetype = "dashed") +
    labs(x = "breakup date", y = "max annual 3 day rate of flow increase (cms)", 
         title = "Albany River at Hat Island (04HA001)" ) +
    theme_bw()
ggsave(filename = here::here("plots", "hat_3_day_increase_severity_doy.png"))

#check with 1 day lag - [mimic Cochrane Flow Report]
#not much difference b/w the patterns in 1 day and 3 day rate of rise
#need to change variable names and figure labels if proceeding beyond a simple check
#may want to create a function to create these figures and generate them for a number of lags for comparison

#------------

# x <- numeric()
# x[1:1] <- NA
#   
# #calculate rolling rate of change
# roll_rate <- alb_hat_q %>% 
#   ungroup() %>%
#   select(Date, Value) %>%
#   mutate(month = month(Date),
#          year = year(Date),
#          doy = yday(Date)) %>%
#   filter(month >= 3 & month < 6 & year >= 1955) %>%
#   group_by(year) %>%
#   mutate(diff_3_day = c(x, diff(Value)),
#          inc_3_day = replace(diff_3_day, diff_3_day < 0, 0),
#          rate_inc_3 = round(inc_3_day/2, 2))
# 
# #join with breakup severity
# roll_rate_jn <- left_join(hat_bdate, roll_rate, by = "year")
# 
# #plot all 3 day increases
# ggplot(roll_rate_jn, aes(x = Date, y = rate_inc_3, col = severity)) +
#   geom_point() +
#   geom_hline(yintercept = 700, linetype = "dashed") +
#   geom_hline(yintercept = 700, linetype = "dashed") +
#   labs(x = "", y = "3 day rate of flow increase (cms)", 
#        title = "Albany River at Hat Island (04HA001)" ) +
#   theme_bw()
# ggsave(filename = here::here("plots", "hat_3_day_increase_severity_year_all.png"))
# 
# #calculate max 3 day increase
# max_roll_rate <- roll_rate_jn %>%
#   group_by(year) %>% 
#   filter(rate_inc_3 == max(rate_inc_3, na.rm = T)) %>% 
#   filter(rate_inc_3 > 50)
# 
# #plot max 3 day increases by year
# ggplot(max_roll_rate, aes(x = Date, y = rate_inc_3, label = year, col = severity)) +
#   geom_text(size = 3) +
#   geom_hline(yintercept = 700, linetype = "dashed") +
#   labs(x = "", y = "max annual 3 day rate of flow increase (cms)", 
#        title = "Albany River at Hat Island (04HA001)" ) +
#   theme_bw()
# ggsave(filename = here::here("plots", "hat_3_day_increase_severity_year.png"))
# 
# #plot max 3 day increases by doy
# max_roll_rate %>%
#   #filter(year != "2012") %>% 
#   ggplot(aes(x = as.Date(doy.x, origin = "2018-01-01"), y = rate_inc_3, label = year,col = severity)) +
#   geom_text(size = 3) +
#   geom_hline(yintercept = 700, linetype = "dashed") +
#   labs(x = "breakup date", y = "max annual 3 day rate of flow increase (cms)", 
#        title = "Albany River at Hat Island (04HA001)" ) +
#   theme_bw()
# ggsave(filename = here::here("plots", "hat_3_day_increase_severity_doy.png"))




