library(lubridate)
library(here)
library(tidyhydat)
library(tidyverse)
library(readxl)

#reproduction of Figure 4 in Hatch CRIPE paper (http://cripe.ca/docs/proceedings/17/Abdelnour-2013.pdf)

b_date <- read_excel(here::here("import", "breakup_date_ken.xlsx"), 
                     col_types = c("numeric", "date", "text", 
                                   "numeric"))
hat_bdate <- b_date %>%
  mutate(doy = yday(date),
         severity = if_else(severity == "1", "severe", "non.severe")) %>%
  filter(river == "albany") %>%
  select(year, doy, severity)

#convert severity to a factor
hat_bdate$severity <- as.factor(hat_bdate$severity)
#change factor order for plotting
hat_bdate$severity <- fct_relevel(hat_bdate$severity, "severe","non.severe")

#-Hydat Data Analysis------------####

#import moose flow
alb_hat_q <- read.csv(here::here("import", "alb_hat_q.csv"))
#convert to date
alb_hat_q$Date <- as.Date(alb_hat_q$Date)

#generate monthly flow summaries from hydat data
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

#generate annual max flow March-May
hat_dly_max_WF <- alb_hat_q  %>%
  mutate(year = year(Date),
         month = month(Date)) %>%
  filter(month >= 3 & month <= 5) %>%
  group_by(year) %>%
  filter(Value == max(Value)) %>%
  distinct(year, .keep_all = T)

#join with severity data
hat_dly_max_WF <- left_join(hat_bdate, hat_dly_max_WF, by = "year")

#create df for rectangle df and ggplot object

d = data.frame(xmin = (as.Date(min(hat_dly_max_WF$doy), origin = "2018-01-01")), 
             xmax = as.Date("2018-04-28"),
             ymin = 4750,
             ymax = max(hat_dly_max_WF$Value, na.rm = T),
             label = "Hatch Tool Flood Zone")

rect <- geom_rect(data = d, aes(xmin = xmin, xmax = xmax,
                                ymin = ymin, ymax = ymax), alpha = 0.25, size = 0) 

rect_txt <- geom_text(data = d, aes(x = xmin + (xmax - xmin)/2, y = ymin + (ymax - ymin)/2, label = label), size = 4) 

#plot daily max flow vs breakup date
ggplot() +
  geom_text(data = hat_dly_max_WF, aes(x = as.Date(doy, origin = "2018-01-01"), y = Value, label = year, col = severity), size = 3, position = position_jitter(height = 0.9)) +
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




