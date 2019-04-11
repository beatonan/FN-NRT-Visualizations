library(lubridate)
library(tidyhydat)
library(tidyverse)
library(here)
library(readxl)
library(conflicted)

conflict_prefer("filter", "dplyr")

#create vector of flow stations for assessment (a station from each major tributary)
#ABITIBI RIVER AT ONAKAWANA - 04ME003 (1959-2017)
#MOOSE RIVER ABOVE MOOSE RIVER - 04LG004 (1983-2017)
#MISSINAIBI RIVER BELOW WABOOSE RIVER - 04LM001 (1972-2017)
#MATTAGAMI RIVER AT SMOKY FALLS - 04LG001 (1926-1963)
#MATTAGAMI RIVER AT LITTLE LONG RAPIDS - 04LG003 (1963-1994)

#Ken indicated that 
#1976, 1985, 2006 and 2013 (see OPG Report, Abitibi Flow graph and photo) 
#may have been a result of high inflows to these sites and subsequent release of water

sta_nums <- c("04ME003", "04LG004", "04LM001", "04LG001", "04LG003")


#import and save daily flow data for Mooose at Moose River Crossing
ms_trib_flow <- hy_daily_flows(sta_nums)

#save as csv
write.csv(ms_trib_flow, here::here("import", "ms_trib_flow.csv"), row.names=FALSE)

#import data from csv
ms_trib_flow <- read.csv(here::here("import", "ms_trib_flow.csv"))

#cast variable types
ms_trib_flow$STATION_NUMBER <- as.factor(ms_trib_flow$STATION_NUMBER) 
ms_trib_flow$Date <- as.Date(ms_trib_flow$Date) 

#rename factors
ms_trib_flow$STATION_NUMBER <- fct_recode(ms_trib_flow$STATION_NUMBER, 
           Abitbi_04ME003 = "04ME003", 
           Moose_04LG004 = "04LG004", 
           Missinaibi_04LM001 = "04LM001", 
           Mattagami_04LG001 = "04LG001", 
           Mattagami_04LG003 = "04LG003")

#plot flow daily ts faceted by station 
ms_trib_flow %>%
  group_by(STATION_NUMBER) %>%
    ggplot(aes(x = Date, y = Value))+ 
    geom_line() +
    facet_grid(rows = vars(STATION_NUMBER))
#save plot
ggsave(filename = here::here("plots", "moose_tribs_dly_flow.png"), height = 8, width = 14)

#generate annual max flow March-May
ms_trb_dly_max <- ms_trib_flow %>%
  mutate(year = year(Date),
         month = month(Date)) %>%
  filter(month >=3 & month <=5) %>%
  group_by(year, STATION_NUMBER) %>%
  filter(Value == max(Value)) %>%
  distinct(year, STATION_NUMBER, .keep_all = T) 

b_date <- read_excel(here::here("import", "breakup_date_ken.xlsx"), 
                     col_types = c("numeric", "date", "text", 
                                   "numeric"))
b_date <- b_date %>%
  mutate(doy = yday(date),
         severity = if_else(severity == "1", "severe", "non.severe")) %>%
  filter(river == "moose")

#convert severity to a factor
b_date$severity <- as.factor(b_date$severity)
#change factor order for plotting
b_date$severity <- fct_relevel(b_date$severity, "severe","non.severe")

#join with severity data
ms_trb_jn <- b_date %>%
  right_join(ms_trb_dly_max, by = "year") %>%
  filter(year >= 1950)

#plot doy vs daily max flow 
ms_trb_jn %>%
  filter(STATION_NUMBER != "Mattagami_04LG001") %>%
  ggplot(aes(x = as.Date(yday(Date), origin = "2018-01-01"), y = Value, label = year, col = severity)) +
  #geom_point() +
  geom_text(size = 3, position = position_jitter(height = 0.9))+
  theme(axis.text = element_text(size = 15))+
  labs(x = "date of maximum daily flow", y = "maximum daily flow (Mar-May) (cms)") +
  #ggtitle("Moose River above Moose River Crossing") +
  facet_grid(rows = vars(STATION_NUMBER), scales = "free")+
  theme_bw()
#save plot
ggsave(filename = here::here("plots", "moose_tribs_dly_max_flow_severity.png"))

#plot flow time-series for each year with severity

ms_trib_flow %>%
  mutate(year = year(Date),
         doy = yday(Date),
         month = month(Date),
         y_date = as.Date(doy, origin = "2018-01-01")) %>%
  left_join(b_date, by = "year") %>%
  filter(year > 1985) %>%
  filter(STATION_NUMBER == "Moose_04LG004") %>%
  filter(month >= 4 & month <= 5) %>%
  ggplot(aes(x = y_date, y = Value, group = year, col = severity))+ 
  #geom_point()+ 
  geom_line(alpha = 0.5)+
  #facet_grid(cols = vars(severity))+
  theme_bw()
#save plot
ggsave(filename = here::here("plots", "moose_dly_flow_severity_col.png"))
ggsave(filename = here::here("plots", "moose_dly_flow_severity_fac.png"))
         
  
