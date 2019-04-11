#calculate ice on/off dates from WSC

library(tidyverse)
library(tidyhydat)
library(here)

#create df of hydat stations
hy_sta <- hy_stations() 

#download daily data for WSC station of interest
#Albany River at Hat Island (04HA001)
# ATTAWAPISKAT RIVER BELOW MUKETEI RIVER(04FC001)

wsc <-  hy_daily_flows("04HA001")


#generate annual ice summaries from hydat data
ice_stat <- wsc %>%
  filter(Symbol == "B") %>%
  mutate(snow_year = if_else(month(Date) > 9, year(Date) + 1, year(Date)),
         month = month(Date)) %>%
  group_by(snow_year) %>%
  summarise(ice_on = yday(min(Date)),
            ice_off = yday(max(Date))) %>%
  mutate(ice_on = replace(ice_on, ice_on < 250, NA),
         ice_off = replace(ice_off, ice_off > 175, NA),
         total_ice = 365 - ice_on + ice_off) %>%
  rename(year = snow_year)

#plot ice data
#boxplot of breakup date at Moose
ice_stat %>%
  na.omit() %>%
  gather(variable, value) %>%
  filter(variable == "ice_off") %>%
  #ggplot(aes(x = as.factor(variable), y = as.Date(value, origin = "2018-01-01"))) +
  ggplot(aes(x = as.factor(variable), y = as.numeric(value))) +
  #facet_grid(vars = cols(variable), scales = "free")+
  geom_boxplot(outlier.alpha = 0)+
  geom_jitter()+
  #coord_flip()+
  labs(y = "day of year", x = "")+
  coord_flip()+
  theme_bw()

#import breakup data and join with ice stat data

b_date <- read_excel(here("import", "breakup_date_ken.xlsx"), 
                     col_types = c("numeric", "date", "text", 
                                   "numeric"))

#recode severity and filter by river
b_date <- b_date %>%
  mutate(doy = yday(date),
         severity = if_else(severity == "1", "severe", "non.severe")) %>%
  filter(river == "albany")

#convert severity to a factor
b_date$severity <- as.factor(b_date$severity)
#change factor order for plotting
b_date$severity <- fct_relevel(b_date$severity, "severe","non.severe")

#join with ice_stat
ice_stat_bu <- b_date %>% 
  left_join(ice_stat, by = "year")

#write csv
write.csv(ice_stat_bu, here::here("import", "ice_stat_04HA001.csv"), row.names = F)
