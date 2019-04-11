#non function wsc imports

#Moose above Moose River Crossing flow
moose_flow <- read.csv(here::here("import", "moose_flow"))
#convert to date
moose_flow$Date <- as.Date(moose_flow$Date)

moose_flow <- moose_flow %>%
  select(date = Date, flow = Value) %>%
  mutate(month = month(date),
         year = year(date),
         doy = yday(date)) %>%
  filter(month >= 3 & month < 6 & year >= 1955)

#Attawapiskat below Muketei Flow

#import att below muk flow
att_muk_q <- read.csv(here::here("import", "att_muk_q.csv"))
#convert to date
att_muk_q$Date <- as.Date(att_muk_q$Date)

att_muk_q <- att_muk_q %>%
  select(date = Date, flow = Value) %>%
  mutate(month = month(date),
         year = year(date),
         doy = yday(date)) %>%
  filter(month >= 3 & month < 7 & year >= 1955)

#Albany Flow

#import alb hat island below 
alb_hat_q <- read.csv(here::here("import", "alb_hat_q.csv"))
#convert to date
alb_hat_q$Date <- as.Date(alb_hat_q$Date)

alb_hat_q <- alb_hat_q %>%
  select(date = Date, flow = Value) %>%
  mutate(month = month(date),
         year = year(date),
         doy = yday(date)) %>%
  filter(month >= 3 & month < 7 & year >= 1955)

##--LVL Data--##

#alb_hat_q <- import_wsc("alb_hat_q.csv", parameter == flow)