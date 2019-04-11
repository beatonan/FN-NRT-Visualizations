library(lubridate)
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

#remotes::install_github("rwhale/rforecastca")

conflict_prefer("filter", "dplyr")
conflict_prefer("here", "here")


####----Preprocess breakup date data-----####

#data import and formatting:
b_date <- read_excel(here::here("import", "breakup_date_ken.xlsx"), 
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

#Get WISKI Flow Data For Moose River above Moose River Crossing

#download moose river WL from wiski using kiwisR
sta <- ki_station_list(hub = 'swmc', search_term = "Moose River above Moose River*")

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
  slice(which.max(flow)) %>% 
  ungroup()

#set facet labels
facet_labels <- c("severe" = "severe", "non.severe" = "non severe")

#create table of WWP limits
#limits <- data.frame(limit = "high limit", value = c(3000, 5000, 6000),  )

blank = data.frame(type = "high", y = limit)

#plot Q: Original
flow_plot <- ggplot() + 
  #geom_line(data = lst.q, aes(x = as.Date(Timestamp, origin = "2018-01-01"), y = Q)) +
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


####---Highlight Plot----####

plot(flow_plot)

d <- highlight_key(q_jn, ~year)

#plot Q: highlight_key
flow_plot <- ggplot() + 
  #geom_line(data = lst.q, aes(x = as.Date(Timestamp, origin = "2018-01-01"), y = Q)) +
  geom_line(data = d, aes(x = as.Date(doy, origin = "2018-01-01"), y = flow, group = year, col = severity), size = 0.25) +
  geom_text(data = q_date, aes(x = as.Date(doy, origin = "2018-01-01"), y = flow, group = year, label = year), col = "black", size = 2, alpha = 0.75) +
  geom_hline(yintercept = limit, linetype = "dashed") +
  geom_text(data = blank, aes(x = as.Date(80, origin = "2018-01-01")), y = limit, label = "WWP High Limit", size = 2, vjust = -0.8) +
  facet_grid(cols = vars(severity), labeller = as_labeller(facet_labels)) +
  scale_x_date(date_labels = "%b-%d", breaks = "3 weeks") +
  theme_bw() +
  labs(x = "", y = "flow (cms)", caption = "*year is positioned at peak flow, not breakup date") +
  ggtitle("Moosonee above Moose River Crossing (04LG004)") +
  theme(strip.background = element_blank(), legend.position = "none")

#convert to plotly object
flow_plotly <- ggplotly(flow_plot, tooltip = "year")

#create highlight graph:

plot_highlight <- highlight(flow_plotly)

plot_highlight

####----Filter/Select Figure----####

#create filter graph

q_jn2 <- SharedData$new(q_jn)

#plot Q: Modified
flow_plot <- ggplot() + 
  #geom_line(data = lst.q, aes(x = as.Date(Timestamp, origin = "2018-01-01"), y = Q)) +
  
  #geom_line(data = q_jn2, aes(x = as.Date(doy, origin = "2018-01-01"), y = flow, group = year, col = severity), size = 0.25) +
  geom_line(data = q_jn2, aes(x = doy, y = flow, group = year, col = severity), size = 0.25) +
  
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
  ggtitle("Moosonee above Moose River Crossing (04LG004)") +
  theme(strip.background = element_blank(), legend.position = "none")

flow_plot

filter_plotly <- ggplotly(flow_plot)

filter_plotly

filter <- bscols(
  filter_select("id", "Select a single year", q_jn2, ~year),
  #filter_checkbox("id", "Select a single year", q_jn2, ~year, inline = T),
  filter_plotly,
  widths = c(12, 12)
)

#create highlight graph

q_jn3 <- SharedData$new(q_jn, ~year, "Highlight a year")


#plot Q: Modified
flow_plot <- ggplot() + 
  #geom_line(data = lst.q, aes(x = as.Date(Timestamp, origin = "2018-01-01"), y = Q)) +
  
  #geom_line(data = q_jn3, aes(x = as.Date(doy, origin = "2018-01-01"), y = flow, group = year, col = severity), size = 0.25) +
  geom_line(data = q_jn3, aes(x = doy, y = flow, group = year, col = severity), size = 0.25) +
  
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
  ggtitle("Moosonee above Moose River Crossing (04LG004)") +
  theme(strip.background = element_blank(), legend.position = "none")

select <- highlight(
  ggplotly(flow_plot, tooltip = "year"), 
  selectize = TRUE, persistent = FALSE
)

bscols(filter, select)
