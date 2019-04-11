
sta <- ki_station_list(hub = 'swmc')

sta <- sta %>% 
  filter(grepl("WNZ", station_no))

sta <- sta %>% 
  filter(grepl("MOOSONEE", station_name))

#get and store time-series information for Moosonee
ts <- ki_timeseries_list(hub = 'swmc', station_id = sta$station_id)

sta <- sta %>% 
  filter(grepl("XKA", station_no))

# look at Nagagami (WNZ), Timmins (YTS) and Kapuskasing (XKA) Climate stations

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