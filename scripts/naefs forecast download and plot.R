#load libraries
library(rforecastca)
library(tidyverse)

####----Gather NAEFS Forecasts----####

#view available parameters output by the naefs model
#params <- rfca_naefs_parameters()

#view available forecast locations output by the naefs model
all_locations <- rfca_cityweather_locations()

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

#plot forecast precip  
naefs_stat %>% 
  filter(Parameter == "APCP-SFC") %>%
  ggplot() +
  geom_ribbon(aes(x = Timestamp, ymax = q90, ymin = q10), alpha = 0.25) +
  geom_ribbon(aes(x = Timestamp, ymax = q66, ymin = q44), alpha = 0.50) +
  geom_line(aes(x = Timestamp, y = q50), linetype = "dashed") + 
  labs(title = "Forecast Precipitation at Moosonee UA Climate Station", x = "", y = "cumulative precipitation (mm)") +
  theme_bw()

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

return(naefs_plot)

}




  