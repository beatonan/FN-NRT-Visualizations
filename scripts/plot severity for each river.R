library(tidyverse)
library(here)
library(readxl)
library(lubridate)

#data import and formatting:
b_date <- read_excel(here::here("import", "breakup_date_ken.xlsx"), 
                     col_types = c("numeric", "date", "text", 
                                   "numeric"))
alb_bdate <- b_date %>%
  mutate(b_doy = yday(date),
         severity = if_else(severity == "1", "severe", "non-severe")) %>%
  filter(river == "albany") %>%
  select(year, severity, b_doy)

#convert severity to a factor
alb_bdate$severity <- as.factor(alb_bdate$severity)
#change factor order for plotting
alb_bdate$severity <- fct_relevel(alb_bdate$severity,"non-severe", "severe")

#create plot
ggplot(alb_bdate, aes(x = year, y = severity, col = severity, shape = severity)) + 
  geom_point() + 
  scale_x_continuous(breaks = c(1950, 1960, 1970, 1980, 1990, 2000, 2010)) +
  theme_bw() +
  labs(title = "Albany Breakup Severity") +
  scale_color_manual(values = c("#00BFC4", "#F8766D")) +
  scale_shape_manual(values = c(17, 16))
ggsave(here::here("plots", "albany breakup severity.png"), height = 4, width = 11)
