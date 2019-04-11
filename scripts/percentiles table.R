library(here)
library(DT)
library(tidyverse)

#import Moose feature data
mse <- read.csv(here::here("import", "moose_features.csv"))

#function for determining percentile of each value within distribution
get_p <- function(x) {round(ecdf(x)(x), 2)*100}

#run fucntion for each feature using mutate
mse_perc <- mse %>% 
  mutate_at(vars(doy:total_ice), get_p)

#extract column names for formatting
colname <- colnames(mse_perc)
colname <- colname[-(1:3)]

#create datatable
#additional formating code available here: 
#(https://stackoverflow.com/questions/30742767/conditional-formatting-cell-in-datatable-in-r)

datatable(mse_perc, filter = "top", rownames = FALSE) %>% 
  formatStyle(
    colname,
    background = styleColorBar(mse_perc$doy, 'red'),
    backgroundSize = '100% 90%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center'
  ) %>% 
  formatRound(colname, digits = 0)
