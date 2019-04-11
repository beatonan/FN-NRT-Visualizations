#run Moose River Report

library(here)
library(rmarkdown)

#rmarkdown::render(here::here("scripts", "moose river conditions report.Rmd"))

#path <- "C:/Users/beatonan/OneDrive - Government of Ontario/Documents/My Documents/R/FN_Hydro_FY18_19/"
#setwd("C:/Users/beatonan")

Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/bin/pandoc")

rmarkdown::render(
  input = here::here("scripts", "test_here.Rmd"),
  output_dir = here::here("plots"),
  output_file = paste0("test_here_", Sys.Date(), ".pdf")
)

# rmarkdown::render(
#   input = paste0(path, "scripts/", "test_here.Rmd"),
#   output_dir = paste0(path, "plots/"),
#   output_file = paste0("test_here_", Sys.Date(), ".pdf")
# )