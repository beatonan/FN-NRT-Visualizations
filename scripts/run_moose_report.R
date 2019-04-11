#run Moose River Report

library(here)
library(rmarkdown)

#rmarkdown::render(here::here("scripts", "moose river conditions report.Rmd"))

path <- "C:/Users/beatonan/OneDrive - Government of Ontario/Documents/My Documents/R/FN_Hydro_FY18_19/"

Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/bin/pandoc")

rmarkdown::render(
  input = paste0(path, "scripts/", "moose river conditions report.Rmd"),
  #output_dir = path,
  output_file = paste0("Moose_Report_", Sys.Date(), ".pdf")
)