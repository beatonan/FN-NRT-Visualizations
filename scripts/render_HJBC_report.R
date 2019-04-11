library(rmarkdown)

#set path
path <- "C:/Users/beatonan/OneDrive - Government of Ontario/Documents/My Documents/R/FN_Hydro_FY18_19/scripts/"

# Rmarkdown file
#hjbc_rmd <- paste0(path, "hjbc_report.rmd")
rmd_in <- paste0(path, "hjbc_report.Rmd")

output_name <- paste0(Sys.Date(), "_HJBC_report.html")


rmarkdown::render(
  input = rmd_in,
  output_format = "html_document",
  output_file = output_name
)
