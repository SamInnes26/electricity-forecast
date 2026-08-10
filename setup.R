required_packages <- c(
  # "shiny",
  # "bslib",
  "ggplot2",
  "dplyr",
  # "DT",
  "jsonlite",
  "httr",
  "lubridate",
  "httr2",
  # "rsconnect",
  "tidyverse"
)

missing_packages <- required_packages[
  !(required_packages %in% installed.packages()[,"Package"])
]

if(length(missing_packages) > 0){
  install.packages(missing_packages)
}
