push <- function(msg=toString(now())) {
  library(lubridate)
  #setwd("~/timeseries")
  system("git add --all")
  system(paste0("git commit -m '", msg, "'"))
  system("git push")
}
