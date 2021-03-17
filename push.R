if(!("lubridate" %in% rownames(installed.packages()))){
  install.packages("lubridate")
}
  

require("lubridate")

push <- function(msg=toString(lubridate::now())) {
  #setwd("~/timeseries")
  system("git add --all")
  system(paste0("git commit -m '", msg, "'"))
  system("git push")
}
