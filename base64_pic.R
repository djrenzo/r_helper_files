data <- rnorm(100, mean=160, sd=8)

hist(data, breaks=5)

# save example plot to file
png("t.png"); plot(0); dev.off()

# Base64-encode file
library(RCurl)
txt <- base64Encode(readBin(tf1, "raw", file.info(tf1)[1, "size"]), "txt")

# Create inline image, save & open html file in browser 
html <- sprintf('<img src="data:image/png;base64,%s">', txt)
cat(html, file = tf2 <- tempfile(fileext = ".html"))
browseURL(tf2)

library(knitr)

printImageURI<-function(file){
  uri=image_uri(file)
  file.remove(file)
  sink("outfile.txt")
  cat(sprintf("![Pic](%s)", uri))
  sink()
}



png("t.png", width = 465, height = 225, units='mm', res = 300)
hist(data, breaks=5, main="Histogram van Lengte Mannen")
mannen <- rnorm(mean=10, n = 1000) 
vrouwen <- rnorm(mean=11, n = 1000) 
hist(mannen, col='blue', main='Bright colors, visible overlap', xlab="Verdeling Mannen & Vrouwen") 
hist(vrouwen, col='red', add=T) 
dev.off()

printImageURI("t.png")
sink("outfile.txt")
cat(printImageURI("t.png"))
sink()

library(ggplot2)
library(tidyr)
df <- data.frame(mannen = rnorm(1000, 10), vrouwen = rnorm(1000, 13)) %>% 
  gather(key, value)

ggplot(df, aes(value, colour = key)) + 
  geom_histogram(aes(y=..density.., fill=key),position="identity", alpha=0.2, binwidth = 0.3, ) +
  geom_density() +
  theme_minimal() 

