library(readxl)
library(ggmap)
library(httr)
library(XML)

b <- read_excel(path="data/2016-reported-energy-and-water-metrics.xlsx", sheet=1, skip=4)

get_formatted_address <- function(address){
  g<-geocode(address, output='all')
  return(g$results[[1]]$formatted_address)
  
}

the_addresses <- paste(b$Address, b$ZIP)

formatted_addresses <- list()
for(i in 1:nrow(b)){
  formatted_addresses[[i]] <- get_formatted_address(the_addresses[i])
}





base_url <- "https://www.google.com/get/sunroof#a="
the_url <- paste0(base_url, paste0(gsub("\\s+", "%20", b$Address[1]), '%20', b$ZIP[1]))
