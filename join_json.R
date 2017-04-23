library(readxl)
library(ggmap)
library(httr)
library(XML)
library(jsonlite)

b <- read_excel(path="data/2016-reported-energy-and-water-metrics.xlsx", sheet=1, skip=4)

load("data/sunroof.Rdata")

## assume $100/month electric bill

no_data <- which(unlist(lapply(json_list, function(x) length(x[[1]]))) != 8)
json_list_no_na <- json_list[-no_data]

savings_100 <- do.call('rbind', lapply(json_list_no_na, function(x) lapply(x['HouseInfoResponse'], function(y) lapply(y[3], function(z) z[12][[1]][[2]][[1]][[6]][[9]]))))
#savings_100 <- do.call('rbind', lapply(json_list, function(x) lapply(x['HouseInfoResponse'], function(y) y[3][[12]][[2]][[1]][[6]][[9]])))
sqft_available <- do.call('rbind', lapply(json_list_no_na, function(x) lapply(x['HouseInfoResponse'], function(y) lapply(y[8], function(z) z[6]))))
sunlight_hours <- do.call('rbind', lapply(json_list_no_na, function(x) lapply(x['HouseInfoResponse'], function(y) lapply(y[8], function(z) z[7]))))

b <- b[-no_data,]
b$savings_100<-as.numeric(as.character(lapply(savings_100[,1], function(x) x[[1]])))
b$has_solar <- FALSE
b[which(!is.na(b$`Onsite Solar (kWh)`)),]$has_solar <- TRUE
b$sunlight_hours <- as.numeric(unlist(sunlight_hours))
b$sqft_available <- as.numeric(unlist(sqft_available))

Energy_Parsed_Df <- b

save.image('data/2016_energy_parsed')

