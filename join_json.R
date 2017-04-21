library(readxl)
library(ggmap)
library(httr)
library(XML)
library(jsonlite)

b <- read_excel(path="data/2016-reported-energy-and-water-metrics.xlsx", sheet=1, skip=4)

load("data/sunroof.Rdata")

## assume $100/month electric bill
savings_100 <- do.call('rbind', lapply(json_list, function(x) lapply(x['HouseInfoResponse'], function(y) y[[3]][[12]][[2]][[1]][[6]][[9]])))
sunlight_hours <- do.call('rbind', lapply(json_list, function(x) lapply(x['HouseInfoResponse'], function(y) y[8][[1]][[6]])))
sqft_available <- do.call('rbind', lapply(json_list, function(x) lapply(x['HouseInfoResponse'], function(y) y[8][[1]][[7]])))

b$savings_100<-as.numeric(as.character(savings_100))
b$has_solar <- FALSE
b[which(!is.na(b$`Onsite Solar (kWh)`)),]$has_solar <- TRUE
b$sunlight_hours <- sunlight_hours
b$sqft_available <- sqft_available
