library(readxl)
library(ggmap)
library(httr)
library(XML)

b <- read_excel(path="data/2016-reported-energy-and-water-metrics.xlsx", sheet=1, skip=4)

get_geocoded_address <- function(address){
  g<-geocode(address, output='all')
  if(g$status == 'ZERO_RESULTS'){
    return('No Match')
  } else {
    return(g$results[[1]])
  }
  
}

the_addresses <- paste(b$Address, b$ZIP)

geocode_info <- list()
for(i in 1:nrow(b)){
  print(i)
  geocode_info[[i]] <- get_geocoded_address(the_addresses[i])
}


lats <- lapply(geocode_info, function(x) lapply(x['geometry'], function(y) lapply(y['location'], function(z) z['lat'])))
lngs <- lapply(geocode_info, function(x) lapply(x['geometry'], function(y) lapply(y['location'], function(z) z['lng'])))
lats <- as.numeric(unlist(lats))
lngs <- as.numeric(unlist(lngs))

df_locations<-data.frame(lat=lats, lng=lngs)

json_list <- list()
for(j in 1:nrow(df_locations)){
  print(j)
  the_url <- paste0("https://www.google.com/async/sclp?ei=yw_5WMn3DYuxarPyoMgK&yv=2&async=lat:", df_locations[j,]$lat,",lng:", df_locations[j,]$lng, ",pf:se,_fmt:jspb")
  r <- GET(the_url)
  the_data<-fromJSON(gsub("\\)]}'\n", "", content(r, type="text")))
  json_list[[j]] <- the_data
}

## assume $100/month electric bill
savings_100 <- do.call('rbind', lapply(json_list, function(x) lapply(x['HouseInfoResponse'], function(y) y[[3]][[12]][[2]][[1]][[6]][[9]])))
sunlight_hours <- do.call('rbind', lapply(json_list, function(x) lapply(x['HouseInfoResponse'], function(y) y[8][[1]][[6]])))
sqft_available <- do.call('rbind', lapply(json_list, function(x) lapply(x['HouseInfoResponse'], function(y) y[8][[1]][[7]])))

b$savings_100<-as.numeric(as.character(savings_100))
b$has_solar <- FALSE
b[which(!is.na(b$`Onsite Solar (kWh)`)),]$has_solar <- TRUE
b$sunlight_hours <- sunlight_hours
b$sqft_available <- sqft_available
# 
# b$formatted_address <- unlist(formatted_addresses)
# base_url <- "https://www.google.com/get/sunroof#a="
# params <- "&b=100&f=lease&np=21&p=1"
# 
# the_url <- paste0(base_url, gsub("\\s+", "%20", formatted_addresses[[1]]), params)
# #the_url <- paste0(base_url)
# r <- GET(the_url, query = list(a = formatted_addresses[[1]], b = 100, f='lease', np=21, p=1))
# r_content <- content(r)
