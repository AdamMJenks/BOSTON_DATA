library(dplyr)
# Read and format DFs -----------------------------------------------------

load('data/2016_energy_parsed')
load('data/geocode.Rdata')

load("data/geocode_land.Rdata")
land_parcels <- read.csv("data/land_parcels.csv")

geocode_info <- geocode_info[-no_data]
lats <- lapply(geocode_info, function(x) lapply(x['geometry'], function(y) lapply(y['location'], function(z) z['lat'])))
lngs <- lapply(geocode_info, function(x) lapply(x['geometry'], function(y) lapply(y['location'], function(z) z['lng'])))
lats <- as.numeric(unlist(lats))
lngs <- as.numeric(unlist(lngs))

df_locations<-data.frame(lat=lats, lng=lngs)

lats2 <- lapply(geocode_info_land, function(x) lapply(x['geometry'], function(y) lapply(y['location'], function(z) z['lat'])))
lngs2 <- lapply(geocode_info_land, function(x) lapply(x['geometry'], function(y) lapply(y['location'], function(z) z['lng'])))
lats2 <- as.numeric(unlist(lats2))
lngs2 <- as.numeric(unlist(lngs2))

df_locations_land<-data.frame(lat=lats2, lng=lngs2)
land_parcels$lat <- lats2
land_parcels$lng <- lngs2

Energy_Parsed_Df[which(Energy_Parsed_Df$`Year Built` == '889'),]$`Year Built` <- '1889'
Energy_Parsed_Df[which(Energy_Parsed_Df$`Year Built` == '1000'),]$`Year Built`  <- '2000'
Energy_Parsed_Df$`Total Site Energy (kBTU)` <- as.numeric(Energy_Parsed_Df$`Total Site Energy (kBTU)`)
Energy_Parsed_Df$lat <- lats
Energy_Parsed_Df$lng <- lngs
Energy_Parsed_Df$year_built <- as.numeric(Energy_Parsed_Df$`Year Built`)

Energy_Surplus_per_property_type <- Energy_Parsed_Df %>% 
  filter(!is.na(Surplus_energy_production_possible)) %>%
  group_by(`Property Type`) %>%
  summarise(Surplus_energy_production_possible = sum(Kwh_potential - Total_Site_Energy_Kwh_Electricity),
            Total_cost = sum(Cost_of_installation_gross)) %>%
  mutate(Exceeds_all_of_boston_by = Surplus_energy_production_possible - sum(Energy_Parsed_Df$Total_Site_Energy_Kwh_Electricity, na.rm = T))
