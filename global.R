library(dplyr)
# Read and format DFs -----------------------------------------------------

load('data/2016_energy_parsed')
load('data/geocode.Rdata')

load("data/geocode_land.Rdata")
land_parcels <- read.csv("data/land_parcels.csv")

load("data/Boston_shape.RData")
load("data/sunroof_land.Rdata")
load("data/2016_land_parsed")
#Boston_shape <- geojsonio::geojson_read("Data/Municipal_Building_Energy_Reporting_BERDO.geojson", what = "sp")

geocode_info <- geocode_info[-no_data]
lats <- lapply(geocode_info, function(x) lapply(x['geometry'], function(y) lapply(y['location'], function(z) z['lat'])))
lngs <- lapply(geocode_info, function(x) lapply(x['geometry'], function(y) lapply(y['location'], function(z) z['lng'])))
lats <- as.numeric(unlist(lats))
lngs <- as.numeric(unlist(lngs))

df_locations<-data.frame(lat=lats, lng=lngs)

geocode_info_land <- geocode_info_land[-no_data_land]
lats2 <- lapply(geocode_info_land, function(x) lapply(x['geometry'], function(y) lapply(y['location'], function(z) z['lat'])))
lngs2 <- lapply(geocode_info_land, function(x) lapply(x['geometry'], function(y) lapply(y['location'], function(z) z['lng'])))
lats2 <- as.numeric(unlist(lats2))
lngs2 <- as.numeric(unlist(lngs2))

df_locations_land<-data.frame(lat=lats2, lng=lngs2)
land_parsed_df$lat <- lats2
land_parsed_df$lng <- lngs2

Energy_Parsed_Df[which(Energy_Parsed_Df$`Year Built` == '889'),]$`Year Built` <- '1889'
Energy_Parsed_Df[which(Energy_Parsed_Df$`Year Built` == '1000'),]$`Year Built`  <- '2000'
Energy_Parsed_Df$`Total Site Energy (kBTU)` <- as.numeric(Energy_Parsed_Df$`Total Site Energy (kBTU)`)
Energy_Parsed_Df$lat <- lats
Energy_Parsed_Df$lng <- lngs
Energy_Parsed_Df$year_built <- as.numeric(Energy_Parsed_Df$`Year Built`)

Energy_Parsed_Df <-  Energy_Parsed_Df %>%
    mutate(Available_sqft_for_panels = sqft_available * 0.66,
                     Number_of_300watt_Panels = Available_sqft_for_panels / 20.67,
                     Kwh_potential = ((Number_of_300watt_Panels * 300)/1000) * sunlight_hours * 0.75,
                     Total_Site_Energy_Kwh = (`Total Site Energy (kBTU)` * 1000) * 0.00029307107017,
                     Total_Site_Energy_Kwh_Electricity = Total_Site_Energy_Kwh * `% Electricity`,
                     Cost_of_installation_gross = Number_of_300watt_Panels * 300 * 4.20,
                     Surplus_energy_production_possible = Kwh_potential - Total_Site_Energy_Kwh_Electricity) %>%
  rename(Property_Name = `Property Name`)

Total_energy_city_of_boston <<- sum(Energy_Parsed_Df$Total_Site_Energy_Kwh_Electricity, na.rm = T)

 
Energy_Surplus_per_property_type <- Energy_Parsed_Df %>% 
  filter(!is.na(Surplus_energy_production_possible)) %>%
  group_by(`Property Type`) %>%
  summarise(Surplus_energy_production_possible = sum(Kwh_potential - Total_Site_Energy_Kwh_Electricity),
            Total_cost = sum(Cost_of_installation_gross)) %>%
  mutate(Exceeds_all_of_boston_by = Surplus_energy_production_possible - sum(Energy_Parsed_Df$Total_Site_Energy_Kwh_Electricity, na.rm = T))

land_parsed_df <-  land_parsed_df %>%
  mutate(Available_sqft_for_panels = sqft_available * 0.66,
         Number_of_300watt_Panels = Available_sqft_for_panels / 20.67,
         Kwh_potential = ((Number_of_300watt_Panels * 300)/1000) * sunlight_hours * 0.75,
         Cost_of_installation_gross = Number_of_300watt_Panels * 300 * 4.20)