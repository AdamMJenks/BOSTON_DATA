### Assumptions that are going to be used:
# 300 watt solar panel has an average of 20.67 square feet.
# 
# Cost is $4.20 cost per watt for MA
# (http://news.energysage.com/how-much-does-the-average-solar-panel-installation-cost-in-the-u-s/)
# 
# 30% ITC federal credit
# (http://www.seia.org/policy/finance-tax/solar-investment-tax-credit)
# 
# 
# On average we can only use 66% of the top of a building for misc. things like walk ways, ladders, electrical systems.  
# 
# BTU to kWH conversion is 1 BTU = 0.00029307107017 kWH
#
# Assume that of potential sunlight hours, only 75% of them are usable due to weather and other variations.

library(dplyr)
# Read and format DFs -----------------------------------------------------

load('data/2016_energy_parsed')

Energy_Parsed_Df$`Total Site Energy (kBTU)` <- as.numeric(Energy_Parsed_Df$`Total Site Energy (kBTU)`)
 
Energy_Parsed_Df <-  Energy_Parsed_Df %>%
                      mutate(Available_sqft_for_panels = sqft_available * 0.66,
                             Number_of_300watt_Panels = Available_sqft_for_panels / 20.67,
                             Kwh_potential = ((Number_of_300watt_Panels * 300)/1000) * sunlight_hours * 0.75,
                             Total_Site_Energy_Kwh = (`Total Site Energy (kBTU)` * 1000) * 0.00029307107017,
                             Total_Site_Energy_Kwh_Electricity = Total_Site_Energy_Kwh * `% Electricity`,
                             Cost_of_installation_gross = Number_of_300watt_Panels * 300 * 4.20,
                             Surplus_energy_production_possible = Kwh_potential - Total_Site_Energy_Kwh_Electricity)

Energy_Surplus_per_property_type <- Energy_Parsed_Df %>% 
                                        filter(!is.na(Surplus_energy_production_possible)) %>%
                                        group_by(`Property Type`) %>%
                                        summarise(Surplus_energy_production_possible = sum(Kwh_potential - Total_Site_Energy_Kwh_Electricity),
                                                  Total_cost = sum(Cost_of_installation_gross)) %>%
                                        mutate(Exceeds_all_of_boston_by = Surplus_energy_production_possible - sum(Energy_Parsed_Df$Total_Site_Energy_Kwh_Electricity, na.rm = T))


Utility_data_raw <- read.csv('data/city-of-boston-utility-data-2011-2015.csv')
