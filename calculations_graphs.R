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

library(dplyr)
# Read and format DFs -----------------------------------------------------

load('data/2016_energy_parsed')

Energy_Parsed_Df$`Total Site Energy (kBTU)` <- as.numeric(Energy_Parsed_Df$`Total Site Energy (kBTU)`)
 
Energy_Parsed_Df <-  Energy_Parsed_Df %>%
                      mutate(Available_sqft_for_panels = sqft_available * 0.66,
                             Number_of_300watt_Panels = Available_sqft_for_panels/ 20.67,
                             Kwh_potential = Number_of_300watt_Panels * 300 * sunlight_hours,
                             Total_Site_Energy_Kwh = (`Total Site Energy (kBTU)` * 1000) * 0.0002930710717,
                             Total_Site_Energy_Kwh_Electricity = Total_Site_Energy_Kwh * `% Electricity`)
                             
