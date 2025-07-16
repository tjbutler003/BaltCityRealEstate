# Use Pacman to install/load packages from one function
if(!require(pacman)){
  install.packages("pacman")
}

pacman::p_load("tidyverse","ZillowR","tmap","tmaptools","sf","censusapi",
               "tidycensus","ZillowR","tigris")

baltCityZillowHomePrices <-
  read_csv("../real_estate_data/zillow_data/Metro_median_sale_price_uc_sfr_month.csv") %>% 
  filter(stringr::str_detect(RegionName,"Baltimore")) %>% 
  select(-c(RegionID:StateName)) %>% 
  pivot_longer(cols = `2008-02-29`:`2025-04-30`,names_to = "month", 
               values_to = "Home_Price") %>% 
  mutate(month = ymd(month))

ggplot(baltCityZillowHomePrices, aes(x = month, y = Home_Price)) +
  geom_line()





View(baltCityZillowHomePrices)


rentByZipcode <- 
  read_csv("../real_estate_data/zillow_data/Zip_zori_uc_sfrcondomfr_sm_month.csv") %>% 
  filter(CountyName == "Baltimore City") %>% 
  select(RegionName,`2015-01-31`:`2025-05-31`) %>% 
  mutate(percentChange22to25 = 
           (round(`2025-05-31`/`2022-05-31`, digits = 2) - 1) * 100) %>% 
  pivot_longer(cols = `2015-01-31`:`2025-05-31`, names_to = "Month", 
               values_to = "RentPrice") %>% 
  mutate(Month = ymd(Month),
         RegionName = factor(RegionName) %>% 
           fct_reorder(RentPrice, .fun = max, .na_rm = T))

ggplot(rentByZipcode, aes(x = Month, y = RentPrice, color = RegionName)) +
  geom_line()

rentPercentChange <- rentByZipcode %>% 
  filter(Month == "2025-05-31" & !is.na(percentChange22to25)) %>% 
  mutate(RegionName = RegionName %>% 
           fct_reorder(percentChange22to25, .fun = min))

ggplot(rentPercentChange,
       aes(x = RentPrice, y = percentChange22to25)) +
  geom_text(aes(label = RegionName), 
            position = position_jitter())


zillowNeighborhoodValueIndex <-
  read_csv("zillow_data/Neighborhood_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")

View(zillowNeighborhoodValueIndex)

zillowNeighborhoodValueIndexLong <- zillowNeighborhoodValueIndex %>% 
  select(RegionName,`2000-01-31`:`2025-05-31`) %>% 
  mutate(percentChange10to25 = 
           (round(`2025-05-31`/`2010-05-31`, digits = 2) - 1) * 100) %>% 
  pivot_longer(`2000-01-31`:`2025-05-31`, names_to = "Month", 
               values_to = "ZillowHomeValue")


affordabilityIndexMortgage <- 
  read_sf("Affordability_Index_-_Mortgage_-_Community_Statistical_Area_map_data/Affordability_Index_-_Mortgage_-_Community_Statistical_Area.shp")

affordabilityIndexRent <- 
  read_csv("Affordability_Index_-_Rent_-_Community_Statistical_Area.csv")

medianIncome <-
  read_csv("Median_Household_Income_csa.csv")

affordabilityIndex <- 
  merge(affordabilityIndexMortgage,
        affordabilityIndexRent) %>% 
  merge(., medianIncome)

mortgageMap <-
  ggplot(affordabilityIndex, aes(fill = affordm22)) +
  geom_sf()

rentMap <-
  ggplot(affordabilityIndex, aes(fill = affordr22)) +
  geom_sf()

medianIncomeMap <-
  ggplot(affordabilityIndex, aes(fill = mhhi22)) +
  geom_sf()

mortgageMap
rentMap
medianIncomeMap

