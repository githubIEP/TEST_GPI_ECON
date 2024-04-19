# 
# 
#  gti.sum <- rio::import ("02_data/processed/terrorism_tracker_data_FINAL.xlsx", sheet = "cleaned-condensed-tt-data")
# 
#  gti.sum <- gti.sum %>% select (geocode, year, deaths_total, injured_total)
# 
#  gti.sum <- gti.sum %>% group_by (geocode, year) %>% summarize (killed = sum(deaths_total), wounded = sum (injured_total)) %>%
#                         rename (iso3c = geocode)
# 
#  gti.sum <- gpi.grid %>% left_join(gti.sum)
# 
#  gti.sum <- gti.sum %>% mutate (killed = case_when (is.na(killed) ~ 0,
#                                                     TRUE ~ killed)) %>%
#                  mutate (wounded = case_when (is.na(wounded) ~ 0,
#                                                    TRUE ~ wounded))
# 
# 
#  
 
 # ===============================================================================================================
 
 
 gpidata <- readRDS("02_data/processed/GPI_2024_EconomicCosting.rds")  
 
 
 gti.sum <- gpidata %>%
   dplyr::select(c(`geocode`, `year`, `terrorism_deaths`)) %>%
   rename(iso3c = geocode) %>%
   rename(killed = terrorism_deaths)
 
 
 gti.sum <- gpi.grid %>% left_join(gti.sum)
 
 gti.sum <- gti.sum %>% mutate (killed = case_when (is.na(killed) ~ 0,
                                                    TRUE ~ killed))
 
 