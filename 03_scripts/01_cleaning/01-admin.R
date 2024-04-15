
# GPI data ----------------------------------------------------------------

gpidata <- rio::import("02_data/processed/Econ-costing-data-2023.xlsx") %>% 
  rename(iso3c=geocode, indicator = element) %>% 
  mutate (indicator = case_when (indicator == "Terrorism deaths" ~ "killed",
  indicator == "Military expenditure % GDP" ~ "military expenditure (% gdp)",
  indicator == "Fear % population" ~ "perceptions of criminality",
  indicator == "Homicides per 100,000" ~ "homicide rate",
  indicator == "Refugees and IDPs" ~ "refugees and idps",
  indicator == "Incarceration rate per 100,000" ~ "incarceration rate",
  indicator == "Peacekeeping" ~ "un peacekeeping funding",
  TRUE ~ indicator))

# Filter to get only GPI countries from WDI -------------------------------------------

pos <- unique(gpidata$iso3c)


# update for 2022
# GPI grid ----------------------------------------------------------------

gpi.country <- read_csv("02_data/processed/gpi.country.csv")
#gpi.grid <- expand.grid(year=c(2007:2021), iso3c=unique(gpi.country$iso3c))
gpi.grid <- expand.grid(year=c(2008:2022), iso3c=unique(gpi.country$iso3c))
gpi.grid <- subset(gpi.grid,!(iso3c=="PSE" & year<2014))
gpi.grid <- subset(gpi.grid,!(iso3c=="SSD" & year<2009))

# function to get data from WEO -------------------------------------------

# weovarselect <- function(data,subject, unit ){
#   data <- subset(data, Subject.Descriptor==subject & Units==unit)
#   data <- data[,c(2,4,10:52)]
#   data <- gather(data, year, value,-c(iso3c, country))
#   
#   data[,'value'] <- as.numeric(as.character(gsub(",","", data[,'value'])))
#   
#   data[,'year']<- as.numeric(as.character(gsub("X","", data[,'year'])))
#   
#   data <- subset(data, year>2006 & year<2018)
#   
#   data <- data[data$iso3c %in% pos, ]
#   
#   return(data)
# }


# see if any missing

# mis <- function(data){
#   pos[which(!(pos %in% data$iso3c ))] 
# }



# table to check missing data ---------------------------------------------

tab <- function(data){
  t <- with(data, table(iso3c, is.na(value)))
  View(t)
}

# lubridate::floor_date(lubridate::today(), "year") - lubridate::days(1)   # if you want to manually set max year for report each year; e.g., 2022 for this year's report



################ Region and Peace level ####################
# update for new GPI data
#based off score

#Peace_and_region <- read_excel("Data/Peace and region.xlsx", 
Peace_and_region <- read_excel("02_data/processed/Peace and region.xlsx",                               
                               col_types = c("text", "text", "numeric"))

Peace_and_region <- Peace_and_region %>%  rename(`GPI overall score`=`2020 Peace level`)
#Peace_and_region <- Peace_and_region %>%  rename(`GPI overall score`=`2021 Peace level`)

Peace_and_region$peace_level = ifelse(Peace_and_region$`GPI overall score` <= 1.45, "Very High Peace", 
                                      ifelse(Peace_and_region$`GPI overall score` >= 1.45 & Peace_and_region$`GPI overall score` <= 1.9, "High Peace", 
                                             ifelse(Peace_and_region$`GPI overall score` >= 1.9 & Peace_and_region$`GPI overall score` <= 2.4, "Medium Peace",
                                                    ifelse(Peace_and_region$`GPI overall score` >= 2.4 & Peace_and_region$`GPI overall score` <= 2.9, "Low Peace",
                                                           ifelse(Peace_and_region$`GPI overall score` >= 2.9, "Very Low Peace", NA)))))
Peace_and_region <- Peace_and_region %>% select(-`GPI overall score`)



######################################################## POPULATION  ############################################################

pop <- gpidata %>% dplyr::filter (indicator == "population") %>% rename (population = value) %>% select (-indicator)

pop <- pop %>% mutate (variablename = "population") %>% rename(value = population, geocode = iso3c)

pop <- f_index_data_pad(pop)

pop %<>% select (c(1, 2, 5)) %>% rename (iso3c = geocode, population = imputed)



pop <- gpi.grid %>% left_join(pop)

