##### ----- GPI ECON COSTING STANDARD TABLES
#' This script is for producing all the standard GPI Econ Costing Tables
##### -----


### --- Libraries, Variables, Functions
library(knitr)
library(openxlsx)
library(reshape2)
library(iepg)
library(dplyr)
library(scales)

## -- Spreadsheet to save charts in
wb_Tables <- createWorkbook()

### --- List of Standard Tables

# Change in the global economic impact of violence, billions of PPP 2022 US dollars, 2021–2022

TABLE_ImpactChange = c(title = "Change in the global economic impact of violence, billions of PPP 2022 US dollars, 2021–2022",
                            sheet = "ImpactChange", source = "IEP Calculations", xtext = "", ytext = "",
                            type = "Table", position = "Normal")

# Change in the global economic impact of violence, billions of PPP 2022 US dollars, 2008–2022

TABLE_ImpactChangeTrend = c(title = "Change in the global economic impact of violence, billions of PPP 2022 US dollars, 2008–2022",
              sheet = "ImpactChangeTrend", source = "IEP Calculations", xtext = "", ytext = "",
              type = "Table", position = "Normal")

# Military expenditure: total, per capita, and as percentage of GDP, 2022

TABLE_MEx = c(title = "Military expenditure: total, per capita, and as percentage of GDP, 2022",
                       sheet = "MEx", source = "IEP Calculations", xtext = "", ytext = "",
                       type = "Table", position = "Normal")

# The ten countries with the highest economic cost of violence, percentage of GDP
TABLE_TenCountries = c(title = "The Ten Countries with the Highest Economic Cost of Violence, Percentage of GDP",
                     sheet = "TenCountries", source = "IEP Calculations", xtext = "", ytext = "",
                     type = "Table", position = "Normal")


### --- Loading Data

econ_impact.df <- rio::import("04_outputs/Economic Impact of Violence.xlsx")

## -- TABLE_ImpactChange -----------------------------------------------------------
# A table showing the change in the global 
# economic impact of violence, billions of PPP 2022 US dollars, 2021–2022

tab_Change <- econ_impact.df %>%
  dplyr::filter(year==max(year), subtype=="costusd") %>%
  pivot_wider(names_from = type, values_from = value) %>%
  replace(is.na(.), 0) %>%
  group_by(indicator2, year) %>%
  summarise(direct = sum(direct)/1000000000, direct = round(direct, 0),
            indirect = sum(indirect)/1000000000, indirect = round(indirect, 0)) %>%
  mutate(multiplier = direct, total_impact = sum(direct, indirect, multiplier)) %>%
  select(-year)


tab_Change_2021 <- econ_impact.df %>%
  dplyr::filter(year==max(year-1), subtype=="costusd") %>%
  pivot_wider(names_from = type, values_from = value) %>%
  replace(is.na(.), 0) %>%
  group_by(indicator2, year) %>%
  summarise(direct = sum(direct)/1000000000, direct = round(direct, 0),
            indirect = sum(indirect)/1000000000, indirect = round(indirect, 0)) %>%
  mutate(multiplier = direct, total_impact_2021 = sum(direct, indirect, multiplier)) %>%
  select(indicator2, total_impact_2021)

tab_Change <- tab_Change %>%
  left_join(tab_Change_2021, by = "indicator2") %>%
  mutate(total_change = total_impact-total_impact_2021, `PERCENTAGE CHANGE` = total_change/total_impact_2021*100, 
         `PERCENTAGE CHANGE` = round(`PERCENTAGE CHANGE`, 1)) %>%
  arrange(desc(total_impact)) %>%
  rename(Indicator = indicator2)

## -- TABLE_ImpactChangeTrend -----------------------------------------------------------
# A table showing the change in the global 
# economic impact of violence, billions of PPP 2022 US dollars, 2008–2022

tab_ChangeTrend <- econ_impact.df %>%
  dplyr::filter(year %in% c (min(year), max(year)), subtype=="impact") %>%
  group_by(indicator2, year) %>%
  summarise(value = sum(value)/1000000000, value = round(value, 1)) %>%
  pivot_wider(names_from = year, values_from = value) %>%
  mutate(BILLIONS = `2022`-`2008`, `PERCENTAGE CHANGE` = BILLIONS/`2008`*100, 
         `PERCENTAGE CHANGE` = round(`PERCENTAGE CHANGE`, 0)) %>%
  arrange(desc(`PERCENTAGE CHANGE`)) %>%
  rename(Indicator = indicator2)


## -- TABLE_MEx -----------------------------------------------------------
# Three tables showing the ten countries with the highest total military expenditure,
# military expenditure per capita and military expenditure as a % of GDP

tab_totalMEx <- econ_impact.df %>%
  dplyr::filter(year == max(year), indicator=="milex", subtype=="costusd") %>%
  select(country, value) %>%
  arrange(desc(value)) %>%
  slice(1:10) %>%
  mutate(value=value/1000000000, value=round(value, 2))  %>%
  rename("COUNTRY" = country, "MILITARY EXPENDITURE (TOTAL, $US BILLIONS)" = value)

tab_PCapMEx <- econ_impact.df %>%
  dplyr::filter(year == max(year), indicator=="milex", subtype %in% c("costusd", "pop")) %>%
  pivot_wider(names_from = subtype, values_from = value) %>%
  mutate(value=costusd/pop, value=round(value, 2)) %>%
  arrange(desc(value)) %>%
  slice(1:10) %>%
  select(country, value)  %>%
  rename("COUNTRY" = country, "MILITARY EXPENDITURE (PER CAPITA, $US)" = value)

tab_GDPMEx <- econ_impact.df %>%
  dplyr::filter(year == max(year), indicator=="milex", subtype %in% c("costusd", "gdp")) %>%
  pivot_wider(names_from = subtype, values_from = value) %>%
  mutate(value=costusd/gdp*100, value=round(value, 2)) %>%
  arrange(desc(value)) %>%
  slice(1:10) %>%
  select(country, value) %>%
  rename("COUNTRY" = country, "MILITARY EXPENDITURE (% OF GDP)" = value)

## -- TABLE_TenCountries -----------------------------------------------------------
# A table of the ten countries with the highest economic cost of violence as a % of GDP

# Transform Data
tab_TenCountries <- econ_impact.df %>%
  dplyr::filter(year == max(year), subtype=="costppp") %>%
  group_by(country, type) %>%
  summarise(value = sum(value)) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  mutate(econ_cost = sum(indirect, direct))

tab_GDP <- econ_impact.df %>%
  dplyr::filter(year==max(year), subtype=="gdp") %>%
  select(country, year, value) %>%
  distinct(country, year, .keep_all = TRUE)

tab_TenCountries <- tab_TenCountries %>%
  left_join(tab_GDP) %>%
  group_by(country) %>%
  summarise(perc_gdp = econ_cost/value) %>%
  arrange(desc(perc_gdp)) %>%
  slice(1:10) %>% 
  add_row(country = 'Average', !!! colMeans(.[-1])) %>%
  mutate(perc_gdp = scales::percent(perc_gdp)) %>%
  rename("ECONOMIC COST OF VIOLENCE AS (% OF GDP)" = perc_gdp, "Country" = country)
