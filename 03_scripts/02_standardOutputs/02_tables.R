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
# Military expenditure: total, per capita, and as percentage of GDP

TABLE_MEx = c(title = "Military expenditure: total, per capita, and as percentage of GDP",
                       sheet = "MEx", source = "IEP Calculations", xtext = "", ytext = "",
                       type = "Table", position = "Normal")

# The ten countries with the highest economic cost of violence, percentage of GDP
TABLE_TenCountries = c(title = "The Ten Countries with the Highest Economic Cost of Violence, Percentage of GDP",
                     sheet = "TenCountries", source = "IEP Calculations", xtext = "", ytext = "",
                     type = "Table", position = "Normal")


### --- Loading Data

econ_impact.df <- rio::import("04_outputs/Economic Impact of Violence.xlsx")

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
