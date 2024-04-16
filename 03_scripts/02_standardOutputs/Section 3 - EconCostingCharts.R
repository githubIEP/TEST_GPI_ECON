##### ----- GPI ECON COSTING STANDARD CHARTS AND TABLES
#' This script is for producing all the standard GPI Econ Costing Charts and Tables
##### -----


### --- Libraries, Variables, Functions
library(knitr)
library(openxlsx)
library(reshape2)
library(iepg)
library(dplyr)
library(scales)
library(cowplot)
library(grid)

## -- Spreadsheet to save charts in
wb_Ssection3 <- createWorkbook()

### --- List of Standard Charts and Tables

# Composition of the global economic impact of violence, 2022
CHART_CompPie = c(title = "Composition of the global economic impact of violence, 2022",
                  sheet = "CompPie", source = "IEP Calculations", xtext = "", ytext = "",
                  type = "Chart", position = "Normal")

# Change in the global economic impact of violence, billions of PPP 2022 US dollars, 2021–2022

TABLE_ImpactChange = c(title = "Change in the global economic impact of violence, billions of PPP 2022 US dollars, 2021–2022",
                       sheet = "ImpactChange", source = "IEP Calculations", xtext = "", ytext = "",
                       type = "Table", position = "Normal")


# Trend in the global economic impact of violence, 2008–2022

CHART_Trend = c(title = "Trend in the global economic impact of violence, 2008–2022",
                  sheet = "Trend", source = "IEP Calculations", xtext = "", ytext = "",
                  type = "Chart", position = "Normal")

CHART_YOYTrend = c(title = "Trend in the global economic impact of violence, 2008–2022",
                sheet = "TrendYOY", source = "IEP Calculations", xtext = "", ytext = "",
                type = "Chart", position = "Normal")

# Change in the global economic impact of violence, billions of PPP 2022 US dollars, 2008–2022

TABLE_ImpactChangeTrend = c(title = "Change in the global economic impact of violence, billions of PPP 2022 US dollars, 2008–2022",
              sheet = "ImpactChangeTrend", source = "IEP Calculations", xtext = "", ytext = "",
              type = "Table", position = "Normal")

# Indexed trend in the economic impact by domain
CHART_DomainTrend = c(title = "Trend in Domains (Indexed)",
                 sheet = "Domain", source = "IEP Calculations", xtext = "", ytext = "INDEXED CHANGE (2008=1)",
                 type = "Chart", position = "Normal")

# Breakdown of the global economic impact of the Armed Conflict domain, 2022
CHART_ArmedViolence = c(title = "Breakdown of the global economic impact of the Armed Conflict domain, 2022",
                      sheet = "ArmedViolence", source = "IEP Calculations", xtext = "", ytext = "",
                      type = "Chart", position = "Normal")

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



## -- CHART_CompPie -----------------------------------------------------------
# A pie chart showing the composition of the global economic impact of violence

CHART_CompPie.df <- econ_impact.df %>%
  dplyr::filter(year == max(year), subtype == "impact") %>%
  group_by(indicator2) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  arrange(desc(value)) %>%
  mutate(indicator2 = if_else(row_number() > 6, "Other", indicator2)) %>%
  group_by(indicator2) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  arrange(desc(value)) %>%
  mutate(
    prop = (value / sum(value)),
    prop = round(prop, 2), 
    ymax = cumsum(prop),
    ymin = c(0, head(ymax, n = -1)),
    labelPosition = (ymax + ymin) / 2,
    label = paste0(indicator2, "\n", prop*100, "%"),
    x_pos = if_else(prop > 0.2, 3.5, 4.5),
    hjust = if_else(prop > 0.2, 0.5, 1))


pCHART_CompPie <- ggplot(CHART_CompPie.df, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = indicator2)) +
  geom_rect() +
  geom_text(data = CHART_CompPie.df, aes(x = 4.5, y = labelPosition, label = label), hjust = 1, size = 3) +  
  geom_segment(data = CHART_CompPie.df, aes(x = 4, y = labelPosition, xend = 4.4, yend = labelPosition), color = "black") +  
  scale_fill_brewer(palette = 4) +
  coord_polar(theta = "y") +
  xlim(c(2, 5.5)) +  
  theme_void() +
  theme(legend.position = "none")

pCHART_CompPie <- f_ThemeTraining(plot = pCHART_CompPie, 
                              chart_info = CHART_CompPie, 
                              plottitle = "Include", 
                              xaxis = "", 
                              yaxis = "", 
                              xgridline = "", 
                              ygridline = "") +
  theme(legend.position = "none")


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

## -- CHART_Trend -----------------------------------------------------------
# A line chart showing the trend in the global economic impact of violence

tab_Trend <- econ_impact.df %>%
  dplyr::filter(subtype == "impact") %>%
  group_by(year) %>%
  summarise(value = sum(value)) %>%
  ungroup()

pCHART_Trend <- ggplot(data = tab_Trend, aes(x = year, y = value/10^12)) +
  geom_line (size = 0.75, color = 'red') +
  scale_x_continuous (breaks = c(2008:2022)) +
  labs(y = "Total Cost (Constant 2022 US$ PPP, trillions)")


pCHART_Trend <- f_ThemeTraining(plot = pCHART_Trend, 
                                chart_info = CHART_Trend, 
                                plottitle = "", 
                                xaxis = "Include", 
                                yaxis = "Include", 
                                xgridline = "", 
                                ygridline = "Include")

## -- CHART_YOYTrend -----------------------------------------------------------
# A bar chart showing the YOY trend in the global economic impact of violence

tab_YOYTrend <- econ_impact.df %>%
  dplyr::filter(subtype == "impact") %>%
  group_by(year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(perc_change = (value - dplyr::lag(value)) / dplyr::lag(value))

# Create chart

pCHART_YoYChange <- tab_YOYTrend %>%
  mutate(colour_group = case_when(
    perc_change == 0 ~ "Grey",
    perc_change > 0 ~ "Red",
    perc_change < 0 ~ "Green",
    TRUE ~ "Grey"
  )) %>%
  ggplot(aes(x = year, y = perc_change)) +
  geom_bar(stat = "identity", aes(fill = colour_group)) +
  scale_fill_manual(values = c("Red" = "red", "Green" = "#53C1AB", "Grey" = "darkgrey"))  +
  scale_y_continuous(labels = scales::percent, limits = c(-0.1, 0.1))+
  scale_x_continuous(breaks = seq(min(tab_YOYTrend$year), max(tab_YOYTrend$year), by = 4))


# Add theme

pCHART_YoYChange <- f_ThemeTraining(
  pCHART_YoYChange, 
  chart_info = CHART_YOYTrend,
  plottitle = "",
  xaxis = "",
  yaxis = "",
  xgridline = "",
  ygridline = "Include") +
  theme(legend.position = "none")


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

## -- CHART_DomainTrend -----------------------------------------------------------
# A line chart showing the indexed trend in the economic impact by domain

tab_DomainTrend <- econ_impact.df %>%
  dplyr::filter(subtype == "impact") %>%
  select(year, indicator2, value) %>%
  group_by(indicator2, year) %>%
  summarise(value = sum(value)) %>%
  group_by(year) %>%
  summarise(
    VC = sum(value[indicator2 %in% c("Military expenditure", "Internal security expenditure", "Peacebuilding", "Peacekeeping", "Private security")]),
    AC = sum(value[indicator2 %in% c("Conflict deaths", "GDP losses", "Refugees and IDPs", "Small arms", "Terrorism")]),
    ISV = sum(value[indicator2 %in% c("Fear", "Homicide", "Incarceration", "Suicide", "Violent crime")]),
   ) %>%
  mutate(VC1 = VC / first(VC),
    AC1 = AC / first(AC),
    ISV1 = ISV / first(ISV)) %>%
  select(-VC, -AC, -ISV)

pCHART_DomainTrend <- ggplot(tab_DomainTrend, aes(x = year)) +
  geom_line(aes(y = VC1), color = "green", size = 1) +
  geom_line(aes(y = AC1), color = "red", size = 1) +
  geom_line(aes(y = ISV1), color = "blue", size = 1) +
  scale_y_continuous(limits = c(0.80, 3), breaks = seq(0.80, 3, by = 0.20)) + 
  scale_x_continuous(breaks = c(2008:2022)) +
  theme_minimal() +
  annotate("text", x = max(CHART_domain.df$year), y = CHART_domain.df$VC1[nrow(CHART_domain.df)], label = "Violence Containment", vjust = -0.6, hjust = 1.5, color = "green") +
  annotate("text", x = max(CHART_domain.df$year), y = CHART_domain.df$AC1[nrow(CHART_domain.df)], label = "Armed Conflict", vjust = 10, hjust = 1, color = "red") +
  annotate("text", x = max(CHART_domain.df$year), y = CHART_domain.df$ISV1[nrow(CHART_domain.df)], label = "Interpersonal and Self-Inflicted Violence", vjust = 2, hjust = 1, color = "blue")

pCHART_DomainTrend <- f_ThemeTraining(plot = pCHART_DomainTrend, 
                                      chart_info = CHART_DomainTrend, 
                                      plottitle = "", 
                                      xaxis = "Include", 
                                      yaxis = "Include", 
                                      xgridline = "", 
                                      ygridline = "")


# Add peace labels
pCHART_DomainTrend <- f_PeaceLabels(pCHART_DomainTrend, 
                                          xaxis = "",
                                          yaxis = "Include",
                                          left_text = "",
                                          right_text = "",
                                          up_text = "Deterioration",
                                          down_text = "Improvement",
                                          yposition = 0.02) 

## -- CHART_ArmedViolence -----------------------------------------------------------
# A pie chart breakdown of the global economic impact of the Armed Conflict domain

tab_ArmedConflict <- econ_impact.df %>%
  dplyr::filter(year == max(year), subtype == "impact",
                indicator2 %in% c("Terrorism", "Refugees and IDPs", "Small arms", "GDP losses", "Conflict deaths")) %>%
  group_by(indicator2) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  arrange(desc(value)) %>%
  mutate(
    prop = (value / sum(value)),
    prop = round(prop, 2), 
    ymax = cumsum(prop),
    ymin = c(0, head(ymax, n = -1)),
    labelPosition = (ymax + ymin) / 2,
    label = paste0(indicator2, "\n", prop*100, "%"),
    x_pos = if_else(prop > 0.2, 3.5, 4.5),  # Adjust 0.1 as needed for your data
    hjust = if_else(prop > 0.2, 0.5, 1))

CHART_ArmedViolence <- ggplot(tab_ArmedConflict, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = indicator2)) +
  geom_rect() +
  geom_text(data = tab_ArmedConflict, aes(x = 4.5, y = labelPosition, label = label), hjust = 1, size = 3) +  
  geom_segment(data = tab_ArmedConflict, aes(x = 4, y = labelPosition, xend = 4.4, yend = labelPosition), color = "black") +  
  scale_fill_brewer(palette = 4) +
  coord_polar(theta = "y") +
  xlim(c(2, 5.5)) +  
  theme_void() +
  theme(legend.position = "none") +
  annotate("text", x = 0, y = 0, label = "Armed Conflict")

pCHART_ArmedViolence <- f_ThemeTraining(plot = pCHART_ArmedViolence, 
                                        chart_info = CHART_ArmedViolence, 
                                        plottitle = "Include", 
                                        xaxis = "", 
                                        yaxis = "", 
                                        xgridline = "", 
                                        ygridline = "") +
  theme(legend.position = "none")

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
