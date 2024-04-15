##### ----- GPI ECON COSTING STANDARD TABLES
#' This script is for producing all the standard GPI Econ Costing Tables
##### -----


### --- Libraries, Variables, Functions
library(knitr)
library(openxlsx)
library(reshape2)

## -- Spreadsheet to save charts in
wb_Tables <- createWorkbook()

### --- List of Standard Tables

# Trend in Economic Impact of Violence
CHART_EconImpact = c(title = "Trend in Economic Impact of Violence",
                     sheet = "EconImpact", source = "IEP Calculations", xtext = "year", ytext = "Economic Impact of Violence (Constant 2022 USD)",
                     type = "Chart", position = "Normal")


### --- Loading Data

econ_impact.df <- rio::import("04_outputs/Economic Impact of Violence.xlsx")

