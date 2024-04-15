#### Project Variables #####

#### General Variables ####

REPORT_YEAR = 2024
LATEST_YEAR = 2023
FIRST_GPI_YEAR = 2011




#### Charts
CHART_UNIT = "cm"

ONEDRIVE = paste0(IEP_USERPATH,"/Global Peace Index/",REPORT_YEAR," GPI")

CHARTBOOK_1 = paste0(ONEDRIVE,"/Layout/Charts/GPI_EconCostingChartbook_",REPORT_YEAR,".xlsx")

CHART_FILES = paste0(ONEDRIVE,"/Layout/Charts")
IMAGE_FILES = paste0(ONEDRIVE,"/Layout/Images")
TABLE_FILES = paste0(ONEDRIVE,"/Layout/Tables")
MAP_FILES = paste0(ONEDRIVE,"/Layout/Maps")

# Chart Sizes
CHARTS <- list(
  small = c(width = 8.45, height = 10),
  medium = c(width = 12, height = 10),
  large = c(width = 17.6, height = 10)
)

# Map Sizes
MAPS <- list(
  small = c(width = 12, height = 8),
  medium = c(width = 14, height = 10),
  large = c(width = 28, height = 14)
)

# Chart Fonts
HEAVY_FONT = "Helvetica LT Pro" 
LIGHT_FONT = "Helvetica LT Pro Light" 


