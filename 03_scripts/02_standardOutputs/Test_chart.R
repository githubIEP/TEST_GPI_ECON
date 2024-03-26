# # Function to load packages only if not already loaded
# f_LibraryLoader(tidyverse,
#                 rio,
#                 openxlsx,
#                 scales,
#                 waterfalls,
#                 stringr,
#                 patchwork,
#                 padr,
#                 extrafont,
#                 svglite)




econ_impact.df <- rio::import("04_outputs/Economic Impact of Violence.xlsx")

CHART_EconImpact = c(title = "Trend in Economic Impact of Violence",
                       sheet = "EconImpact", source = "IEP Calculations", xtext = "year", ytext = "Economic Impact of Violence (Constant 2022 USD)",
                       type = "Chart", position = "Normal")




CHART_EconImpact.df <- econ_impact.df %>%
  dplyr::filter(subtype == "impact") %>%
  group_by(year) %>%
  summarise(value = sum(value)) %>%
  ungroup()


p <- ggplot(data = CHART_EconImpact.df, aes(x = year, y = value)) +
  geom_line(stat = "identity")




pCHART_EconImpact <- f_ThemeTraining(plot = p, 
                                       chart_info = CHART_EconImpact, 
                                       plottitle = "Include", 
                                       xaxis = "Include", 
                                       yaxis = "Include", 
                                       xgridline = "", 
                                       ygridline = "Include")


pCHART_EconImpact




wb_SECTION3 <- createWorkbook()


SECTION3_EXPORT = c("CHART_EconImpact")


figure_count = 0
table_count = 0

#' The final step is to export the data. This will save any chart files in three sizes as svgs, three sizes
#' as pngs, and save the data in a workbook. The function takes four arguments: the section number, the workbook object,
#' the location of the spreadsheet you want to create, and the list of charts.
#' 

f_ProjectExport("1", wb_SECTION3, CHARTBOOK_1, SECTION3_EXPORT)



