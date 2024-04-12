f_ThemeTraining <- function(plot, chart_info, plottitle, xaxis, yaxis, xgridline, ygridline) {
  finalcaption <- paste0("Source: ", chart_info[["source"]])
  
  plot_labels <- labs(
    title = chart_info[["title"]],
    x = chart_info[["xtext"]],
    y = chart_info[["ytext"]],
    caption = finalcaption
  )
  
  plot_base <- theme_minimal()
  
  plot_theme <- plot_base +
    theme(text = element_text(family = HEAVY_FONT),
          plot.title.position = "plot",
          plot.subtitle = element_text(size = 9),
          plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0, colour = "#888686", size = 7),
          axis.text = element_text(colour = "#444444", size = 6.5, family = LIGHT_FONT),
          axis.title = element_text(face = "bold", size = 7, family = HEAVY_FONT),
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 7, family = LIGHT_FONT),
          axis.line.x = element_blank(),
          axis.line.y = element_blank()
    )
  
  if (plottitle == "Include") {
    plot_theme <- plot_theme + theme(plot.title = element_text(size = 13, family = HEAVY_FONT))
  } else {
    plot_theme <- plot_theme + theme(plot.title = element_blank())
  }
  
  if (xaxis == "Include") {
    plot_theme <- plot_theme + theme(axis.line.x.bottom = element_line(colour = "#444444"))
  }
  
  if (yaxis == "Include") {
    plot_theme <- plot_theme + theme(axis.line.y = element_line(colour = "#444444"))
  }
  
  if (ygridline == "Include") {
    plot_theme <- plot_theme + theme(panel.grid.major.y = element_line(colour = "lightgrey"))
  } else {
    plot_theme <- plot_theme + theme(panel.grid.major.y = element_blank())
  }
  
  if (xgridline == "Include") {
    plot_theme <- plot_theme + theme(panel.grid.major.x = element_line(colour = "lightgrey"))
  } else {
    plot_theme <- plot_theme + theme(panel.grid.major.x = element_blank())
  }
  
  # Apply themes and labels to the plot
  plot <- plot + plot_labels + plot_theme
  
  # Adjust y-axis to position x-axis line at y=0
  if (xaxis == "Include") {
    plot <- plot + scale_y_continuous(expand = c(0,0))
  }
  
  return(plot)
}

# f_TrainingSavePlots: This function saves the plot in three different sizes -----------
#' This function looks at the type of object(eg chart, map, diagram) and saves the plot
#' in three different sizes in the appropriate location
f_TrainingSavePlots <- function(chart_title, plot_name) {
  
  if (chart_title["type"] == "Chart") {
    # Looping through the three chart sizes
    for (size_name in names(CHARTS)) {
      size <- CHARTS[[size_name]]
      file_base_name <- paste0(chart_title["sheet"], "_", size_name)
      
      # Save as PNG in CHARTS_PATH with transparency
      png_file <- paste0(IMAGE_FILES, "/", file_base_name, ".png")
      ggsave(png_file, plot_name, device = "png", width = size["width"], height = size["height"], units = CHART_UNIT, bg = "transparent")
      
      # Save as SVG in ONEDRIVE_PATH
      svg_file <- paste0(CHART_FILES, "/", file_base_name, ".svg")
      ggsave(svg_file, plot_name, device = "svg", width = size["width"], height = size["height"], units = CHART_UNIT)
    }
  }
  
  if (chart_title["type"] == "Map") {
    # Looping through the three chart sizes
    for (size_name in names(TRAINING_MAPS)) {
      size <- TRAINING_MAPS[[size_name]]
      file_base_name <- paste0(chart_title["sheet"], "_", size_name)
      
      # Save as PNG in CHARTS_PATH with transparency
      png_file <- paste0(IMAGE_FILES, "/", file_base_name, ".png")
      ggsave(png_file, plot_name, device = "png", width = size["width"], height = size["height"], units = CHART_UNIT, bg = "transparent")
      
      # Save as SVG in ONEDRIVE_PATH
      svg_file <- paste0(MAP_FILES, "/", file_base_name, ".svg")
      ggsave(svg_file, plot_name, device = "svg", width = size["width"], height = size["height"], units = CHART_UNIT)
    }
  }
  
  if (chart_title["type"] == "Diagram") {
    # Looping through the three chart sizes
    for (size_name in names(CHARTS)) {
      size <- CHARTS[[size_name]]
      file_base_name <- paste0(chart_title["sheet"], "_", size_name)
      
      # Save as SVG in ONEDRIVE_PATH
      svg_file <- paste0(CHART_FILES, "/", file_base_name, ".svg")
      svg_temp = DiagrammeRsvg::export_svg(plot_name)
      write_lines(svg_temp,svg_file)
      
      #convert to png
      rsvg_png(svg_file, paste0(IMAGE_FILES, "/", file_base_name, ".png"))
      
      
    }
  }
  
}

# f_TrainingChartbook: Creates a chartbook object ------------------------------
#' This function creates a workbook object in R, that will eventually be exported as an
#' excel file. The workbook will contain all the data associated with the charts, maps etc.
f_TrainingChartbook <- function(filepath) {
  {
    wb <- createWorkbook()
    addWorksheet(wb, "default")
    saveWorkbook(wb, filepath, overwrite = TRUE)
  }
}

# f_TrainingSheet: This function creates a specific sheet in the workbook ------
f_TrainingSheet <- function(workbook, chart_name) {
  
  sheet_name = chart_name["sheet"]
  
  # Check if the sheet already exists
  existing_sheets <- sheets(workbook)
  if (!(sheet_name %in% existing_sheets)) {
    addWorksheet(workbook, sheet_name)
  }
  
  # Define and apply styles
  font_style <- createStyle(fontSize = 8, fontName = "Arial")
  addStyle(workbook, sheet = sheet_name, style = font_style, rows = 1:1000, cols = 1:100, gridExpand = TRUE)
  
  setColWidths(workbook, sheet = sheet_name, cols = 1, widths = 1)      # Column 1 width
  setColWidths(workbook, sheet = sheet_name, cols = 2, widths = "auto")  # Auto width for Column B
  
  bold_style <- createStyle(fontName = "Arial", fontSize = 8, textDecoration = "bold")
  addStyle(workbook, sheet = sheet_name, style = bold_style, rows = 1:1000, cols = 2, gridExpand = TRUE)
  
  # Add specific cell text
  writeData(workbook, sheet = sheet_name, x = "Title", startCol = 2, startRow = 2)
  writeData(workbook, sheet = sheet_name, x = "sub-title", startCol = 2, startRow = 3)
  writeData(workbook, sheet = sheet_name, x = "x-axis title", startCol = 2, startRow = 4)
  writeData(workbook, sheet = sheet_name, x = "y-axis title", startCol = 2, startRow = 5)
  writeData(workbook, sheet = sheet_name, x = "source", startCol = 2, startRow = 6)
  writeData(workbook, sheet = sheet_name, x = "Notes", startCol = 2, startRow = 7)
  writeData(workbook, sheet = sheet_name, x = chart_name["title"], startCol = 3, startRow = 2)
  writeData(workbook, sheet = sheet_name, x = chart_name["xtext"], startCol = 3, startRow = 4)
  writeData(workbook, sheet = sheet_name, x = chart_name["ytext"], startCol = 3, startRow = 5)
  writeData(workbook, sheet = sheet_name, x = chart_name["source"], startCol = 3, startRow = 6)
  
}

# f_TrainingSheetData: This function writes data to the worksheet --------------
f_TrainingSheetData <- function(df, workbook, chart_info) {
  writeData(workbook, sheet = chart_info["sheet"], 
            x = df, startCol = 3, startRow = 9)
}


# f_TrainingSheetImage: This function creates a chart image for the workbook. --
#' This won't be the final image file, it's just a reference for comms
f_TrainingSheetImage <- function(workbook, chart_info) {
  insertImage(workbook,chart_info[["sheet"]],paste0(IMAGE_FILES,"/",chart_info[["sheet"]],"_small.png"),
              startRow = 9, startCol = 10)
}

## -- f_ProjectExport: A function that exports both charts and data ------------
#' This is the master function that combines all of the functions above, and exports
#' images, data, and worksheets, all labelled with the appropriate numbers etc
#' 
f_ProjectExport <- function(section, workbook, spreadsheet, chartlist) {
  
  for (chart_info in chartlist) {
    
    # Transform string into variable
    chart_var = get(chart_info, envir = .GlobalEnv)
    
    if (chart_var['position'] == "Normal") {
      if (chart_var['type'] %in% c("Chart", "Diagram", "Map")) {
        figure_count = figure_count + 1
        figure_xx = paste0(section, ".", figure_count)
        chart_var['counter'] = figure_xx
        chart_var['sheet'] = paste0(figure_xx, "_", chart_var['sheet'])
      } else if (chart_var['type'] == "Table") {
        table_count = table_count + 1
        table_xx = paste0(section, ".", table_count)
        chart_var['counter'] = table_xx
        chart_var['sheet'] = paste0(table_xx, "_", chart_var['sheet'])
      } else {
        chart_var['counter'] = section
      }
    }
    
    if (nchar(chart_var['sheet']) > 31) {
      chart_var['sheet'] = substr(chart_var['sheet'], 1, 31)
    }
    
    if (chart_var["type"] == "Chart") {
      chart_name = paste0("p",chart_info)
      chart_plot = get(chart_name, envir = .GlobalEnv)
      chart_data = paste0(chart_info,".df")
      chart_df = get(chart_data, envir = .GlobalEnv)
      f_TrainingSheet(workbook,chart_var)
      f_TrainingSheetData(chart_df, workbook, chart_var)
      f_TrainingSavePlots(chart_var, chart_plot)
      Sys.sleep(1)
      f_TrainingSheetImage(workbook = workbook, chart_info = chart_var)
    }
    
    # If type is table
    if (chart_var["type"] == "Table") {
      chart_data = paste0(chart_info,".df")
      chart_df = get(chart_data, envir = .GlobalEnv)
      f_TrainingSheet(workbook,chart_var)
      f_TrainingSheetData(chart_df, workbook, chart_var)
      rio::export(chart_df,paste0(TABLE_FILES,"/",chart_var[["sheet"]],".csv"), row.names = FALSE)
    }
    
    # If type is Map or Diagram
    if (chart_var["type"] == "Map" | chart_var["type"] == "Diagram") {
      chart_name = paste0("p",chart_info)
      chart_plot = get(chart_name, envir = .GlobalEnv)
      f_TrainingSheet(workbook,chart_var)
      f_TrainingSavePlots(chart_var, chart_plot)
      Sys.sleep(1)
      f_TrainingSheetImage(workbook = workbook, chart_info = chart_var)
    }
  }
  
  # save the workbook
  saveWorkbook(workbook, spreadsheet, overwrite = TRUE)
  
  # Assign current count for this section to a variable in the global environment
  assign(paste0("figure_count"), figure_count, envir = .GlobalEnv)
  assign(paste0("table_count"), table_count, envir = .GlobalEnv)
}

#' Now that we have our variables and functions sorted, it's time to make a chart,
#' and then we can export it