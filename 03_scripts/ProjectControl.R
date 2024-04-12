
source("03_scripts/ProjectFunction.R")
source("03_scripts/ProjectVariables.R")
source("03_scripts/ProjectExport.R")


script_dir <- "03_scripts/01_cleaning"

script_files <- list.files(script_dir, pattern = "\\.R$", full.names = TRUE)

for (script_file in script_files) {
  source(script_file)
}

source("03_scripts/02_standardOutputs/Test_chart.R")
