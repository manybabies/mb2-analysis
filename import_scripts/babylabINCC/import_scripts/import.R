library(tidyverse)
library(here)

LAB_NAME <- 'babylabINCC'
DATA_DIR = file.path('import_scripts', LAB_NAME, 'raw_data')
ADULT_DATA_FOLDER <- here(DATA_DIR, "raw_data/adults")

data_adult <- rbindlist(lapply(list.files(ADULT_DATA_FOLDER), function(f) {read.csv(file.path(ADULT_DATA_FOLDER, f))}))

# MISSING: MEDIAFILE NAMING - wait for them to change the files
