# load constants and create the folders needed for data preprocessing

DATA_DIR <- here('data')
DEMO_DATA_DIR = here(DATA_DIR, '01_demographic_data')
XY_DATA_DIR = here(DATA_DIR, '01_xy_data')
INTERMEDIATE_FOLDER <- here(DATA_DIR, "02_intermediates")
RESULTS_FOLDER <- here(DATA_DIR, "03_preprocessed")

for(dir in c(DATA_DIR,
             DEMO_DATA_DIR,
             XY_DATA_DIR,
             INTERMEDIATE_FOLDER,
             RESULTS_FOLDER)
    ){
  dir.create(here(dir), showWarnings = FALSE)
}

INTERMEDIATE_000_EXCL_SESSION <- "000-exclusion-session-errors.csv"
INTERMEDIATE_000_EXCL_TEMP <- "000-exclusion-open-missings-all.csv"

INTERMEDIATE_001_ADULT <- "001-demographic-data-adults.Rds"
INTERMEDIATE_001_TODDLER <- "001-demographic-data-toddlers.Rds"
INTERMEDIATE_002A <- "002-resampled-et-data.Rds"
INTERMEDIATE_002 <- "002-merged-et-data.Rds"
INTERMEDIATE_002B <- "002-merged-et-demo-data.Rds"
INTERMEDIATE_003 <- "003-traildata-added-data.Rds"
INTERMEDIATE_004 <- "004-resampled-data.Rds"
INTERMEDIATE_005 <- "005-aoi-data.Rds"
INTERMEDIATE_006 <- "006-aoi-data-excluded.Rds"
INTERMEDIATE_006a <- "006-exclusions.csv"
