# clean up amsterdam eye tracking files
library(tidyverse)
library(data.table)

# adults
DATA_COLLECTION_PATH = 'import_scripts/babylabAmsterdam/raw_data/adults'

data_raw <- rbindlist(lapply(list.files(DATA_COLLECTION_PATH), function(f) {
  read.csv(file.path(DATA_COLLECTION_PATH, f), sep = ',')
}))

write.csv(data_raw, 'import_scripts/babylabAmsterdam/babylabAmsterdam_adults_eyetrackingdata.csv')


# toddlers
DATA_COLLECTION_PATH = 'import_scripts/babylabAmsterdam/raw_data/toddlers'

data_raw <- rbindlist(lapply(list.files(DATA_COLLECTION_PATH), function(f) {
  read.csv(file.path(DATA_COLLECTION_PATH, f), sep = ',')
}))


write.csv(data_raw, 'import_scripts/babylabAmsterdam/babylabAmsterdam_toddlers_eyetrackingdata.csv')
