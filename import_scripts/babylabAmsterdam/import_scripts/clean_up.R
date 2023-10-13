# clean up amsterdam eye tracking files
library(tidyverse)
library(data.table)

# adults
DATA_COLLECTION_PATH = 'data/babylabAmsterdam/raw_data/adults'

data_raw <- rbindlist(lapply(list.files(DATA_COLLECTION_PATH), function(f) {
  read.csv(file.path(DATA_COLLECTION_PATH, f), sep = ',')
}))

write.csv(data_raw, 'data/babylabAmsterdam/babylabAmsterdam_adults_eyetrackingdata.csv')


# toddlers
DATA_COLLECTION_PATH = 'data/babylabAmsterdam/raw_data/toddlers'

data_raw <- rbindlist(lapply(list.files(DATA_COLLECTION_PATH), function(f) {
  read.csv(file.path(DATA_COLLECTION_PATH, f), sep = ',')
}))


write.csv(data_raw, 'data/babylabAmsterdam/babylabAmsterdam_toddlers_eyetrackingdata.csv')
