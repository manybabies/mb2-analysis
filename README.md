# mb2-analysis

Analysis scripts for ManyBabies 2, corresponding to the Child Development registered report, [Schuwerk, Kampis et al. (accepted pending data collection)](https://psyarxiv.com/x4jbm/). 

Subdirectories catalog different stages of the analysis:

* `data_integrity` describes data standards and provides validation for data files and AOIs
* `design_analysis` is power analysis for the Child Development registered report paper
* `helper` stores general helper functions
* `metadata` stores functions that create metadata for analysis
* `pilot_analysis` gives the analysis for the pilots 1a and 1b (reported in the Child Dev RR)
* `pilot_data` contains raw and processed data as well as import scripts for each pilot dataset.
* `data` contains raw and processed data and import scripts for the main dataset.

## Data preprocessing

All data directories follow the following format (adapted from [peekbank](http://peekbank.stanford.edu)):

* `raw_data` - as received from lab
* `processed_data` - data in the peekbank data standard format (current specification used in this project is described in `data_integrity/peekds.Rmd`). 
* `import_scripts` - contains `import.R` and other helper scripts to go from raw to processed data formats. 

Important notes on how to do this task are in the [MB2 data import guide](https://docs.google.com/document/d/1MEEQicPc1baABDHFasbWoujvj2GwfBGarwrzyS2JQtM/edit).
