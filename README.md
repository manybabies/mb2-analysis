# mb2-analysis

Analysis scripts for ManyBabies 2, corresponding to the Child Development registered report, [Schuwerk, Kampis et al. (accepted pending data collection)](https://psyarxiv.com/x4jbm/). 

For the analysis files for the pilot, look towards the 'Pilot analysis' release or the 'pilot_final' branch.

Subdirectories explanations:

* `helper` stores general helper functions
* `metadata` stores functions that create metadata for analysis
* `data` contains import scripts for the main dataset.
  * Important notes on how to do this task are in the [MB2 data import guide](https://docs.google.com/document/d/1MEEQicPc1baABDHFasbWoujvj2GwfBGarwrzyS2JQtM/edit).
* `demographic_data`
* `processed_data`
* `processed_xy_data`

## TODOS:

Is design_analysis from the pilot still relevant to the study? If so, get it from the pilot branch and put it back in here
Also add
* `design_analysis` is power analysis for the Child Development registered report paper

Proposed changes to folder structure:
* data -> import_scripts (as that's all that's being commited into that folder anyway)
* create new folder `data` (gitignored), and move the following things there
  * 01_xy_data (data that is currently downloaded to `processed_xy_data` at top level)
  * 01_demographic_data (data that is currently downloaded to `demographic_data` at top level)
  * 02_intermediates (Rds fioles that are shared between preprocessing steps - is currently living in `processed_data` at top level)
  * 03_preprocessed (will contain data that can be directly consumed by the analysis scripts)


