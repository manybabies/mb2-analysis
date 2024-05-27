# mb2-analysis

Analysis scripts for ManyBabies 2, corresponding to the Child Development registered report, [Schuwerk, Kampis et al. (accepted pending data collection)](https://psyarxiv.com/x4jbm/). 

The 'main' branch contains all analysis scripts necessary to reproduce the results from the registered report. For the analysis files of the pilot, look towards the 'Pilot analysis' release or the 'pilot_final' branch.

Subdirectories explanations:

* `helper` stores general helper functions
* `metadata` stores functions that create metadata for preprocessing/analysis
* `import_scripts` contains import scripts for the main dataset.
  * Important notes on how to do this task are in the [MB2 data import guide](https://docs.google.com/document/d/1MEEQicPc1baABDHFasbWoujvj2GwfBGarwrzyS2JQtM/edit).
  * the webcam data was imported by a separate script, found and explained in the [mb2 webcam eye-tracking repository](https://github.com/adriansteffan/mb2-browser-version?tab=readme-ov-file#data-processing-pipeline)
* `data` (will be created while running the pipeline is gitignored )
  * `01_xy_data` - xy data will be downloaded from osf into this
  * `01_demographic_data` - demographic data will be downloaded from osf into this
  * `02_intermediates` - preprocessing steps will save intermediate rds files here
  * `03_preprocessed` - the result of the preprocessing steps - to be consumed by the analysis script


