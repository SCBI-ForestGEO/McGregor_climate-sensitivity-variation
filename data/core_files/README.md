# Core files

Core files and metadata should **ONLY** be used from the ForestGEO-Data/tree_cores repo. Do NOT use any core data from any other repo.

These folders and files are the exact same as those in the ForestGEO-Data repo, but have been moved here for easier sourcing for the script [canopy_position_analysis](https://github.com/SCBI-ForestGEO/McGregor_climate-sensitivity-variation/blob/master/scripts/canopy_position_analysis.R).

Terminology: a tree's "chronology" has been used to mean the .rwl file

## Process for choosing which cores to use in analyses
Originally I was helping Ryan with figuring out issues with the cores.
In Fall 2018, we did some reorganizing of the core folders that resulted in the current (as of June 2019) format of the [ForestGEO-Data/tree_cores](https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/tree/master/tree_cores) repo, which is now the main place for doing further analyses (the data is also copied in [climate_sensitivity_cores](https://github.com/SCBI-ForestGEO/climate_sensitivity_cores/tree/master/data/cores) plus the Dropbox folder "Tree Cores", so things can get a bit confusing).
I focused on the top species in terms of ANPP contribution, which are those that were used in Ryan's analysis (see README [here](https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/tree/master/tree_cores/chronologies)). 
Actual rwl files were taken from the [current_chronologies/complete/separated_by_canopy_position](https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/tree/master/tree_cores/chronologies/current_chronologies/complete) (themselves originally from the [processed](https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/tree/master/tree_cores/measurement_files) folder), with only cores taken at DBH (ID ends with "A") used).
I wrote scripts to then separate these out by canopy position based on the data collected by AJ.

In sum, my analysis uses cores that 
- were used in Ryan's analysis
- are complete cores
- were only taken at DBH

In the code, my data inputs for the cores are only the .rwl files (linked above). I did not use any data that came from ARSTAN outputs
