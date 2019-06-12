# Methods

Research was conducted at the Smithsonian Conservation Biology Institute (SCBI) in Virginia, USA, with cores taken from trees on the 25.6 ha ForestGEO (Global Earth Observatory) study plot [@andersonteixeira_ctfs-forestgeo:_2015]. SCBI (38°53'36.6"N, 78° 08'43.4"W) [@gonzalezakre_patterns_2016] is located in the central Appalachian Mountains at the northern edge of Shenandoah National Park. Primary forest composition in the plot is oak-hickory, while elevations range from 273-338m above sea level [@gonzalezakre_patterns_2016] with a topographic relief of 65m (Bourg et al 2013).

## Data Collection
Cores were collected in 2010-2011 or 2016-2017 from a breast height of 1.3m using a 5mm increment borer. Live trees were selected at random from each species in 2010-2011, with at least 30 of those trees having a diameter at breast height (DBH) of at least 10cm (Bourg et al 2013). In 2016-2017, cores were collected from all dead trees found in the annual mortality census [@gonzalezakre_patterns_2016]. They were sanded, measured, and standardized in the same process as seen in Helcoski et al 2019.

Cores were then split into canopy and subcanopy groupings and processed with ARSTAN. Using CRU data **with Valentine's quilts for each grouping**, it was found that all climate variables tested were significant by tree position when using a linear mixed model (**this was done early-on, included here because unsure if will keep for full analysis**).

To determine focus drought years, we used the pointRes package in R (version 3.6.0) to determine resilience metrics. Specifically, we calculated pointer years (main drought years) if >50% of the cored trees experienced <30% growth in a year compared to the past 5 years. The top 6 pointer years were compared with PDSI (Palmer Drought Severity Index) values for the region, obtained from NOAA. For ease of research, we then took the top three pointer years of 1964-1966 (resistance metric averaged due to a single, prolonged drought), 1977, and 1999.

Canopy classes were observed in the field during the growing season of 2018 following the crown position protocol from Jennings 1999, whereby positions were ranked as dominant, codominant, intermediate, or suppressed. For ease of core analysis, these labels were condensed to canopy (including dominant and codominant) and subcanopy (intermediate and suppressed). Individual cores were then grouped by canopy or subcanopy before being processed by ARSTAN, and species inclusion in the study was dependent on the robustness of each species’ cores. In total, 14 species had cores that were considered complete and usable, as seen in Table 1.

Geographic info

Elevation for the trees was extracted from a USGS DEM in ArcMap. Distance to water was calculated as the shortest distance in meters between each individual tree and the major streams running through the ForestGEO plot.

Growth

As part of the ForestGEO five-year census, there are DBH(diameter at breast height) measurements for all stems. Using the data from 2008 and a dataset of bark thickness for the plot collected from previous studies, we devised an equation to retroactively calculate DBH for all trees,

diam_nobark_1999 = dbh2008 - 2*(bark.depth2008) - 2*(sum(ring.width1999:ring.width2008))

First, we generated log-log regression equations for bark thickness based on DBH, and then determined mean bark thickness per species. Ring widths were determined from the processed cores, and once the diameter without bark was calculated for a certain year, we added the mean bark thickness in from the regression equations.

Sapwood ratio also used the bark thickness data, and was also calculated for 2008, where

sapwood_area = total wood area (without bark) - heartwood area, and
sapwood_Ratio = sapwood area / total wood area (without bark)

Once sapwood area for each tree was calculated, log-log regression equations were determined on a per-species basis, which were then used 

Height

Height data for trees was obtained from clinometers and rangefinders, from 2012 to 2019. Log=log regression equations were developed for species based on this height data, then heights of the study trees were calculated using these equations. For species that didn't have enough height measurements, heights were calculated from equations derived from all study-focus species.

Height sources include [@stovall_assessing_2018], [@stovall_terrestrial_2018], [@andersonteixeira_ctfs-forestgeo:_2015], 


Traits
Turgor loss point (TLP) was determined per species from leaf trait studies carried out at sCBI in 2018. TLP was determined using __________________ on ### of individuals from each species (https://github.com/EcoClimLab/HydraulicTraits/blob/master/data/SCBI/processed_trait_data/SCBI_LeafTraits_measurements_plus_intermediary.csv).  Ring porosity was qualified from Anderson-Teixeira et al 2015.

After the data was collected, linear mixed models were run with the resistance value of each tree for each pointer year (determined from the pointRes package) as the response variable. Traits and other geographic data were used as independent variables.