
# Methods

Research was conducted at the Smithsonian Conservation Biology Institute (SCBI) in Virginia, USA, with cores taken from trees on the 25.6 ha ForestGEO (Global Earth Observatory) study plot [@andersonteixeira_ctfs-forestgeo:_2015]. SCBI (38°53'36.6"N, 78° 08'43.4"W) [@gonzalezakre_patterns_2016] is located in the central Appalachian Mountains at the northern edge of Shenandoah National Park. Primary forest composition in the plot is oak-hickory, while elevations range from 273-338m above sea level [@gonzalezakre_patterns_2016] with a topographic relief of 65m [@bourg_initial_2013].

![Structure of ForestGEO plot](tables_figures/ForestGEO_plot.jpg){width=500px}

## Data Collection
Cores were collected in 2010-2011 or 2016-2017 from a breast height of 1.3m using a 5mm increment borer. Live trees were selected at random from each species in 2010-2011, with at least 30 of those trees having a diameter at breast height (DBH) of at least 10cm [@bourg_initial_2013]. In 2016-2017, cores were collected from all dead trees found in the annual mortality census [@gonzalezakre_patterns_2016]. They were sanded, measured, and standardized in the same process as seen in [@helcoski_growing_2019]. Cores were then split into canopy and subcanopy groupings as determined in the field. Species' inclusion in the study was dependent on the robustness of each species’ cores. In total, 14 species had cores that were considered complete and usable, as seen in Table 1.

|**Species**|**n_cores.canopy**|**n_cores.subcanopy**|**included**
|----------|-------------|---------------|-----------|
*Carya cordiformis*|2|11|yes
*Carya glabra*|9|22|yes
*Carya ovalis*|9|14|yes
*Carya tomentosa*|0|13|sub-canopy only
*Fagus grandifolia*|7|73|yes
*Fraxinus americana*|20|42|yes
*Fraxinus nigra*|1|11| sub-canopy only
*Juglans nigra*|22|9|yes
*Liriodendron tulipifera*|38|60|yes
*Quercus alba*|38|23|yes
*Quercus prinus*|27|32|yes
*Quercus rubra*|43|26|yes
*Quercus velutina*|54|23|yes




Using CRU data **with Valentine's quilts for each grouping**, it was found that all climate variables tested were significant by tree position when using a linear mixed model (**this was done early-on, included here because unsure if will keep for full analysis**).

To determine focus drought years, we used the pointRes package [@R-pointRes] in R (version 3.5.2) to determine resilience metrics. Specifically, we calculated pointer years (main drought years) if >50% of the cored trees experienced <30% growth in a year compared to the previous 5 years. The top 6 pointer years were compared with PDSI (Palmer Drought Severity Index) values for the region, obtained from NOAA. Based on this comparison, we then took the top three pointer years of 1964-1966 (resistance metric averaged due to a single, prolonged drought), 1977, and 1999.

![Time series of combined tree cores by species](tables_figures/Time_series_for_each_species.jpg){width=500px}

### Biophysical traits
Canopy classes were observed in the field during the growing season of 2018 following the crown position protocol from [@jennings_assessing_1999], whereby positions were ranked as dominant, codominant, intermediate, or suppressed. In determining pointer years, these labels were condensed to canopy (including dominant and codominant) and subcanopy (intermediate and suppressed). The full set of canopy classes were used for all other analyses.

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> sp </th>
   <th style="text-align:right;"> n_cores </th>
   <th style="text-align:right;"> dominant </th>
   <th style="text-align:right;"> co-dominant </th>
   <th style="text-align:right;"> intermediate </th>
   <th style="text-align:right;"> suppressed </th>
   <th style="text-align:right;"> prior dead </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> caco </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cagl </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> caovl </td>
   <td style="text-align:right;"> 23 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cato </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fagr </td>
   <td style="text-align:right;"> 80 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 48 </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fram </td>
   <td style="text-align:right;"> 62 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 12 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> juni </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> litu </td>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> qual </td>
   <td style="text-align:right;"> 61 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 34 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> qupr </td>
   <td style="text-align:right;"> 59 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> quru </td>
   <td style="text-align:right;"> 69 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 23 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> quve </td>
   <td style="text-align:right;"> 77 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
</tbody>
</table>

Elevation for the trees was extracted from a USGS DEM in ArcMap. Distance to water was calculated as the shortest distance in meters between each individual tree and the major streams running through the ForestGEO plot.

Height data for trees was collected from several researchers at SCBI from 2012 to 2019, with methods including manual [@stovall_assessing_2018, NEON], digital rangefinders [@andersonteixeira_ctfs-forestgeo:_2015], and automatic LiDAR [@stovall_terrestrial_2018]. Rangefinders either used the tangent method (Impulse 200LR, TruPulse 360R) or the sine method (Nikon ForestryPro) for calculating heights. The associated errors for using either method were acknowledged [@larjavaara_measuring_2013]. Log-log, species-specific regression equations were developed, which were used to calculate heights of the study trees. For species that didn't have enough height measurements, heights were calculated from equations derived from all species in the study.

Equations required the DBH in each individual drought year. As part of the ForestGEO five-year census, there are DBH(diameter at breast height) measurements for all stems. Using the data from 2008 and a dataset of bark thickness for the plot collected from previous studies, we devised an equation to retroactively calculate DBH for all trees,

`diam_nobark_1999 = dbh2008 - 2*(bark.depth2008) - 2*(sum(ring.width1999:ring.width2008))`

First, we generated log-log regression equations for bark thickness based on DBH, and then determined mean bark thickness per species. Ring widths were determined from the processed cores, and once the diameter without bark was calculated for a certain year, we added the mean bark thickness from the regression equations.

### Leaf Hydraulic Traits
Hydraulic traits were collected from SCBI and are summarized in Table 1. Ring porosity qualifications were obtained from [@andersonteixeira_size-related_2015].


Need to say the methodology for all traits used (**Nobby making short list**)
1. PLA
2. LMA
3. Chlorophyll
4. Wood density
5. TLP
6. P50 = psi_0.5_kl50
7. P80 = psi_0.5_kl80

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Trait </th>
   <th style="text-align:left;"> Unit </th>
   <th style="text-align:right;"> mean </th>
   <th style="text-align:right;"> min </th>
   <th style="text-align:right;"> max </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Ring Porosity </td>
   <td style="text-align:left;"> ring, semi-ring, diffuse </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Percent Leaf Area </td>
   <td style="text-align:left;"> % </td>
   <td style="text-align:right;"> 15.09 </td>
   <td style="text-align:right;"> 8.52 </td>
   <td style="text-align:right;"> 24.64 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Leaf Mass Area </td>
   <td style="text-align:left;"> g/m2 </td>
   <td style="text-align:right;"> 53.50 </td>
   <td style="text-align:right;"> 30.68 </td>
   <td style="text-align:right;"> 75.80 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Chlorophyll </td>
   <td style="text-align:left;"> m2/g </td>
   <td style="text-align:right;"> 0.67 </td>
   <td style="text-align:right;"> 0.36 </td>
   <td style="text-align:right;"> 0.98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wood density </td>
   <td style="text-align:left;"> g/cm3 </td>
   <td style="text-align:right;"> 0.70 </td>
   <td style="text-align:right;"> 0.40 </td>
   <td style="text-align:right;"> 1.09 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TLP </td>
   <td style="text-align:left;"> MPa </td>
   <td style="text-align:right;"> -2.36 </td>
   <td style="text-align:right;"> -2.76 </td>
   <td style="text-align:right;"> -1.92 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P50 </td>
   <td style="text-align:left;"> MPa </td>
   <td style="text-align:right;"> 0.75 </td>
   <td style="text-align:right;"> 0.44 </td>
   <td style="text-align:right;"> 1.36 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P80 </td>
   <td style="text-align:left;"> MPa </td>
   <td style="text-align:right;"> 1.42 </td>
   <td style="text-align:right;"> 0.68 </td>
   <td style="text-align:right;"> 2.60 </td>
  </tr>
</tbody>
</table>


After the data was collected, linear mixed models were run with the resistance value of each tree for each pointer year (determined from the pointRes package) as the response variable. Traits and other geographic data were used as independent variables.

## Model runs
Traits' importance in predicting drought tolerance was calculated from mixed-effects models and the lowest AICc [@R-lme4, @R-AICcmodavg]. Specifically, we first determined the most important biophysical traits before including them in the model with the leaf hydraulic traits [**need to say why?**]
