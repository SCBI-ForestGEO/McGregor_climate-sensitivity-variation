
# Methods
## Study site
Research was conducted at the 25.6 ha ForestGEO (Global Earth Observatory) study plot at the Smithsonian Conservation Biology Institute (SCBI) in Virginia, USA (38°53'36.6"N, 78° 08'43.4"W) [@andersonteixeira_ctfs-forestgeo:_2015]. SCBI is located in the central Appalachian Mountains at the northern edge of Shenandoah National Park.  Elevations range from 273-338m above sea level [@gonzalezakre_patterns_2016] with a topographic relief of 65m [@bourg_initial_2013]. Dominant species include *Liriodendron tulipifera*,  oaks (*Quercus* spp.), and hickories (*Carya* spp.).

## Data collection and preparation
The SCBI ForestGEO plot was censused in 2008, 2013, and 2018 following standard ForestGEO protocols... [DETAILS]. Here, we use ...(*very brief summary of which data we use for what purpose-- but some will be better integrated with methods descriptions below*)

We analyzed tree-ring data from the twelve species contributing most to woody aboveground net primary productivity (ANPP_stem) (Table 1), which together comprised 97% of whole-ecosystem ANPP_stem between 2008 and 2013. Cores were collected in 2010-2011 or 2016-2017 from a breast height of 1.3m using a 5mm increment borer. In 2010-2011, cores were collected from randomly selected live trees were selected at random from each species in 2010-2011, with at least 30 of those trees having a diameter at breast height (DBH) of at least 10cm [@bourg_initial_2013]. In 2016-2017, cores were collected from all dead trees found in the annual mortality census [@gonzalezakre_patterns_2016]. Cores were sanded, measured, and cross-dated using standard procedures, as detailed in [@helcoski_growing_2019]. The resulting chronologies have been published in association with [@helcoski_growing_2019]: (GitHub URL), (ITRDB). 

Height measurements (n=# trees) were taken by several researchers between 2012 to 2019, and are archived in a public GitHub repository (*GitHUB URL*). Measurement methods included manual [@stovall_assessing_2018, NEON], digital rangefinders [@andersonteixeira_ctfs-forestgeo:_2015] (CITATION NOT APPROPRIATE), and automatic LiDAR [@stovall_terrestrial_2018]. Rangefinders either used the tangent method (Impulse 200LR, TruPulse 360R) or the sine method (Nikon ForestryPro) for calculating heights. The associated errors for using either method were acknowledged [@larjavaara_measuring_2013]. Species-specific height allometries were developed (Table S# - **ADD THIS TABLE TO SI**). For species that didn't have enough height measurements, heights were calculated from equations derived from all species in the study. 

For each tree, we combined tree-ring records and allometric equations of height and bark thickness to retroactively calculate DBH and estimate height for the years 1950-2009. Prior DBH was estimated using the following equation:

`diam_1999 = dbh2008 - 2*(bark.depth2008) - 2*(sum(ring.width1999:ring.width2008)) + 2*(bark.depth_1999)`  (**clean up this equation--put a generic year instead of 1999, use sum symbol, etc.**)

Here, *ring.width* was measured from cores. Bark thickness was estimated from species-specific allometries based on the bark thickness data of [@andersonteixeira_size-related_2015].
Specifically, we used linear regression equations on log-transformed data to relate bark thickness to DBH (Table S#- **create table to give these equations in SI**) and then used these to estimate bark thickness based on DBH. 

Crown positions were recorded in the field during the growing season of 2018 following the crown position protocol from [@jennings_assessing_1999], whereby positions were ranked as dominant, codominant, intermediate, or suppressed. As there was no way to retroactively estimate crown position, we assumed that 2018 crown position was reflective of each tree's position over the past 60 years. While some trees undoubtedly changed position, an analysis of canopy position relative to height (Fig. XX) and height change since *1959* indicated that change was likely slow. [**work on this-- provide details?**]

Elevation for the trees was extracted from a USGS DEM in ArcMap. Distance to water was calculated as the shortest distance in meters between each individual tree and the major streams running through the ForestGEO plot.

Hydraulic traits were collected from SCBI and are summarized in Table 1. In August 2018, we collected leaf samples from three individuals of each species ... (**Nobby's description of methods for the following**) 
1. PLA
2. LMA
4. Wood density
5. TLP
6. P50 = psi_0.5_kl50
7. P80 = psi_0.5_kl80 


**Table 1. Species analyzed here, listed in descending order of ANPP_stem. n cores and DBH range represented, and species traits** [*This replaces/combines the two remaining tables in this section. Suggested columns, with those to include only if they fit in parentheses: species, (stems >=10 cm per ha in plot), (ANPP_stem), n cores, DBH range of cores, (n cores in each canopy position) species means for each trait]


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

## Climate and drought years
[**add description of climate data used in Fig. 1, NEON vertical profiles**]

To determine focus drought years, we used the pointRes package [@R-pointRes] in R (version 3.5.2) to determine resilience metrics. Specifically, we calculated pointer years (main drought years) if >50% of the cored trees experienced <30% growth in a year compared to the previous 5 years. The top 6 pointer years were compared with PDSI (Palmer Drought Severity Index) values for the region, obtained from NOAA. Based on this comparison, we then took the top three pointer years of 1964-1966 (resistance metric averaged due to a single, prolonged drought), 1977, and 1999. [**This needs work. It's very complex to describe what you did, and in retrospect not well-aligned with our current analysis. It would be much more straightforward to simply combine ALL cores in the pointRes package to identify potential pointer years. Is this a lot of work? In any case, we should find a way to present this analysis in one paragraph (no tables!) of main body text. If you need to stick with species/canopy position-level analysis, any tables and detailed description should go in SI**]

**Figure 1. Time series of peak growing season (May-August) climate conditions and residual chronologies for each species.** Droughts analyzed here are indicated by dashed lines, and shading indicates the pre-drought period used in calculations of the resistance metric. Figure modified from [@helcoski_growing_2019].
![Time series of combined tree cores by species](tables_figures/Time_series_for_each_species.jpg){width=500px}

## Analysis
After the data was collected, linear mixed models were run with the resistance value of each tree for each pointer year (determined from the pointRes package) as the response variable. Traits and other geographic data were used as independent variables.

Traits' importance in predicting drought tolerance was calculated from mixed-effects models and the lowest AICc [@R-lme4, @R-AICcmodavg]. Specifically, we first determined the most important biophysical traits before including them in the model with the leaf hydraulic traits [**need to say why?**]
