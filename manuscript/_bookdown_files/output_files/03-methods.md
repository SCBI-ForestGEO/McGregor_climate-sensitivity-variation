

# Methods
### Study site
Research was conducted at the 25.6 ha ForestGEO (Global Earth Observatory) study plot at the Smithsonian Conservation Biology Institute (SCBI) in Virginia, USA (38°53'36.6"N, 78° 08'43.4"W) [@andersonteixeira_ctfs-forestgeo:_2015]. SCBI is located in the central Appalachian Mountains at the northern edge of Shenandoah National Park.  Elevations range from 273-338m above sea level [@gonzalezakre_patterns_2016] with a topographic relief of 65m [@bourg_initial_2013]. Dominant species include *Liriodendron tulipifera*,  oaks (*Quercus* spp.), and hickories (*Carya* spp.).

### Data collection and preparation
The SCBI ForestGEO plot was censused in 2008, 2013, and 2018 following standard ForestGEO protocols... [DETAILS]. Here, we use ...(*very brief summary of which data we use for what purpose-- but some will be better integrated with methods descriptions below*)

We analyzed tree-ring data from the twelve species contributing most to woody aboveground net primary productivity (ANPP_stem) (Table 1), which together comprised 97% of whole-ecosystem ANPP_stem between 2008 and 2013. Cores were collected in 2010-2011 or 2016-2017 from a breast height of 1.3m using a 5mm increment borer. In 2010-2011, cores were collected from randomly selected live trees were selected at random from each species in 2010-2011, with at least 30 of those trees having a diameter at breast height (DBH) of at least 10cm [@bourg_initial_2013]. In 2016-2017, cores were collected from all dead trees found in the annual mortality census [@gonzalezakre_patterns_2016]. Cores were sanded, measured, and cross-dated using standard procedures, as detailed in [@helcoski_growing_2019]. The resulting chronologies have been published in association with [@helcoski_growing_2019]: (GitHub URL), (ITRDB). 

Height measurements (n=# trees) were taken by several researchers between 2012 to 2019, and are archived in a public GitHub repository (*GitHUB URL*). Measurement methods included manual [@stovall_assessing_2018, NEON], digital rangefinders [@andersonteixeira_size-related_2015], and automatic LiDAR [@stovall_terrestrial_2018]. Rangefinders either used the tangent method (Impulse 200LR, TruPulse 360R) or the sine method (Nikon ForestryPro) for calculating heights. The associated errors for using either method were acknowledged [@larjavaara_measuring_2013]. Species-specific height allometries were developed (Table S# - **ADD THIS TABLE TO SI**). For species that didn't have enough height measurements, heights were calculated from equations derived from all species in the study. 

For each tree, we combined tree-ring records and allometric equations of height and bark thickness to retroactively calculate DBH and estimate height for the years 1950-2009. Prior DBH was estimated using the following equation, using 2008 as the earliest year for having reliable DBH measurements:

$$
diamYEAR = dbh2008 - 2*(bark.depth2008) - 2*\Sigma(ring.widthYEAR:ring.width2008) + 2*(bark.depth_YEAR)
$$

Here, *ring.width* was measured from cores. Bark thickness was estimated from species-specific allometries based on the bark thickness data of [@andersonteixeira_size-related_2015].
Specifically, we used linear regression equations on log-transformed data to relate bark thickness to DBH (Table S#- **create table to give these equations in SI**) and then used these to estimate bark thickness based on DBH. 

Crown positions were recorded in the field during the growing season of 2018 following the crown position protocol from [@jennings_assessing_1999], whereby positions were ranked as dominant, codominant, intermediate, or suppressed. As there was no way to retroactively estimate crown position, we assumed that 2018 crown position was reflective of each tree's position over the past 60 years. While some trees undoubtedly changed position, an analysis of crown position relative to height (Fig. XX) and height change since *1959* indicated that change was likely slow. [**work on this-- provide details?**]

Topographic wetness index (TWI) was calculated using the [@R-dynatopmodel] package in R.

Hydraulic traits were collected from SCBI and are summarized in Table 1. In August 2018, we collected leaf samples from three individuals of each species ... (**Nobby's description of methods for the following**) 
1. PLA
2. LMA
4. Wood density
5. TLP


**Table 1. Species analyzed here, listed in descending order of ANPP_stem. n cores and DBH range represented, and species traits** [*This replaces/combines the two remaining tables in this section. Suggested columns, with those to include only if they fit in parentheses: species, (stems >=10 cm per ha in plot), (ANPP_stem), n cores, DBH range of cores, (n cores in each crown position) species means for each trait]


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
</tbody>
</table>

### Climate and drought years
[**add description of climate data used in Fig. 1, NEON vertical profiles**]

To accurately understand climate sensitivity, this study used a specific definition of drought, which is not a common practice [@slette_how_2019]. We used the pointRes package [@R-pointRes] in R (version 3.5.3) to determine drought periods based on trees' drought resistance, which is defined by [@lloret_components_2011] as the ratio between the performance during and before the disturbance. Candidate drought years were defined if >50% of the cored trees experienced <30% growth in a year compared to the previous 5 years. These were then cross-validated with the regional Palmer Drought Severity Index (PDSI) values for each year, which yielded a set of three periods that were consistently shown as drought: 1964-1966, 1977, and 1999.

**Figure 1. Time series of peak growing season (May-August) climate conditions and residual chronologies for each species.** Droughts analyzed here are indicated by dashed lines, and shading indicates the pre-drought period used in calculations of the resistance metric. Figure modified from [@helcoski_growing_2019].
![Time series of combined tree cores by species](tables_figures/Time_series_for_each_species.jpg){width=500px}

### Analysis
Once the data was collected, linear mixed models were run following the order of the hypotheses as seen in Figure ??? [individual_tested_traits]. Using the [@R-pointRes] package, we set up models with the resistance value as the response variable, and each prediction's variable as the independent variable. Variables' importance in predicting drought tolerance was calculated from mixed-effects models and the lowest AICc [@R-lme4, @R-AICcmodavg].Null models were determined in order of the predictions. First, we analyzed the combined scenario to determine if "year" was significant. Upon establishing this, we tested height and DBH as size parameters. Although both were significant, height was kept due to its larger delta AICc compared with the null model. We then tested the remaining biophysical and hydraulic traits individually against a null model containing height and year. This yielded Figure ??? (cand_full). All variables with dAICc >2 were used as candidates for each scenario's best model (figure ???? (tested_traits_best))

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> prediction </th>
   <th style="text-align:left;"> variable </th>
   <th style="text-align:left;"> variable_description </th>
   <th style="text-align:left;"> null_model </th>
   <th style="text-align:left;"> tested_model </th>
   <th style="text-align:left;"> null_model_year </th>
   <th style="text-align:left;"> tested_model_year </th>
   <th style="text-align:right;"> dAIC_all </th>
   <th style="text-align:left;"> coef_all </th>
   <th style="text-align:left;"> coef_var_all </th>
   <th style="text-align:right;"> dAIC_1964.1966 </th>
   <th style="text-align:left;"> coef_1964.1966 </th>
   <th style="text-align:left;"> coef_var_1964.1966 </th>
   <th style="text-align:right;"> dAIC_1977 </th>
   <th style="text-align:left;"> coef_1977 </th>
   <th style="text-align:left;"> coef_var_1977 </th>
   <th style="text-align:right;"> dAIC_1999 </th>
   <th style="text-align:left;"> coef_1999 </th>
   <th style="text-align:left;"> coef_var_1999 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1.1 </td>
   <td style="text-align:left;"> year </td>
   <td style="text-align:left;"> drought.year </td>
   <td style="text-align:left;"> resist.value ~ (1|sp/tree) </td>
   <td style="text-align:left;"> resist.value ~ (1|sp/tree)+year </td>
   <td style="text-align:left;"> resist.value ~ (1|sp) </td>
   <td style="text-align:left;"> resist.value ~ (1|sp) </td>
   <td style="text-align:right;"> 35.834 </td>
   <td style="text-align:left;"> - </td>
   <td style="text-align:left;"> year1977 (-0.09), year1999 (-0.081) </td>
   <td style="text-align:right;"> 0.000 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0.000 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0.000 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2.1 </td>
   <td style="text-align:left;"> dbh.ln.cm </td>
   <td style="text-align:left;"> ln[DBH] </td>
   <td style="text-align:left;"> resist.value ~ year+(1|sp/tree) </td>
   <td style="text-align:left;"> resist.value ~ year+(1|sp/tree)+dbh.ln.cm </td>
   <td style="text-align:left;"> resist.value ~ (1|sp) </td>
   <td style="text-align:left;"> resist.value ~ (1|sp)+dbh.ln.cm </td>
   <td style="text-align:right;"> 7.411 </td>
   <td style="text-align:left;"> - </td>
   <td style="text-align:left;"> dbh.ln.cm (-0.035) </td>
   <td style="text-align:right;"> 18.276 </td>
   <td style="text-align:left;"> - </td>
   <td style="text-align:left;"> dbh.ln.cm (-0.082) </td>
   <td style="text-align:right;"> -0.947 </td>
   <td style="text-align:left;"> - </td>
   <td style="text-align:left;"> dbh.ln.cm (-0.021) </td>
   <td style="text-align:right;"> -1.918 </td>
   <td style="text-align:left;"> + </td>
   <td style="text-align:left;"> dbh.ln.cm (0.006) </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2.2 </td>
   <td style="text-align:left;"> height.ln.m </td>
   <td style="text-align:left;"> ln[height] </td>
   <td style="text-align:left;"> resist.value ~ year+(1|sp/tree) </td>
   <td style="text-align:left;"> resist.value ~ year+(1|sp/tree)+height.ln.m </td>
   <td style="text-align:left;"> resist.value ~ (1|sp) </td>
   <td style="text-align:left;"> resist.value ~ (1|sp)+height.ln.m </td>
   <td style="text-align:right;"> 7.591 </td>
   <td style="text-align:left;"> - </td>
   <td style="text-align:left;"> height.ln.m (-0.058) </td>
   <td style="text-align:right;"> 17.788 </td>
   <td style="text-align:left;"> - </td>
   <td style="text-align:left;"> height.ln.m (-0.133) </td>
   <td style="text-align:right;"> -1.077 </td>
   <td style="text-align:left;"> - </td>
   <td style="text-align:left;"> height.ln.m (-0.032) </td>
   <td style="text-align:right;"> -2.022 </td>
   <td style="text-align:left;"> + </td>
   <td style="text-align:left;"> height.ln.m (0.002) </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3.3 </td>
   <td style="text-align:left;"> position_all </td>
   <td style="text-align:left;"> crown.position </td>
   <td style="text-align:left;"> resist.value ~ height.ln.m+year+(1|sp/tree) </td>
   <td style="text-align:left;"> resist.value ~ height.ln.m+year+(1|sp/tree)+position_all </td>
   <td style="text-align:left;"> resist.value ~ height.ln.m+(1|sp) </td>
   <td style="text-align:left;"> resist.value ~ height.ln.m+(1|sp)+position_all </td>
   <td style="text-align:right;"> 1.682 </td>
   <td style="text-align:left;"> - </td>
   <td style="text-align:left;"> position_alldominant (-0.044), position_allintermediate (-0.042), position_allsuppressed (-0.053) </td>
   <td style="text-align:right;"> -2.548 </td>
   <td style="text-align:left;"> - </td>
   <td style="text-align:left;"> position_alldominant (-0.055), position_allintermediate (0.007), position_allsuppressed (-0.044) </td>
   <td style="text-align:right;"> -0.700 </td>
   <td style="text-align:left;"> - </td>
   <td style="text-align:left;"> position_alldominant (-0.076), position_allintermediate (-0.033), position_allsuppressed (0.029) </td>
   <td style="text-align:right;"> 4.087 </td>
   <td style="text-align:left;"> - </td>
   <td style="text-align:left;"> position_alldominant (-0.003), position_allintermediate (-0.082), position_allsuppressed (-0.102) </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3.4 </td>
   <td style="text-align:left;"> TWI </td>
   <td style="text-align:left;"> topographic.wetness.index </td>
   <td style="text-align:left;"> resist.value ~ height.ln.m+year+(1|sp/tree) </td>
   <td style="text-align:left;"> resist.value ~ height.ln.m+year+(1|sp/tree)+TWI </td>
   <td style="text-align:left;"> resist.value ~ height.ln.m+(1|sp) </td>
   <td style="text-align:left;"> resist.value ~ height.ln.m+(1|sp)+TWI </td>
   <td style="text-align:right;"> 2.631 </td>
   <td style="text-align:left;"> - </td>
   <td style="text-align:left;"> TWI (-0.009) </td>
   <td style="text-align:right;"> -0.563 </td>
   <td style="text-align:left;"> + </td>
   <td style="text-align:left;"> TWI (0.009) </td>
   <td style="text-align:right;"> 5.264 </td>
   <td style="text-align:left;"> - </td>
   <td style="text-align:left;"> TWI (-0.02) </td>
   <td style="text-align:right;"> 3.037 </td>
   <td style="text-align:left;"> - </td>
   <td style="text-align:left;"> TWI (-0.015) </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4.1 </td>
   <td style="text-align:left;"> rp </td>
   <td style="text-align:left;"> ring.porosity </td>
   <td style="text-align:left;"> resist.value ~ height.ln.m+year+(1|sp/tree) </td>
   <td style="text-align:left;"> resist.value ~ height.ln.m+year+(1|sp/tree)+rp </td>
   <td style="text-align:left;"> resist.value ~ height.ln.m+(1|sp) </td>
   <td style="text-align:left;"> resist.value ~ height.ln.m+(1|sp)+rp </td>
   <td style="text-align:right;"> -3.553 </td>
   <td style="text-align:left;"> + </td>
   <td style="text-align:left;"> rpring (0.04), rpsemi-ring (0.013) </td>
   <td style="text-align:right;"> -2.161 </td>
   <td style="text-align:left;"> + </td>
   <td style="text-align:left;"> rpring (0.101), rpsemi-ring (0.015) </td>
   <td style="text-align:right;"> 0.895 </td>
   <td style="text-align:left;"> - </td>
   <td style="text-align:left;"> rpring (-0.19), rpsemi-ring (-0.147) </td>
   <td style="text-align:right;"> 4.083 </td>
   <td style="text-align:left;"> + </td>
   <td style="text-align:left;"> rpring (0.2), rpsemi-ring (0.151) </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4.2 </td>
   <td style="text-align:left;"> PLA_dry_percent </td>
   <td style="text-align:left;"> percent.leaf.area </td>
   <td style="text-align:left;"> resist.value ~ height.ln.m+year+(1|sp/tree) </td>
   <td style="text-align:left;"> resist.value ~ height.ln.m+year+(1|sp/tree)+PLA_dry_percent </td>
   <td style="text-align:left;"> resist.value ~ height.ln.m+(1|sp) </td>
   <td style="text-align:left;"> resist.value ~ height.ln.m+(1|sp)+PLA_dry_percent </td>
   <td style="text-align:right;"> 4.413 </td>
   <td style="text-align:left;"> - </td>
   <td style="text-align:left;"> PLA_dry_percent (-0.011) </td>
   <td style="text-align:right;"> 5.825 </td>
   <td style="text-align:left;"> - </td>
   <td style="text-align:left;"> PLA_dry_percent (-0.016) </td>
   <td style="text-align:right;"> -0.190 </td>
   <td style="text-align:left;"> - </td>
   <td style="text-align:left;"> PLA_dry_percent (-0.01) </td>
   <td style="text-align:right;"> -0.701 </td>
   <td style="text-align:left;"> - </td>
   <td style="text-align:left;"> PLA_dry_percent (-0.007) </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4.3 </td>
   <td style="text-align:left;"> LMA_g_per_m2 </td>
   <td style="text-align:left;"> leaf.mass.area </td>
   <td style="text-align:left;"> resist.value ~ height.ln.m+year+(1|sp/tree) </td>
   <td style="text-align:left;"> resist.value ~ height.ln.m+year+(1|sp/tree)+LMA_g_per_m2 </td>
   <td style="text-align:left;"> resist.value ~ height.ln.m+(1|sp) </td>
   <td style="text-align:left;"> resist.value ~ height.ln.m+(1|sp)+LMA_g_per_m2 </td>
   <td style="text-align:right;"> -1.895 </td>
   <td style="text-align:left;"> + </td>
   <td style="text-align:left;"> LMA_g_per_m2 (0.001) </td>
   <td style="text-align:right;"> -1.075 </td>
   <td style="text-align:left;"> + </td>
   <td style="text-align:left;"> LMA_g_per_m2 (0.002) </td>
   <td style="text-align:right;"> -1.698 </td>
   <td style="text-align:left;"> - </td>
   <td style="text-align:left;"> LMA_g_per_m2 (-0.001) </td>
   <td style="text-align:right;"> -1.985 </td>
   <td style="text-align:left;"> + </td>
   <td style="text-align:left;"> LMA_g_per_m2 (0) </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4.4 </td>
   <td style="text-align:left;"> mean_TLP_Mpa </td>
   <td style="text-align:left;"> mean.turgor.loss.point </td>
   <td style="text-align:left;"> resist.value ~ height.ln.m+year+(1|sp/tree) </td>
   <td style="text-align:left;"> resist.value ~ height.ln.m+year+(1|sp/tree)+mean_TLP_Mpa </td>
   <td style="text-align:left;"> resist.value ~ height.ln.m+(1|sp) </td>
   <td style="text-align:left;"> resist.value ~ height.ln.m+(1|sp)+mean_TLP_Mpa </td>
   <td style="text-align:right;"> 4.580 </td>
   <td style="text-align:left;"> - </td>
   <td style="text-align:left;"> mean_TLP_Mpa (-0.207) </td>
   <td style="text-align:right;"> 1.352 </td>
   <td style="text-align:left;"> - </td>
   <td style="text-align:left;"> mean_TLP_Mpa (-0.217) </td>
   <td style="text-align:right;"> 1.008 </td>
   <td style="text-align:left;"> - </td>
   <td style="text-align:left;"> mean_TLP_Mpa (-0.236) </td>
   <td style="text-align:right;"> 0.132 </td>
   <td style="text-align:left;"> - </td>
   <td style="text-align:left;"> mean_TLP_Mpa (-0.177) </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4.5 </td>
   <td style="text-align:left;"> WD_g_per_cm3 </td>
   <td style="text-align:left;"> wood.density </td>
   <td style="text-align:left;"> resist.value ~ height.ln.m+year+(1|sp/tree) </td>
   <td style="text-align:left;"> resist.value ~ height.ln.m+year+(1|sp/tree)+WD_g_per_cm3 </td>
   <td style="text-align:left;"> resist.value ~ height.ln.m+(1|sp) </td>
   <td style="text-align:left;"> resist.value ~ height.ln.m+(1|sp)+WD_g_per_cm3 </td>
   <td style="text-align:right;"> -2.018 </td>
   <td style="text-align:left;"> + </td>
   <td style="text-align:left;"> WD_g_per_cm3 (0.005) </td>
   <td style="text-align:right;"> -1.960 </td>
   <td style="text-align:left;"> - </td>
   <td style="text-align:left;"> WD_g_per_cm3 (-0.049) </td>
   <td style="text-align:right;"> -1.236 </td>
   <td style="text-align:left;"> - </td>
   <td style="text-align:left;"> WD_g_per_cm3 (-0.175) </td>
   <td style="text-align:right;"> 0.171 </td>
   <td style="text-align:left;"> + </td>
   <td style="text-align:left;"> WD_g_per_cm3 (0.247) </td>
  </tr>
</tbody>
</table>

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> prediction </th>
   <th style="text-align:left;"> variable </th>
   <th style="text-align:left;"> variable_description </th>
   <th style="text-align:left;"> top_model </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1.1 </td>
   <td style="text-align:left;"> year </td>
   <td style="text-align:left;"> drought.year </td>
   <td style="text-align:left;"> all </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2.2 </td>
   <td style="text-align:left;"> height.ln.m </td>
   <td style="text-align:left;"> ln[height] </td>
   <td style="text-align:left;"> all </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2.2 </td>
   <td style="text-align:left;"> height.ln.m </td>
   <td style="text-align:left;"> ln[height] </td>
   <td style="text-align:left;"> 1966 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3.3 </td>
   <td style="text-align:left;"> position_all </td>
   <td style="text-align:left;"> crown.position </td>
   <td style="text-align:left;"> 1999 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3.4 </td>
   <td style="text-align:left;"> TWI.ln </td>
   <td style="text-align:left;"> topographic.wetness.index </td>
   <td style="text-align:left;"> all </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3.4 </td>
   <td style="text-align:left;"> TWI.ln </td>
   <td style="text-align:left;"> topographic.wetness.index </td>
   <td style="text-align:left;"> 1977 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3.4 </td>
   <td style="text-align:left;"> TWI.ln </td>
   <td style="text-align:left;"> topographic.wetness.index </td>
   <td style="text-align:left;"> 1999 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4.1 </td>
   <td style="text-align:left;"> rp </td>
   <td style="text-align:left;"> ring.porosity </td>
   <td style="text-align:left;"> 1999 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4.2 </td>
   <td style="text-align:left;"> PLA_dry_percent </td>
   <td style="text-align:left;"> percent.leaf.area </td>
   <td style="text-align:left;"> all </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4.2 </td>
   <td style="text-align:left;"> PLA_dry_percent </td>
   <td style="text-align:left;"> percent.leaf.area </td>
   <td style="text-align:left;"> 1966 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4.4 </td>
   <td style="text-align:left;"> mean_TLP_Mpa </td>
   <td style="text-align:left;"> mean.turgor.loss.point </td>
   <td style="text-align:left;"> all </td>
  </tr>
</tbody>
</table>

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> best_model </th>
   <th style="text-align:right;"> r2 </th>
   <th style="text-align:left;"> scenario </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> resist.value ~ year+height.ln.m+position_all+TWI+PLA_dry_percent+mean_TLP_Mpa+(1|sp/tree) </td>
   <td style="text-align:right;"> 0.13 </td>
   <td style="text-align:left;"> all droughts </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resist.value ~ height.ln.m+rp+PLA_dry_percent+(1|sp) </td>
   <td style="text-align:right;"> 0.24 </td>
   <td style="text-align:left;"> 1964-1966 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resist.value ~ TWI+rp+mean_TLP_Mpa+(1|sp) </td>
   <td style="text-align:right;"> 0.21 </td>
   <td style="text-align:left;"> 1977 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resist.value ~ height.ln.m+position_all+TWI+rp+PLA_dry_percent+(1|sp) </td>
   <td style="text-align:right;"> 0.25 </td>
   <td style="text-align:left;"> 1999 </td>
  </tr>
</tbody>
</table>
