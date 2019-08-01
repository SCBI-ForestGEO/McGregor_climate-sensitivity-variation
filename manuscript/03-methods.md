

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


\begin{table}[H]
\centering
\begin{tabular}{l|r|r|r|r|r|r}
\hline
sp & n\_cores & dominant & co-dominant & intermediate & suppressed & prior dead\\
\hline
caco & 13 & NA & 2 & 5 & 5 & 1\\
\hline
cagl & 31 & 1 & 8 & 16 & 5 & 1\\
\hline
caovl & 23 & 4 & 5 & 12 & 2 & NA\\
\hline
cato & 13 & NA & NA & 6 & 2 & 5\\
\hline
fagr & 80 & NA & 7 & 48 & 25 & NA\\
\hline
fram & 62 & NA & 17 & 19 & 14 & 12\\
\hline
juni & 31 & NA & 21 & 8 & NA & 2\\
\hline
litu & 98 & 9 & 29 & 25 & 30 & 5\\
\hline
qual & 61 & 4 & 34 & 20 & 3 & NA\\
\hline
qupr & 59 & 1 & 26 & 20 & 12 & NA\\
\hline
quru & 69 & 6 & 36 & 23 & 2 & 2\\
\hline
quve & 77 & 6 & 46 & 22 & 1 & 2\\
\hline
\end{tabular}
\end{table}

\begin{table}[H]
\centering
\begin{tabular}{l|l|r|r|r}
\hline
Trait & Unit & mean & min & max\\
\hline
Ring Porosity & ring, semi-ring, diffuse & NA & NA & NA\\
\hline
Percent Leaf Area & \% & 15.09 & 8.52 & 24.64\\
\hline
Leaf Mass Area & g/m2 & 53.50 & 30.68 & 75.80\\
\hline
Wood density & g/cm3 & 0.70 & 0.40 & 1.09\\
\hline
TLP & MPa & -2.36 & -2.76 & -1.92\\
\hline
\end{tabular}
\end{table}

### Climate and drought years
[**add description of climate data used in Fig. 1, NEON vertical profiles**]

To accurately understand climate sensitivity, this study used a specific definition of drought, which is not a common practice [@slette_how_2019]. We used the pointRes package [@R-pointRes] in R (version 3.5.3) to determine drought periods based on trees' drought resistance, which is defined by [@lloret_components_2011] as the ratio between the performance during and before the disturbance. Candidate drought years were defined if >50% of the cored trees experienced <30% growth in a year compared to the previous 5 years. These were then cross-validated with the regional Palmer Drought Severity Index (PDSI) values for each year, which yielded a set of three periods that were consistently shown as drought: 1964-1966, 1977, and 1999.

**Figure 1. Time series of peak growing season (May-August) climate conditions and residual chronologies for each species.** Droughts analyzed here are indicated by dashed lines, and shading indicates the pre-drought period used in calculations of the resistance metric. Figure modified from [@helcoski_growing_2019].
![Time series of combined tree cores by species](tables_figures/Time_series_for_each_species.jpg){width=500px}

### Analysis
Once the data was collected, linear mixed models were run following the order of the hypotheses as seen in Figure ??? [individual_tested_traits]. Using the [@R-pointRes] package, we set up models with the resistance value as the response variable, and each prediction's variable as the independent variable. Variables' importance in predicting drought tolerance was calculated from mixed-effects models and the lowest AICc [@R-lme4, @R-AICcmodavg].Null models were determined in order of the predictions. First, we analyzed the combined scenario to determine if "year" was significant. Upon establishing this, we tested height and DBH as size parameters. Although both were significant, height was kept due to its larger delta AICc compared with the null model. We then tested the remaining biophysical and hydraulic traits individually against a null model containing height and year. This yielded Figure ??? (cand_full). All variables with dAICc >2 were used as candidates for each scenario's best model (figure ???? (tested_traits_best))

\begin{table}[H]
\centering
\begin{tabular}{r|l|l|l|l|l|l|r|l|l|r|l|l|r|l|l|r|l|l}
\hline
prediction & variable & variable\_description & null\_model & tested\_model & null\_model\_year & tested\_model\_year & dAIC\_all & coef\_all & coef\_var\_all & dAIC\_1964.1966 & coef\_1964.1966 & coef\_var\_1964.1966 & dAIC\_1977 & coef\_1977 & coef\_var\_1977 & dAIC\_1999 & coef\_1999 & coef\_var\_1999\\
\hline
1.1 & year & drought.year & resist.value \textasciitilde{} (1|sp/tree) & resist.value \textasciitilde{} (1|sp/tree)+year & resist.value \textasciitilde{} (1|sp) & resist.value \textasciitilde{} (1|sp) & 35.834 & - & year1977 (-0.09), year1999 (-0.081) & 0.000 & NA & NA & 0.000 & NA & NA & 0.000 & NA & NA\\
\hline
2.1 & dbh.ln.cm & ln[DBH] & resist.value \textasciitilde{} year+(1|sp/tree) & resist.value \textasciitilde{} year+(1|sp/tree)+dbh.ln.cm & resist.value \textasciitilde{} (1|sp) & resist.value \textasciitilde{} (1|sp)+dbh.ln.cm & 7.411 & - & dbh.ln.cm (-0.035) & 18.276 & - & dbh.ln.cm (-0.082) & -0.947 & - & dbh.ln.cm (-0.021) & -1.918 & + & dbh.ln.cm (0.006)\\
\hline
2.2 & height.ln.m & ln[height] & resist.value \textasciitilde{} year+(1|sp/tree) & resist.value \textasciitilde{} year+(1|sp/tree)+height.ln.m & resist.value \textasciitilde{} (1|sp) & resist.value \textasciitilde{} (1|sp)+height.ln.m & 7.591 & - & height.ln.m (-0.058) & 17.788 & - & height.ln.m (-0.133) & -1.077 & - & height.ln.m (-0.032) & -2.022 & + & height.ln.m (0.002)\\
\hline
3.3 & position\_all & crown.position & resist.value \textasciitilde{} height.ln.m+year+(1|sp/tree) & resist.value \textasciitilde{} height.ln.m+year+(1|sp/tree)+position\_all & resist.value \textasciitilde{} height.ln.m+(1|sp) & resist.value \textasciitilde{} height.ln.m+(1|sp)+position\_all & 1.682 & - & position\_alldominant (-0.044), position\_allintermediate (-0.042), position\_allsuppressed (-0.053) & -2.548 & - & position\_alldominant (-0.055), position\_allintermediate (0.007), position\_allsuppressed (-0.044) & -0.700 & - & position\_alldominant (-0.076), position\_allintermediate (-0.033), position\_allsuppressed (0.029) & 4.087 & - & position\_alldominant (-0.003), position\_allintermediate (-0.082), position\_allsuppressed (-0.102)\\
\hline
3.4 & TWI & topographic.wetness.index & resist.value \textasciitilde{} height.ln.m+year+(1|sp/tree) & resist.value \textasciitilde{} height.ln.m+year+(1|sp/tree)+TWI & resist.value \textasciitilde{} height.ln.m+(1|sp) & resist.value \textasciitilde{} height.ln.m+(1|sp)+TWI & 2.631 & - & TWI (-0.009) & -0.563 & + & TWI (0.009) & 5.264 & - & TWI (-0.02) & 3.037 & - & TWI (-0.015)\\
\hline
4.1 & rp & ring.porosity & resist.value \textasciitilde{} height.ln.m+year+(1|sp/tree) & resist.value \textasciitilde{} height.ln.m+year+(1|sp/tree)+rp & resist.value \textasciitilde{} height.ln.m+(1|sp) & resist.value \textasciitilde{} height.ln.m+(1|sp)+rp & -3.553 & + & rpring (0.04), rpsemi-ring (0.013) & -2.161 & + & rpring (0.101), rpsemi-ring (0.015) & 0.895 & - & rpring (-0.19), rpsemi-ring (-0.147) & 4.083 & + & rpring (0.2), rpsemi-ring (0.151)\\
\hline
4.2 & PLA\_dry\_percent & percent.leaf.area & resist.value \textasciitilde{} height.ln.m+year+(1|sp/tree) & resist.value \textasciitilde{} height.ln.m+year+(1|sp/tree)+PLA\_dry\_percent & resist.value \textasciitilde{} height.ln.m+(1|sp) & resist.value \textasciitilde{} height.ln.m+(1|sp)+PLA\_dry\_percent & 4.413 & - & PLA\_dry\_percent (-0.011) & 5.825 & - & PLA\_dry\_percent (-0.016) & -0.190 & - & PLA\_dry\_percent (-0.01) & -0.701 & - & PLA\_dry\_percent (-0.007)\\
\hline
4.3 & LMA\_g\_per\_m2 & leaf.mass.area & resist.value \textasciitilde{} height.ln.m+year+(1|sp/tree) & resist.value \textasciitilde{} height.ln.m+year+(1|sp/tree)+LMA\_g\_per\_m2 & resist.value \textasciitilde{} height.ln.m+(1|sp) & resist.value \textasciitilde{} height.ln.m+(1|sp)+LMA\_g\_per\_m2 & -1.895 & + & LMA\_g\_per\_m2 (0.001) & -1.075 & + & LMA\_g\_per\_m2 (0.002) & -1.698 & - & LMA\_g\_per\_m2 (-0.001) & -1.985 & + & LMA\_g\_per\_m2 (0)\\
\hline
4.4 & mean\_TLP\_Mpa & mean.turgor.loss.point & resist.value \textasciitilde{} height.ln.m+year+(1|sp/tree) & resist.value \textasciitilde{} height.ln.m+year+(1|sp/tree)+mean\_TLP\_Mpa & resist.value \textasciitilde{} height.ln.m+(1|sp) & resist.value \textasciitilde{} height.ln.m+(1|sp)+mean\_TLP\_Mpa & 4.580 & - & mean\_TLP\_Mpa (-0.207) & 1.352 & - & mean\_TLP\_Mpa (-0.217) & 1.008 & - & mean\_TLP\_Mpa (-0.236) & 0.132 & - & mean\_TLP\_Mpa (-0.177)\\
\hline
4.5 & WD\_g\_per\_cm3 & wood.density & resist.value \textasciitilde{} height.ln.m+year+(1|sp/tree) & resist.value \textasciitilde{} height.ln.m+year+(1|sp/tree)+WD\_g\_per\_cm3 & resist.value \textasciitilde{} height.ln.m+(1|sp) & resist.value \textasciitilde{} height.ln.m+(1|sp)+WD\_g\_per\_cm3 & -2.018 & + & WD\_g\_per\_cm3 (0.005) & -1.960 & - & WD\_g\_per\_cm3 (-0.049) & -1.236 & - & WD\_g\_per\_cm3 (-0.175) & 0.171 & + & WD\_g\_per\_cm3 (0.247)\\
\hline
\end{tabular}
\end{table}

\begin{table}[H]
\centering
\begin{tabular}{r|l|l|l}
\hline
prediction & variable & variable\_description & top\_model\\
\hline
1.1 & year & drought.year & all\\
\hline
2.2 & height.ln.m & ln[height] & all\\
\hline
2.2 & height.ln.m & ln[height] & 1966\\
\hline
3.3 & position\_all & crown.position & 1999\\
\hline
3.4 & TWI.ln & topographic.wetness.index & all\\
\hline
3.4 & TWI.ln & topographic.wetness.index & 1977\\
\hline
3.4 & TWI.ln & topographic.wetness.index & 1999\\
\hline
4.1 & rp & ring.porosity & 1999\\
\hline
4.2 & PLA\_dry\_percent & percent.leaf.area & all\\
\hline
4.2 & PLA\_dry\_percent & percent.leaf.area & 1966\\
\hline
4.4 & mean\_TLP\_Mpa & mean.turgor.loss.point & all\\
\hline
\end{tabular}
\end{table}

\begin{table}[H]
\centering
\begin{tabular}{l|r|l}
\hline
best\_model & r2 & scenario\\
\hline
resist.value \textasciitilde{} year+height.ln.m+position\_all+TWI+PLA\_dry\_percent+mean\_TLP\_Mpa+(1|sp/tree) & 0.13 & all droughts\\
\hline
resist.value \textasciitilde{} height.ln.m+rp+PLA\_dry\_percent+(1|sp) & 0.24 & 1964-1966\\
\hline
resist.value \textasciitilde{} TWI+rp+mean\_TLP\_Mpa+(1|sp) & 0.21 & 1977\\
\hline
resist.value \textasciitilde{} height.ln.m+position\_all+TWI+rp+PLA\_dry\_percent+(1|sp) & 0.25 & 1999\\
\hline
\end{tabular}
\end{table}
