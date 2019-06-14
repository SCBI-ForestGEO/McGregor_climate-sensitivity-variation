
---
title: "Tree size, exposure, and hydraulic traits interactively shape drought response in a temperate broadleaf forest"
author: "Ian McGregor, Ryan Helcoski, Norbert Kunert, Alan Tepley, Valentine Herrmann, Neil Pederson, Lawren Sack, Joseph Zailaa, Krista Anderson-Teixeira"
output: 
  bookdown::html_book:
    theme: united
  bookdoown::gitbook:
    highlight: tango
documentclass: book
bibliography: [book.bib, packages.bib]
biblio-style: apalike
link-citations: yes
github-repo: manuscript
description: "Article for Ian's research during his time in Anderson-Teixeira lab"
---



<!--chapter:end:index.Rmd-->


# Abstract

something something something

<!--chapter:end:01-abstract.Rmd-->


# Introduction

Understanding how and why trees respond to drought is critical to predicting forest drought responses and climate change feedbacks.

Forests are diverse in terms of tree sizes and functional traits, and it is known that trees varying in size and functional traits respond differently to drought. Therefore, in order to understand whole-forest response to drought, we need to know how responses vary by tree size/ species. To do so, there are three fundamental questions that must be addressed:

*First, what drives the observed tendency for large trees to suffer more during drought?*  
Bennett et al. 2015 showed that in forests globally, large trees suffer greater growth reductions during drought. However, this analysis quantified tree size based on DBH, which has no direct mechanistic meaning. This study proposed two major mechanisms (besides insects): (1) inherently greater biophysical challenge of being tall; (2) greater exposure of the crowns of large trees. It is also expected that roots play a role. These hypotheses need to be tested.

*Second, how do species' traits influence drought response?* 
Analyzed drought responses on the species level doesn't get at mechanisms and is not feasible in diverse forests. The solution is a trait-based approach. Leaf hydraulic traits hold more promise than more commonly/ traditionally measured traits such as wood density and SLA (Medeiros et al.). 

*Third, how do tree size and functional traits interact to influence drought response?*
Its possible that pattern observed by Bennett et al. could be caused by smaller trees being more drought resistant. Alternatively, larger trees may have more drought-resistant traits.

*Questions & Hypotheses*


**1. Why do larger trees suffer greater growth declines during drought?**

1.0. Our forest displays the same trend as most forests globally (Bennett et al. 2015). (Note that Bennett et al. 2015 identified only one study on tree growth responses to drought in the Eastern US temperate deciduous biome. We know little about how tree size shapes drought response in this biome.)

1.1. Height is a strong predictor of drought stress.

    * P1.1-Drought response increases with height at time of drought.

1.2. Large trees suffer more during drought because of greater exposure (to radiation, wind, etc.)--either in relation to neighboring trees or because of position on landscape.

    * P1.2a- Trees currently in a dominant canopy position suffered more during drought. 
    
1.3. Rooting volume/depth relative to water sources are critical in drought response. Effects of drought on larger trees are mediated by the fact that large trees have better access to water.
  
     * P1.3a- drought response increases with elevation and/or distance from stream
     * P1.3b- There is a dbh\*elev interaction, elevation (relative to stream?) matters less for big trees with larger (and potentially deeper) root system.

**2.Do species functional traits predict drought response?**

     * P2.1- TLP correlates negatively with drought resistance (NEVER tested), 
     * P2.2- diffuse porous species are more sensitive than ring porous (previously observed in eastern dec forests- Elliot et al. 2015, Friedrichs et al. 2009)

**3.How are functional traits distributed across size classes, and how does this affect size-resistance relationship?** (Reviewers of Bennett et al. 2015 suggested that larger trees may suffer more because larger trees tend to be species with more drought-sensitive traits. *Conversely, larger size classes may have more drought-resistant species because of greater biophysical challenges.*

     * P3.1a- TLP is lower (larger negative) in smaller/ understory trees
     * P3.1a- diffuse porous species more common in understory 
   
*For the above, I think it would be more interesting to test the converse prediction, which appears to be true. However, it should sample from tree population based on census data, not cores.*
   
     * P3.2- Inclusion of TLP / rp in model eliminates (or significantly reduces) effect of tree size.

<!--chapter:end:02-intro.Rmd-->


# Methods

Research was conducted at the Smithsonian Conservation Biology Institute (SCBI) in Virginia, USA, with cores taken from trees on the 25.6 ha ForestGEO (Global Earth Observatory) study plot [@andersonteixeira_ctfs-forestgeo:_2015]. SCBI (38°53'36.6"N, 78° 08'43.4"W) [@gonzalezakre_patterns_2016] is located in the central Appalachian Mountains at the northern edge of Shenandoah National Park. Primary forest composition in the plot is oak-hickory, while elevations range from 273-338m above sea level [@gonzalezakre_patterns_2016] with a topographic relief of 65m (Bourg et al 2013).

## Data Collection
Cores were collected in 2010-2011 or 2016-2017 from a breast height of 1.3m using a 5mm increment borer. Live trees were selected at random from each species in 2010-2011, with at least 30 of those trees having a diameter at breast height (DBH) of at least 10cm (Bourg et al 2013). In 2016-2017, cores were collected from all dead trees found in the annual mortality census [@gonzalezakre_patterns_2016]. They were sanded, measured, and standardized in the same process as seen in Helcoski et al 2019.

Cores were then split into canopy and subcanopy groupings and processed with ARSTAN. Using CRU data **with Valentine's quilts for each grouping**, it was found that all climate variables tested were significant by tree position when using a linear mixed model (**this was done early-on, included here because unsure if will keep for full analysis**).

To determine focus drought years, we used the pointRes package in R (version 3.5.2) to determine resilience metrics. Specifically, we calculated pointer years (main drought years) if >50% of the cored trees experienced <30% growth in a year compared to the past 5 years. The top 6 pointer years were compared with PDSI (Palmer Drought Severity Index) values for the region, obtained from NOAA. For ease of research, we then took the top three pointer years of 1964-1966 (resistance metric averaged due to a single, prolonged drought), 1977, and 1999.

Individual cores were then grouped by canopy or subcanopy before being processed by ARSTAN, and species inclusion in the study was dependent on the robustness of each species’ cores. In total, 14 species had cores that were considered complete and usable, as seen in Table 1.

|**Species**|**n_cores.canopy**|**n_cores.subcanopy**|**included**
|----------|-------------|---------------|-----------|
*Carya cordiformis*|2|11| sub-canopy only
*Carya glabra*|9|22|yes
*Carya ovalis*|9|14|yes
*Carya tomentosa*|0|13|sub-canopy only
*Fagus grandifolia*|7|73|yes
*Fraxinus americana*|20|42|yes
*Fraxinus nigra*|1|11| sub-canopy only
*Juglans nigra*|22|9|yes
*Liriodendron tulipifera*|38|60|yes
*Pinus strobus*|14|21|no
*Quercus alba*|38|23|yes
*Quercus prinus*|27|32|yes
*Quercus rubra*|43|26|yes
*Quercus velutina*|54|23|yes




### Biophysical traits
Canopy classes were observed in the field during the growing season of 2018 following the crown position protocol from [@jennings_assessing_1999], whereby positions were ranked as dominant, codominant, intermediate, or suppressed. For ease of core analysis, these labels were condensed to canopy (including dominant and codominant) and subcanopy (intermediate and suppressed).

Elevation for the trees was extracted from a USGS DEM in ArcMap. Distance to water was calculated as the shortest distance in meters between each individual tree and the major streams running through the ForestGEO plot.

Height data for trees was collected from several researchers at SCBI from 2012 to 2019, with methods including manual [@stovall_assessing_2018, NEON], digital rangefinders [@andersonteixeira_ctfs-forestgeo:_2015], and automatic LiDAR [@stovall_terrestrial_2018]. Rangefinders either used the tangent method (Impulse 200LR, TruPulse 360R) or the sine method (Nikon ForestryPro) for calculating heights. The associated errors for using either method were acknowledged [@larjavaara_measuring_2013]. Log-log, species-specific regression equations were developed, which were used to calculate heights of the study trees. For species that didn't have enough height measurements, heights were calculated from equations derived from all species in the study.

Equations required the DBH in each individual drought year. As part of the ForestGEO five-year census, there are DBH(diameter at breast height) measurements for all stems. Using the data from 2008 and a dataset of bark thickness for the plot collected from previous studies, we devised an equation to retroactively calculate DBH for all trees,

`diam_nobark_1999 = dbh2008 - 2*(bark.depth2008) - 2*(sum(ring.width1999:ring.width2008))`

First, we generated log-log regression equations for bark thickness based on DBH, and then determined mean bark thickness per species. Ring widths were determined from the processed cores, and once the diameter without bark was calculated for a certain year, we added the mean bark thickness from the regression equations.

### Leaf Hydraulic Traits
Hydraulic traits were collected from SCBI and are summarized in Table 1.

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Trait </th>
   <th style="text-align:left;"> Unit </th>
   <th style="text-align:left;"> Source </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Ring Porosity </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> [@andersonteixeira_ctfs-forestgeo:_2015] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Percent Leaf Area </td>
   <td style="text-align:left;"> % </td>
   <td style="text-align:left;"> Kunert et al 2019 [in prep] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Leaf Mass Area </td>
   <td style="text-align:left;"> g/m2 </td>
   <td style="text-align:left;"> Kunert et al 2019 [in prep] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Chlorophyll </td>
   <td style="text-align:left;"> m2/g </td>
   <td style="text-align:left;"> Kunert et al 2019 [in prep] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wood density </td>
   <td style="text-align:left;"> g/cm3 </td>
   <td style="text-align:left;"> Kunert et al 2019 [in prep] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TLP </td>
   <td style="text-align:left;"> MPa </td>
   <td style="text-align:left;"> Kunert et al 2019 [in prep] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P50 </td>
   <td style="text-align:left;"> MPa </td>
   <td style="text-align:left;"> Kunert et al 2019 [in prep] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P80 </td>
   <td style="text-align:left;"> MPa </td>
   <td style="text-align:left;"> Kunert et al 2019 [in prep] </td>
  </tr>
</tbody>
</table>


After the data was collected, linear mixed models were run with the resistance value of each tree for each pointer year (determined from the pointRes package) as the response variable. Traits and other geographic data were used as independent variables.

<!--chapter:end:03-methods.Rmd-->


# Results

## Biophysical Models
Crown position 
When including only biophysical traits, trees' resistance value (on a per-species basis) is explained best by crown position and height, with codominant trees being the most resistant to drought. This follows on work done by [@bennett_larger_2015] [and others?] which show that larger trees suffer more during drought, and confirms that this susceptibility can be seen in tree ring analyses. 







From [@kannenberg_linking_2019], species with diffuse porous wood anatomy (*Liriodendron*) are more sensitive to drought, whereas ring-porous are not as affected because they more easily rebuild structures for hydraulic conductivity. This paper mentions it would be good to have this data with respect to latent affects from drought.

<!--chapter:end:04-results.Rmd-->


# Discussion

*1. paragraph summarizing main results--> primary conclusions*

We partially supported the hypothesis that crown exposure makes trees more vulnerable to drought. Co-dominant trees had the highest drought resistance. Dominant trees had lower resistance, likely because they are the most exposed. Other studies have found clear evidence of greater drought sensitivity in trees with exposed crowns (e.g., [@suarez_factors_2004]; [@scharnweber_confessions_2019]). At the same time, intermediate and suppressed trees had even lower resistance. This indicates that other mechanisms such as competition or rooting depth were important. (Also note that our study design was not ideal for testing the role of canopy position. Current canopy position is a conservative separator of canopy position: trees may currently be in more dominant positions than they were at the time, but backwards movement is unlikely. This would bias against finding a signficant effect for H1.2. Height may be a more reliable predictor of past canopy position than is current canopy position, and explains a portion of variation in canopy position.)

Proximity to stream--either vertical (elev) or horizontal (distance)-- did not increase drought resistance; rather, it tended to decrease resistance (H1.3a). This may be because individuals growing further from water are acclimatized to drier conditions. However, the increase in drought resistance with distance from stream was less for small than large trees (H1.3b), indicating a potential importance of root depth/volume in conferring drought resistance.

<!--chapter:end:05-discussion.Rmd-->


# Conclusion

words words words

<!--chapter:end:06-conclusion.Rmd-->


# References

TRY database - ONLY INCLUDE IF USING SLA
Kattge, J., S. Diaz, S. Lavorel, I. C. Prentice, P. Leadley, G. Boenisch, E. Garnier, M. Westoby, P. B. Reich, I. J. Wright, J. H. C. Cornelissen, C. Violle, S. P. Harrison, P. M. v. Bodegom, M. Reichstein, B. J. Enquist, N. A. Soudzilovskaia, D. D. Ackerly, M. Anand, O. Atkin, M. Bahn, T. R. Baker, D. Baldocchi, R. Bekker, C. Blanco, B. Blonder, W. J. Bond, R. Bradstock, D. E. Bunker, F. Casanoves, J. Cavender-Bares, J. Q. Chambers, F. S. Chapin, J. Chave, D. Coomes, W. K. Cornwell, J. M. Craine, B. H. Dobrin, L. Duarte, W. Durka, J. Elser, G. Esser, M. Estiarte, W. F. Fagan, J. Fang, F. Fernandez-Mendez, A. Fidelis, B. Finegan, O. Flores, H. Ford, D. Frank, G. T. Freschet, N. M. Fyllas, R. V. Gallagher, W. A. Green, A. G. Gutierrez, T. Hickler, S. Higgins, J. G. Hodgson, A. Jalili, S. Jansen, C. Joly, A. J. Kerkhoff, D. Kirkup, K. Kitajima, M. Kleyer, S. Klotz, J. M. H. Knops, K. Kramer, I. Kuehn, H. Kurokawa, D. Laughlin, T. D. Lee, M. Leishman, F. Lens, T. Lenz, S. L. Lewis, J. Lloyd, J. Llusia, F. Louault, S. Ma, M. D. Mahecha, P. Manning, T. Massad, B. Medlyn, J. Messier, A. T. Moles, S. C. Mueller, K. Nadrowski, S. Naeem, U. Niinemets, S. Noellert, A. Nueske, R. Ogaya, J. Oleksyn, V. G. Onipchenko, Y. Onoda, J. Ordonez, G. Overbeck, W. A. Ozinga, S. Patino, S. Paula, J. G. Pausas, J. Penuelas, O. L. Phillips, V. Pillar, H. Poorter, L. Poorter, P. Poschlod, A. Prinzing, R. Proulx, A. Rammig, S. Reinsch, B. Reu, L. Sack, B. Salgado-Negret, J. Sardans, S. Shiodera, B. Shipley, A. Siefert, E. Sosinski, J.-F. Soussana, E. Swaine, N. Swenson, K. Thompson, P. Thornton, M. Waldram, E. Weiher, M. White, S. White, S. J. Wright, B. Yguel, S. Zaehle, A. E. Zanne, C. Wirth. 2011. TRY - a global database of plant traits. Global Change Biology, 17:2905-2935.

<!--chapter:end:07-references.Rmd-->

