# Tree size, exposure, and hydraulic traits interactively shape drought response in a temperate broadleaf forest

Ian McGregor

Ryan Helcoski

Norbert Kunert

Alan Tepley

Valentine Herrmann

Neil Pederson

Lawren Sack

Krista Anderson-Teixeira

## Abstract


## Introduction

Understanding how and why trees respond to drought is critical to predicting forest drought responses and climate change feedbacks.

Bennett et al. 2015 showed that in forests globally, large trees suffer greater growth reductions during drought. However, this analysis quantified tree size based on DBH, which has no direct mechanistic meaning. This study proposed two major mechanisms (besides insects): (1) inherently greater biophysical challenge of being tall; (2) greater exposure of the crowns of large trees. It is also expected that roots play a role. And its possible that pattern could be caused by smaller trees being more drought resistant. These hypotheses need to be tested.

Bennett et al. 2015 identified only one study on tree growth responses to drought in the Eastern US temperate deciduous biome. We know little about how tree size shapes drought response in this biome.  

*Questions & Hypotheses*


**1. Why do larger trees suffer greater growth declines during drought?**

1.0. Our forest displays the same trend as most forests globally (Bennett et al. 2015).

1.1. Height is a strong predictor of drought stress.

    * P1.1-Drought response increases with height at time of drought (derived from dbh). Height will be a significant predictor both alone or in combination with canopy position and elevation.

1.2. Large trees suffer more during drought because of greater exposure (to radiation, wind, etc.)--either in relation to neighboring trees or because of position on landscape.

    * P1.2a- Trees currently in a canopy position suffered more during drought. If canopy position is more important than height, we'd expect current canopy position to be a better predictor than current height.
    * P1.2b- Current canopy position will improve model over just the effect of height. Better comparison if we use current height.

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




## Methods 

*Site description*
- general
- climate

*[Data collection]*
- tree census
- cores (summary of core collection, processing, chronology building)
- canopy position
- TLP

*Analyses*
- restistance calculation
- selecting drought years
- height calculation
- distance to water
- general model description... We compared null vs predicted models for each prediction (Table 1).



## Results

**Table 1. Mixed effects model setups and results for testing hypotheses 1 & 2. Note that H3 is tested using a different framework.**

Prediction | Model variable(s)*  | Null model variables* | Predicted direction of response** | dAIC*** - all years | dAIC 1964-66 | dAIC 1977 | dAIC 1999
--- | --- | --- | --- | --- | --- | --- | ---  
1.0 | ln[dbh] | (none) | - | **31.10** | **47.81** | -0.5 | -1.54  
1.1 | ln[height] | (none) | - | **33.86** | **48.75** | -0.42 | -0.93 
1.2a | canopy.position (all)| (none) | dominant < co-dominant < intermediate < supressed | **69.07** | **17.51** | **15.41** | **36.57**
1.2b | canopy.position (all) + ln[height] | ln[height] | dominant < co-dominant < intermediate < supressed | **74.28** | **4.71** | **16.33** | **38.68**
1.3a1 | elev + ln[height] | ln[height]  | - | 0.52 | 1.75 | **5.09** | 0.74 
1.3a2 | stream.distance + ln[height] | ln[height]  | - | -0.48  | **3.74** | **2.76** | 2.00  
1.3b1 | elev x ln[height] | ln[height] | + | 1.21 | **3.12** | **4.32** | 0.95 
1.3b2 | stream.distance x ln[height] | ln[height] | + | -0.43  | **2.89** | **3.93** | 0.80 
2.1 |  TLP + ln[height] | ln[height] | -  | **5.08** | **2.37** | 1.32 | -0.78 
2.2 |  rp + ln[height] | ln[height] | ring>diffuse  | -2.42 | 0.57 | 0.58 | **5.83** 

*all models include (1|sp/tree) (random effect) and year (fixed) when run with all years. When run for individual years, all models include (1|sp) as the random effect.

** refers to model variable added to null. 

*** dAIC is calculated as AIC_model with variable(s) - AIC_null model. When response is opposite prediction, dAIC is listed as NA (there no instances of this where dAIC>2 --*confirm*).


- - - 

- Our site matches trend observed globally by Bennett et al.: Larger trees had lower restistance to drought (1.0)

- H1.1 is supported. Height was the most important variable explaining resistance (1.1) and height_ln explained more variation than DBH_ln.  

- H1.2 is partially supported. Dominant trees had lower resistance than co-dominant trees, but intermediate and suppressed trees had even lower resistance (co-dominant>dominant>intermediate>suppressed; H1.2a). 

- H1.3 is  partially rejected (1.3a)/ partially accepted (1.3b): drought resistance increases with elevation or distance from water (H1.3a). However, there is a consistently + interaction between height_ln and elev or distance (1.3b), indicating that short trees have less resistance (relative to tall trees) as you move up in elevation/away from streams.

- H2.1 is supported; trees with less negative TLP has lower resistance

- H2.2 is supported; ring porous trees have greater resistance


## Discussion

*1. paragraph summarizing main results--> primary conclusions*


We partially supported the hypothesis that crown exposure makes trees more vulnerable to drought. Co-dominant trees had the highest drought resistance. Dominant trees had lower resistance, likely because they are the most exposed. Other studies have found clear evidence of greater drought sensitivity in trees with exposed crowns (e.g., [Suarez et al. 2004](https://besjournals.onlinelibrary.wiley.com/doi/pdf/10.1111/j.1365-2745.2004.00941.x)
;[Scharnweber et al. 2019](https://www.sciencedirect.com/science/article/pii/S1125786518302017)). At the same time, intermediate and suppressed trees had even lower resistance. This indicates that other mechanisms such as competition or rooting depth were important. (Also note that our study design was not ideal for testing the role of canopy position. Current canopy position is a conservative separator of canopy position: trees may currently be in more dominant positions than they were at the time, but backwards movement is unlikely. This would bias against finding a signficant effect for H1.2. Height may be a more reliable predictor of past canopy position than is current canopy position, and explains a portion of variation in canopy position.)

Proximity to stream--either vertical (elev) or horizontal (distance)-- did not increase drought resistance; rather, it tended to decrease resistance (H1.3a). This may be because individuals growing further from water are acclimatized to drier conditions. However, the increase in drought resistance with distance from stream was less for small than large trees (H1.3b), indicating a potential importance of root depth/volume in conferring drought resistance.


## Acknowledgements

|**Name**|**Contribution**|
|--------|----------------|
|Amanda Jean Seglem|got canopy position data|
|see list [here](https://github.com/EcoClimLab/HydraulicTraits/tree/master/data/SCBI)| help with traits|
| see list [here](https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/tree/master/tree_cores) | help with cores|


## Sources
Any potential sources to look into can be listed here, but ultimately the master sources list will be on Zotero.

on canopy position:
[Scharnweber et al. 2019](https://www.sciencedirect.com/science/article/pii/S1125786518302017)

[Suarez et al. 2004](https://besjournals.onlinelibrary.wiley.com/doi/pdf/10.1111/j.1365-2745.2004.00941.x)

on tree-rings:
[Scharnweber et al 2019](https://www.sciencedirect.com/science/article/pii/S1125786518302017)
