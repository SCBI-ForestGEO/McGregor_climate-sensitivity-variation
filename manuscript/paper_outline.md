# Tree size, exposure, and hydraulic traits interactively shape drought response in a temperate broadleaf forest


## Abstract


## Introduction

Understanding how and why trees respond to drought is critical to predicting forest drought responses and climate change feedbacks.

*Questions & Hypotheses*


**1. Why do larger trees suffer greater growth declines during drought?**

1.0. Our forest displays the same trend as most forests globally (Bennett et al. 2015).

1.1. Height is a strong predictor of drought stress.

    * P1.1-Drought response increases with height at time of drought (derived from dbh). Height will be a significant predictor both alone or in combination with canopy position and elevation.

1.2. Large trees suffer more during drought because of greater exposure (to radiation, wind, etc.)--either in relation to neighboring trees or because of position on landscape.

    * P1.2a- Trees currently in a canopy position suffered more during drought. If canopy position is more important than height, we'd expect current canopy position to be a better predictor than current height.
    * P1.2b- Current canopy position will improve model over just the effect of height. Better comparison if we use current height.
    * P1.2c-Trees at higher elevations--particularly tall trees--suffer more because they are more exposed. Thus, elevation has additive or interactive effect with that of height (model with height + / * elevation better predictor than just height.)

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
1.0 | ln[dbh] | (none) | - | **31.1** | **47.81** | -0.5 | -1.54  
1.1 | ln[height] | (none) | - | **33.86** | **48.75** | -0.42 | -0.93 
1.2a | canopy.position  | (none) | canopy<subcanopy | -2.02 | NA | -1.5 | **3.64**  
1.2b | canopy.position + ln[height]  | ln[height] | canopy<subcanopy | **13.86** | -1.99 | 1.21 | **12.13** 
1.2c1, 1.3a1 | elev + ln[height] | ln[height]  | - | 0.52 | NA | **5.09** | 0.74 
1.2c2 | elev x ln[height] | ln[height] | - | 1.21  | **3.12** | **4.32** | 0.95 
1.3b1 | elev x ln[height] | ln[height] | + | NA | NA | NA | NA 
1.3a2 | stream.distance + ln[height] | ln[height]  | - | -0.48  | NA | **2.76** | 2.00  
1.3b2 | stream.distance x ln[height] | ln[height] | + | NA  | NA | NA | NA 
2.1 |  TLP + ln[height] | ln[height] | -  | **5.08** | **2.37** | 1.32 | -0.78 
2.2 |  rp + ln[height] | ln[height] | ring>diffuse  | -2.42 | 0.57 | NA | **5.83** 

*all models include (1|sp/tree) (random effect) and year (fixed) when run with all years. When run for individual years, all models include (1|sp) as the random effect.

** refers to model variable added to null. 

*** dAIC is calculated as AIC_model with variable(s) - AIC_null model. When response is opposite prediction, dAIC is listed as NA (there no instances of this where dAIC>2 --*confirm*).

This is a table showing which dAIC values are >2, but are listed as NA due to the interaction.

| Prediction | Model | Original value dAIC | Coefficient
|------------|--------|--------|-----------------
|1.2a | dAIC 1964-1966| 9.54| 0.0757
|1.3b1|dAIC 1964-1966 |3.12| 0.0044
|1.3b1|dAIC 1977 | 4.32| 0.0064
|1.3a2|dAIC 1964-1966 | 3.74| -0.027
|1.3b2|dAIC1964-1966 | 2.89 | 0.025
|1.3b2|dAIC 1977 | 3.93| 0.127


## Discussion


## Collaborators

|**Name**|**Contribution**|
|--------|----------------|
|Ian McGregor|running study|
|Krista Anderson-Teixeira|senior author|
|Ryan Helcoski|processed cores|
|Valentine Herrmann|R analyses|
|Alan Tepley|provided guidance|
|Neil Pederson|provided guidance|
|Amanda Jean Seglem|got canopy position data|


## Sources
Any potential sources to look into can be listed here, but ultimately the master sources list will be on Zotero.

on canopy position:
[Scharnweber et al. 2019](https://www.sciencedirect.com/science/article/pii/S1125786518302017)

[Suarez et al. 2004](https://besjournals.onlinelibrary.wiley.com/doi/pdf/10.1111/j.1365-2745.2004.00941.x)

on tree-rings:
[Scharnweber et al 2019](https://www.sciencedirect.com/science/article/pii/S1125786518302017)
