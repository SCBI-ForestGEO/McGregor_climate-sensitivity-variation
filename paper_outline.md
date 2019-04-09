# Outline for paper

## Abstract


## Hypotheses
1. Height is a strong predictor of drought stress.

    * P1-Drought response increases with height at time of drought (derived from dbh). Height will be a significant predictor both alone or in combination with canopy position and elevation.

2. Large trees suffer more during drought because of greater exposure (to radiation, wind, etc.)--either in relation to neighboring trees or because of position on landscape.

    * P2a- Trees currently in a canopy position suffered more during drought. If canopy position is more important than height, we'd expect current canopy position to be a better predictor than current height.
    * P2b- Current canopy position will improve model over just the effect of height. Better comparison if we use current height.
    * P2c-Trees at higher elevations--particularly tall trees--suffer more because they are more exposed. Thus, elevation has additive or interactive effect with that of height (model with height + / * elevation better predictor than just height.)

3. Rooting volume/depth relative to water sources are critical in drought response. Effects of drought on larger trees are mediated by the fact that large trees have better access to water.
  
     * P3a- drought response increases with elevation and/or distance from stream
     * P3b- There is a dbh\*elev interaction, elevation (relative to stream?) matters less for big trees with larger (and potentially deeper) root system.
      
4. Larger trees suffer more because larger trees tend to be species with more drought-sensitive traits.

    * P4a- TLP predicts drought response
    * P4b- TLP is lower (larger negative) in smaller/ understory trees
    * P4c- Inclusion of TLP in model eliminates (or significantly reduces) effect of height (and elev).

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
- general model description

To test H1,... (per hypotheses)

Prediction | Model Variable(s)* | Null model Variables* | dAIC - all years | dAIC - 1964... 
--- | --- | --- | --- | --- 
DBH | ln[dbh] | (none) | (AIC_model with variable(s) - AIC_null model) | --- 
P1 | ln[height] | (none) | (AIC_model with variable(s) - AIC_null model) | --- 
P3a | elev + ln[dbh] | ln[dbh]  | (AIC_model with variable(s) - AIC_null model) | --- 
P3a | stream.distance + ln[dbh] | ln[dbh]  | (-1.8 - tentative) | --- 
P3b | elev x ln[dbh] | ln[dbh]  | (AIC_model with variable(s) - AIC_null model) | --- 
P3a | stream.distance + ln[dbh] | ln[dbh]  | (AIC_model with variable(s) - AIC_null model) | --- 

*all models include tree/species (random effect) and year (fixed) when run together

## Results


## Discussion


## Collaborators

|**Name**|**Contribution**|
|--------|----------------|
|Ian McGregor|running study|
|Krista AT|senior author|
|Ryan Helcoski|processed cores|
|Valentine Herrmann|R analyses|
|Alan Tepley|provided guidance|
|Neil Pederson|provided guidance|


## Sources
Any potential sources to look into can be listed here, but ultimately the master sources list will be on Zotero.
