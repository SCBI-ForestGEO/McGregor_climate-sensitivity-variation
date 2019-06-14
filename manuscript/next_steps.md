# Remaining steps for paper

## Data
1. We officially have all the data that we're going to be using. The last step involves figuring out how to parse the threshold between sunny and cloudy days based on daily solar radiation.

- [ ] search more for any precedents, but don't take a lot of time on this. If anything, we can assign our own threshold (median looking over the timeframe, or looking at the average of each month)
- the whole point is to think biologically about this and what makes sense, in other words, what can be sufficiently and logically argued 

- [ ] when above is done, get daily mean of other variables, split by sun/cloud
- Krista mentioned that we should be doing monthly mean, and this would become a figure where like you have a different line for each month with height on the y-axis

## Figures
### Graphs
- [ ] basic map of plot
- [ ] vertical profile graphs (would go in results)
- [ ] ring-width index like in Ryan's paper, but exclude frni and pist, and add line for PDSI at top (ask valentine for help)
- also add line to indicate 5 years pre-drought and a line at the drought
- [ ] each trait plotted against height (logged values of height), possibly by weighted mean/% (Meakem made code for this when she was here, ask valentine if know anything)

### Table
- [ ] species table showing species, number of cores, and number of trees in each crown position (6 total columns)
- [ ] trait table, showing trait, unit, mean, and range
- [ ] hypothesis testing table (like the one we have already)
- null model will be the best model, so like, if testing effect of height, you'd do null minus height
- [ ] table of coefficients of best models (biophysical and traits)

## Models
- [ ] officially remove pist from analysis (because it already is de facto)
- [ ] check if frni is de facto removed from model runs. If not, see what the effect is if we remove it altogether (from both biophysical and traits runs)
- [ ] for caco and cato (<3 canopy cores), combine the cores to get the overall resistance values for them, then split into canopy/subcanopy (using the tag number)
- [ ] Use both height and position in traits model (reasoning is that "we did the biophysical model first to determine which was the most important trait, before adding it in with hydraulic traits to see how that affected drought resistance")

## Paper
New Phytologist
- [ ] get formatting options that they prefer and update bookdown code for that

### Writing
- [ ] get bookdown working so that it makes correct html and .md (but .md most important)
- [ ] write abstract / have it mostly written
- [ ] change main questions? structure paper according to them
- [ ] would need to get specific methods from Nobby, since he's not publishing this data from SCBI right now. This will be the first time this data is published. (If a ton of methods, then some will be moved to supplementary data)
- [ ] Keep methods updated (by subsections [hypothesis, etc])
- [ ] do some discussion input
