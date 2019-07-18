# Remaining steps for paper

## Data
- [X] topographic wetness index (#38)
- [X] search more for any precedents, but don't take a lot of time on this. If anything, we can assign our own threshold (median looking over the timeframe, or looking at the average of each month)
- [X] get threshold / figure out how to split by sun/cloud
  - the whole point is to think biologically about this and what makes sense, in other words, what can be sufficiently and logically argued 
  - \[Edit 17 June] we have decided to forego this step for the time being 
- [X] officially remove pist from analysis (because it already is de facto)
- [X] check if frni is de facto removed from model runs. If not, see what the effect is if we remove it altogether (from both biophysical and traits runs)
- [X] for caco and cato (<3 canopy cores), combine the cores to get the overall resistance values for them, then split into canopy/subcanopy (using the tag number)

## Figures
### Graphs
- [ ] make more consolidated graphs (#37)
- [ ] each trait plotted against height (logged values of height), possibly by weighted mean/% (Meakem made code for this when she was here, ask valentine if know anything)
- [X] basic map of plot
- [X] vertical profile graphs (would go in results)
- [X] ring-width index like in Ryan's paper, but exclude pist, and add line for PDSI at top (ask valentine for help)
- also add line to indicate 5 years pre-drought and a line at the drought
  - the droughts we've chosen based on PDSI values are 1964-1966, 1977, and 1999. So the 5 years pre-drought would be 1959-1961$, 1972, and 1994.

### Table
- [ ] hypothesis testing table (like the one we have already)
  - null model will be the best model, so like, if testing effect of height, you'd do null minus height
- [X] species table showing species, number of cores, and number of trees in each crown position (6 total columns)
- [X] trait table, showing trait, unit, mean, and range
- [X] table of coefficients of best models (biophysical and traits)

## Models
- [ ] make table going through each trait individually (#36, and by extension #33, #16)
- [ ] run through the different hypotheses and fill out the hypothesis table
- [X] Use both height and position in traits model (reasoning is that "we did the biophysical model first to determine which was the most important trait, before adding it in with hydraulic traits to see how that affected drought resistance")


## Paper
New Phytologist formatting options [here](https://nph.onlinelibrary.wiley.com/hub/journal/14698137/about/author-guidelines)
- [ ] update bookdown code based on formatting
- [X] get formatting options that they prefer


### Writing
- [ ] give good description of droughts in publication (#39)
- [ ] write abstract / have it mostly written
- [ ] change main questions? structure paper according to them
- [ ] would need to get specific methods from Nobby, since he's not publishing this data from SCBI right now. This will be the first time this data is published. (If a ton of methods, then some will be moved to supplementary data)
- [ ] Keep methods updated (by subsections [hypothesis, etc])
- [ ] do some discussion input
- [X] get bookdown working

Test traits individually against a null model, and if those are siginificant on their own, then they are candidates for the full model

1. Start out looking at size, and we determine it to be significant. Height is better than DBH, so look at height going forward (is statistically better, calculated from DBH).
2. Biophysical is the environment, add in other effects.
3. Separately, we look at leaf traits.
4. Look at null model using height, test all effects together, anything with dAIC <2 is dropped as a contender for full model
5. Then run full model and see what comes out on top

6. make comment in discussion about the effect of drought in individual years (have table showing the different years)
