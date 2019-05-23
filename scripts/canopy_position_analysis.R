######################################################
# Purpose: Analysis of tree cores with relation to tree characteristics, in order to determine causes of drought susceptibility (using ForestGEO cores)
# Developed by: Ian McGregor - mcgregori@si.edu
# R version 3.5.2 - First created February 2019
######################################################

#1. full script set-up ####
library(RCurl)
cru1901 <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/climate_sensitivity_cores/master/results/canopy_vs_subcanopy/1901_2009/tables/monthly_correlation/correlation_with_CRU_SCBI_1901_2016_climate_data.csv"), stringsAsFactors = FALSE)

library(ggplot2)
library(ggpubr)

#subset out caco, cato, and frni because they don't have pair of canopy and subcanopy
cru1901 <- cru1901[!(cru1901$Species %in% c("CACO_subcanopy", "CATO_subcanopy", "FRNI_subcanopy")), ]

cru1901_loop <- cru1901

#create separate identifier
cru1901_loop$position <- ifelse(grepl("subcanopy", cru1901$Species), "subcanopy", "canopy")
cru1901_loop$Species <- gsub("_[[:alpha:]]+$", "", cru1901$Species)

##########################################################################################
#2. box plots ####
cru1901_loop$variable <- as.character(cru1901_loop$variable)
clim <- unique(cru1901_loop$variable)
species <- unique(cru1901_loop$Species)
months <- c("curr.may", "curr.jun", "curr.jul", "curr.aug")

#creates a lattice graph showing box plot of variables grouped by species
ggplot(data = cru1901) +
  aes(x = Species, y = coef, fill = variable) +
  geom_boxplot() +
  labs(title = "Correlation by species and variable",
       y = "Correlation") +
  facet_wrap( ~ Species, scales="free", nrow=4) +
  theme_minimal()

#creates lattice graph comparing canopy and subcanopy across species

pdf("graphs_plots/canopy_subcanopy_correlation.pdf", width=10)
cru1901_loop$Species <- as.factor(cru1901_loop$Species)

#this piece of code puts the graphs in date order
cru1901_loop <- within(cru1901_loop, month <- factor(month, levels=cru1901_loop$month[1:17]))
with(cru1901_loop, levels(month))

for (j in seq(along=clim)){
    cru1901_sub <- cru1901_loop[cru1901_loop$variable %in% clim[[j]], ]
    cru1901_sub <- group_by(cru1901_sub, month)
    
    q <- ggplot(data = cru1901_sub) +
      geom_boxplot(aes(x = position, y = coef, fill = position)) +
      labs(title = paste0("Canopy vs subcanopy: ", clim[[j]]),
           y = "Correlation") +
      stat_compare_means(aes(x=position, y=coef), method="t.test", label.x.npc = 0, label.y.npc = 0.97) +
      facet_wrap(~ month, scales="free", nrow=4) +
      theme_minimal()
    print(q)
}

dev.off()


############################################################################################
#3. intro to the mixed effects model ####
library(car)
library(MASS)
library(lme4)
library(AICcmodavg)
library(data.table)
library(dplyr)

cru1901_loop$corr <- cru1901_loop$coef + 1 #since we have negative values

##3a. descriptions of running the mixed effects model ####

#response variable is coef (continuous) (renamed corr)
#do mixed model, look at the residuals, then check the distribution of the residuals
#help taken from https://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html

# corr is the response variable, position and month are fixed effects, and Species is random
#REML is FALSE bc we only have one random effect
lmm <- lmer(corr ~ position + month + (1 | Species), data=cru1901_loop, REML=FALSE)
summary(lmm)

#this anova gives p-values. In other words, it says is the effect of position or month on corr significant? Or, is the effect due to random chance (high p-value) or not (low p-value)?
Anova(lmm)

v="PET" #(assuming cru1901_loop is subset to only this variable)
assign(paste0(v, "_drop1_table"), drop1(lmm)) #creates output

#check distribution of residuals
qqp(residuals(lmm), "norm", main="Normal residuals")

#we want the AIC value to be the lowest it can be. This function says, if one of the fixed effects has a higher value than the original, then the output of the model wouldn't matter if we took out that variable.
drop1(lmm)

##3b. finding the right model to use ####
#make subsets and vector for loops
cru1901_lmm <- droplevels(cru1901_loop[grepl("(r.may|r.jun|r.jul|r.aug)", cru1901_loop$month) & !cru1901_loop$variable %in% "frs", ]) #only current growing season
clim <- unique(cru1901_lmm$variable)

#determine what model is best
#this function looks through all the models in the list (in the loop below) to say what is the best one (what fits the best)
aictab(cand.models, second.ord=TRUE, sort=TRUE)

all_models_aic <- NULL
all_models_bic <- NULL
for (v in seq(along=clim)){
  cru_sub <- cru1901_lmm[cru1901_lmm$variable %in% clim[[v]], ]
  
  lmm.null <- lmer(corr ~ 1 + (1 | Species), data=cru_sub, REML=FALSE)
  lmm.position <- lmer(corr ~ position + (1 | Species), data=cru_sub, REML=FALSE)
  lmm.month <- lmer(corr ~ month + (1 | Species), data=cru_sub, REML=FALSE)
  lmm.full <- lmer(corr ~ position + month + (1 | Species), data=cru_sub, REML=FALSE)
  
  cand.models <- list(lmm.null, lmm.position, lmm.month, lmm.full)
  names(cand.models) <- c("null_1", "position_2", "month_3", "full_4")
  
  #this function looks through all the models above to say what is the best one (what fits the best)
  var_aic <- aictab(cand.models, second.ord=TRUE, sort=TRUE)
  var_aic$var <- clim[[v]]
  all_models_aic <- rbind(all_models_aic, var_aic)
  
  var_bic <- bictab(cand.models, sort=TRUE)
  var_bic$var <- clim[[v]]
  all_models_bic <- rbind(all_models_bic, var_bic)
}

#subset by only the top result (the minimum AICc value) for each variable
aic_top <- all_models_aic %>%
  group_by(var) %>%
  filter(AICc == min(AICc))

bic_top <- all_models_bic %>%
  group_by(var) %>%
  filter(BIC == min(BIC))

##3c. run the best model ####
#the overall top result appears to be the full model. Thus, we use that model below.
clim_anova <- NULL
clim_BIC <- NULL
for (v in seq(along=clim)){
  cru_sub <- cru1901_lmm[cru1901_lmm$variable %in% clim[[v]], ]
  
  lmm.full <- lmer(corr ~ position + month + (1 | Species), data=cru_sub, REML=TRUE)
  
  vari_anova <- Anova(lmm.full)
  vari_anova$var <- clim[[v]]
  clim_anova <- rbind(clim_anova, vari_anova)
  
  #although we compared AIC outside of the loop when determining what model to use here, it's good to also check out BIC for a thorough analysis.
  vari_drop1 <- drop1(lmm.full, k = log(nrow(cru_sub))) #because we add k = log(nrow(cru_sub)),                                                             this now shows BIC 
  vari_drop1$var <- clim[[v]]
  clim_BIC <- rbind(clim_BIC, vari_drop1)
  
  q <- qqp(residuals(lmm.full), "norm", main=paste0(clim[[v]], "_residuals"))
  print(q)
}

clim_BIC <- setDT(clim_BIC, keep.rownames="aspect")[]
clim_BIC <- setnames(clim_BIC, old="AIC", new="BIC")
clim_BIC$aspect <- gsub("<none>", "full model", clim_BIC$aspect)
clim_BIC$var <- as.character(clim_BIC$var)
colnames(clim_BIC[,3]) <- "BIC"

clim_anova$signif <- ifelse(clim_anova$`Pr(>Chisq)` <0.05, 1, 0)

#filter by min value across variables. This shows BIC for this particular model (lmm.full). This makes sense considering lmm.full wasn't the majority preferred method for BIC (see bic_top).
clim_BIC_top <- clim_BIC %>%
  group_by(var) %>%
  filter(BIC == min(BIC))
#############################################################################################
#4. finding pointer years and resistance metrics ####
rm(list=ls())
library(pointRes)
library(dplR)
library(data.table)
library(tools)
library(dplyr)
library(reshape2)

#NB ####
##to be clear, I wrote this code before I realized that some of the work done in these loops had already been done in the outputs of res.comp (specifically out.select). However, since the code runs well, and I double-checked that it was giving the same outputs as analyzing out.select, I'm keeping it as is.

##4a. canopy ####
dirs_can <- dir("data/core_files/canopy_cores", pattern = "_canopy.rwl")

dirs_can <- dirs_can[!dirs_can %in% c("frni_canopy.rwl", "frni_drop_canopy.rwl", "caco_drop_canopy.rwl")]

sp_can <- gsub("_drop_canopy.rwl", "", dirs_can)

canopy <- list()
widths_can <- list()
canopy_table <- NULL
for (i in seq(along=dirs_can)){
  for (j in seq(along=sp_can)){
    if (i==j){
      file <- dirs_can[[i]]
      rings <- read.rwl(paste0("data/core_files/canopy_cores/", file)) #read in rwl file
      area <- bai.in(rings) #convert to bai.in
      testr <- res.comp(area, nb.yrs=5, res.thresh.neg = 30, series.thresh = 50) #get resilience metrics
      canopy[[i]] <- testr
      widths_can[[i]] <- rings
      
      testr_table <- data.frame(testr$out)
      testr_table <- testr_table[testr_table$nb.series > 4, ] #remove where there are < 4 series
      testr_table$sp <- sp_can[[j]]
      testr_table$position <- "canopy"
      
      canopy_table <- rbind(canopy_table, testr_table)
    }
  }
}
values <- paste0(sp_can, "_can_res")
names(canopy) <- values
values <- paste0(sp_can, "_canopy")
names(widths_can) <- values


##4b. subcanopy ####
dirs_subcan <- dir("data/core_files/subcanopy_cores", pattern = "_subcanopy.rwl")

#dirs_subcan <- dirs_subcan[dirs_subcan != "frni_drop_subcanopy.rwl" & dirs_subcan != "caco_drop_subcanopy.rwl"]

sp_subcan <- gsub("_drop_subcanopy.rwl", "", dirs_subcan)

subcanopy <- list()
widths_sub <- list()
subcanopy_table <- NULL
for (i in seq(along=dirs_subcan)){
  for (j in seq(along=sp_subcan)){
    if (i==j){
      file <- dirs_subcan[[i]]
      rings <- read.rwl(paste0("data/core_files/subcanopy_cores/", file)) #read in rwl file
      area <- bai.in(rings) #convert to bai.in
      test <- res.comp(area, nb.yrs=5, res.thresh.neg = 30, series.thresh = 50) #get resilience metrics
      subcanopy[[i]] <- test
      widths_sub[[i]] <- rings

      test_table <- data.frame(test$out)
      test_table <- test_table[test_table$nb.series > 4, ] #remove where there are < 4 series
      test_table$sp <- sp_subcan[[j]]
      test_table$position <- "subcanopy"
      
      subcanopy_table <- rbind(subcanopy_table, test_table)
    }
  }
}
values_sub <- paste0(sp_subcan, "_subcan_res")
names(subcanopy) <- values_sub
values <- paste0(sp_subcan, "_subcanopy")
names(widths_sub) <- values

widths <- c(widths_can, widths_sub) #combine into one, then delete individual. For use in #5d
widths_can <- NULL
widths_subcan <- NULL

##4c. df for pointer years of all trees combined ####
full_ind <- rbind(canopy_table, subcanopy_table) #full table of indices for canopy and subcanopy cores
pointers <- full_ind[full_ind$nature == -1, ]

library(dplyr)
years_point <- count(pointers, vars=year) #counts the occurrences of each unique year
colnames(years_point) <- c("yr", "n.pointer")
years_point <- years_point[order(years_point$n.pointer, decreasing=TRUE), ]

#top drought years by species and canopy position
years_bysp <- pointers[pointers$year %in% c(1964, 1965, 1966, 1977, 1999), ]
years_bysp <- years_bysp[, c(1,13,14,2:12)]
years_bysp <- years_bysp[order(years_bysp$year, years_bysp$sp), ]

#write.csv(pointers, "data/occurrence_of_pointer_yrs.csv", row.names=FALSE)

##4d. resistance metrics for all trees ####
neil_list <- read.csv("data/core_list_for_neil.csv", stringsAsFactors = FALSE)

neil_list$tag <- paste0("X", neil_list$tag) #to match the colnames of can_resist below

# pointer_years <- head(years_point$yr) #from above in #4c
# pointer_years <- pointer_years[!pointer_years %in% c(1911, 1947, 1991)]
pointer_years <- c(1964, 1965, 1966, 1977, 1999)

# IMPORTANT: we are defining "1966" as the average of the resistance values over 1964, 1965, and 1966.
pointer_years_simple <- c(1966, 1977, 1999)

###canopy ####
#this loop says, for the different species in the list "canopy" (names(canopy)), create a dataframe of only the resistance index. Make a list of the colnames, which are the individual trees. Then, assign species identifiers for each one from Neil's core list, subset by the defined pointer years, and melt the data before rbinding.

tag_n <- names(canopy)
trees_canopy <- NULL
for (i in seq(along=1:length(tag_n))){
  can_resist <- data.frame(canopy[[i]]$resist)
  years <- rownames(can_resist)
  colnames(can_resist) <- gsub("A", "", colnames(can_resist))
  tree_series <- colnames(can_resist)
  
  ind <- can_resist
  ind_neil <- neil_list[neil_list$tag %in% tree_series, ]
  
  ind$year <- years
  ind$sp <- unique(ind_neil$sp)
  ind$position <- "canopy"
  
  ind <- ind[ind$year %in% pointer_years, ]
  
  ## these three lines of code are for taking the mean of 1964-1966, 
  ## since it was a multi-year drought. We're calling it 1966 for simplicity.
  ind["1966", 1:(ncol(ind) -3)] <- colMeans(ind[c(1:3), 1:(ncol(ind) -3)])
  ind <- ind[-c(1,2), ]
  ind[, 1:(ncol(ind) -3)] <- round(ind[, 1:(ncol(ind) -3)], 2)
  
  change <- melt(ind)
  setnames(change, old=c("variable", "value"), new=c("tree", "resist.value"))
  change$tree <- gsub("X", "", change$tree)
  change$tree <- gsub("^0", "", change$tree)
  
  trees_canopy <- rbind(trees_canopy, change)
}


###subcanopy ####
#this loop says, for the different species in the list "subcanopy" (names(subcanopy)), create a dataframe of only the resistance index. Make a list of the colnames, which are the individual trees. Then, assign species identifiers for each one from Neil's core list, subset by the defined pointer years, and melt the data before rbinding.

tag_n <- names(subcanopy)
trees_subcanopy <- NULL
for (i in seq(along=1:length(tag_n))){
  sub_resist <- data.frame(subcanopy[[i]]$resist)
  years <- rownames(sub_resist)
  colnames(sub_resist) <- gsub("A", "", colnames(sub_resist))
  tree_series <- colnames(sub_resist)
 
    ind <- sub_resist
    ind_neil <- neil_list[neil_list$tag %in% tree_series, ]
    
    ind$year <- years
    ind$sp <- unique(ind_neil$sp)
    ind$position <- "subcanopy"
    
    ind <- ind[ind$year %in% pointer_years, ]
    
    ## these three lines of code are for taking the mean of 1964-1966, 
    ## since it was a multi-year drought. We're calling it 1966 for simplicity.
    ind["1966", 1:(ncol(ind) -3)] <- colMeans(ind[c(1:3), 1:(ncol(ind) -3)])
    ind <- ind[-c(1,2), ]
    ind[, 1:(ncol(ind) -3)] <- round(ind[, 1:(ncol(ind) -3)], 2)
    
    change <- melt(ind)
    setnames(change, old=c("variable", "value"), new=c("tree", "resist.value"))
    change$tree <- gsub("X", "", change$tree)
    change$tree <- gsub("^0", "", change$tree)
    
    trees_subcanopy <- rbind(trees_subcanopy, change)
}

###rbind together ####
trees_all <- rbind(trees_canopy, trees_subcanopy)
trees_all$year <- as.numeric(trees_all$year)

#subset out NAs for resistance values (not necessary, bc lmm will automatically exclude them)
trees_all <- trees_all[!is.na(trees_all$resist.value), ]
trees_all$year <- as.character(trees_all$year)

##4e. determine proportion of resistance values per sp ####
prop <- data.frame("sp" = unique(trees_all$sp))
prop$value.over1 <- NA
prop$can.value.over1 <- NA
prop$sub.value.over1 <- NA

trees_all.sp <- unique(trees_all$sp)

for (i in seq(along=prop$sp)){
  for (j in seq(along=unique(trees_all.sp))){
    if (i==j){
      temp <- trees_all[trees_all$sp == trees_all.sp[[j]] & trees_all$resist.value>=1, ]
      temp <- temp[!is.na(temp$resist.value), ]
      prop$value.over1[[i]] <- nrow(temp)
      prop$can.value.over1[[i]] <- nrow(temp[temp$position == "canopy", ])
      prop$sub.value.over1[[i]] <- nrow(temp[temp$position == "subcanopy", ])
    }
  }
}



##4f. comparing residuals of PDSI values and BAI of all trees ####
dirs_can <- dir("data/core_files/canopy_cores", pattern = "_canopy.rwl")

dirs_can <- dirs_can[!dirs_can %in% c("frni_canopy.rwl", "frni_drop_canopy.rwl", "caco_drop_canopy.rwl")]

sp_can <- gsub("_drop_canopy.rwl", "", dirs_can)

bai_table_can <- NULL
for (i in seq(along=dirs_can)){
  for (j in seq(along=sp_can)){
    if (i==j){
      file <- paste0("data/core_files/canopy_cores/", dirs_can[[i]])
      rings <- read.rwl(file) #read in rwl file
      area <- bai.in(rings) #convert to bai.in
      
      ##transpose the dataframe
      transorg <- transpose(area)
      rownames(transorg) <- colnames(area)
      colnames(transorg) <- rownames(area)
      transorg <- setDT(transorg, keep.rownames = TRUE)[]
      setnames(transorg,1,"tag")
      
      #create column with only numeric tag numbers, then order
      transorg$tag <- gsub("A", "", transorg$tag) 
      transorg$tag <- as.numeric(transorg$tag)
      transorg <- transorg %>% select(tag,everything())
      formatC(transorg$tag, width=6,format="d", flag="0") 
      #a shorter version of this is sprintf("%06d", transorg$tag)
      
      
      bai_table_can <- rbind(bai_table_can, transorg, fill=TRUE)
    }
  }
}

dirs_subcan <- dir("data/core_files/subcanopy_cores", pattern = "_subcanopy.rwl")

#dirs_subcan <- dirs_subcan[dirs_subcan != "frni_drop_subcanopy.rwl" & dirs_subcan != "caco_drop_subcanopy.rwl"]

sp_subcan <- gsub("_drop_subcanopy.rwl", "", dirs_subcan)

bai_table_sub <- NULL
for (i in seq(along=dirs_subcan)){
  for (j in seq(along=sp_subcan)){
    if (i==j){
      file <- file <- paste0("data/core_files/subcanopy_cores/", dirs_subcan[[i]])
      rings <- read.rwl(file) #read in rwl file
      area <- bai.in(rings) #convert to bai.in
      
      ##transpose the dataframe
      transorg <- transpose(area)
      rownames(transorg) <- colnames(area)
      colnames(transorg) <- rownames(area)
      transorg <- setDT(transorg, keep.rownames = TRUE)[]
      setnames(transorg,1,"tag")
      
      #create column with only numeric tag numbers, then order
      transorg$tag <- gsub("A", "", transorg$tag) 
      transorg$tag <- as.numeric(transorg$tag)
      transorg <- transorg %>% select(tag,everything())
      formatC(transorg$tag, width=6,format="d", flag="0") 
      #a shorter version of this is sprintf("%06d", transorg$tag)
      
      
      bai_table_sub <- rbind(bai_table_sub, transorg, fill=TRUE)
    }
  }
}

bai_table <- rbind(bai_table_can, bai_table_sub, fill=TRUE)

q <- data.frame(colMeans(bai_table, na.rm=TRUE)) #remove tag name
q <- setDT(q, keep.rownames = TRUE)[]
q <- q[-1, ]
setnames(q,1,"year")
setnames(q,2,"bai")
q$year <- as.numeric(q$year)
q <- q[order(year)]
q <- q[-c(234:241), ] #values after 2009 drop off considerably and affect data

#shows relationship between bai and year
ggplot(data = q) +
  aes(x = year, y = bai) +
  geom_point(color = "#0c4c8a") +
  theme_minimal()

library(stats)
testMod <- lm(bai~year, data=q)
print(testMod)
summary(testMod)

q$resid <- residuals(testMod)
q_plot <- ggplot(data = q) +
  aes(x = year, y = resid) +
  geom_line(color = "#0c4c8a") +
  theme_minimal()

#plot and manually subset to 1950+
# plot_ly(q, x=q$year, y=q$resid, type="scatter", mode="lines")

pdsi <- read.csv("data/pdsi/pdsi_value_comparison.csv")

pdsi_true <- data.frame("year" = 1850:2017, "noaa_va_pdsi" = "")
pdsi_true$noaa_va_pdsi <- colMeans(matrix(pdsi$noaa_va_pdsi, nrow=12))
pdsi_true <- pdsi_true[-c(161:168), ]
pdsi_true <- pdsi_true[complete.cases(pdsi_true), ]
testMod1 <- lm(noaa_va_pdsi~year, data=pdsi_true)
pdsi_true$resid <- residuals(testMod1)
ggplot(data = pdsi_true) +
  aes(x = year, y = resid) +
  geom_line(color = "#0c4c8a") +
  theme_minimal()

#plot and manually subset to 1950+
# plot_ly(pdsi_true, x=pdsi_true$year, y=pdsi_true$resid, type="scatter", mode="line")

##########################################################################################
#5. add in climate and growth variables ####
library(ggplot2)
library(devtools) #for sourcing functions for regression equations
library(rgdal) #to read in shapefiles
library(broom) #for the tidy function
library(sf) #for mapping
library(ggthemes) #for removing graticules when making pdf
library(rgeos) #for distance calculation
library(RCurl) #for reading in URLs

##5a. add in turgor loss point values ####
#add in tlp values (from Krista github issue #6 https://github.com/SCBI-ForestGEO/McGregor_climate-sensitivity-variation/issues/6)
turgor <- data.frame("sp" = c("cagl", "caovl", "fagr", "fram", "juni", "litu", "pist", "qual", "qupr", "quru", "quve", "caco", "cato", "frni"), "tlp" = c(-2.1282533, -2.24839333, -2.57164, -2.1012133, -2.75936, -1.9212933, NA, -2.58412, -2.3601733, -2.6395867, -2.3879067, -2.1324133, -2.31424, NA))

trees_all$tlp <- turgor$tlp[match(trees_all$sp, turgor$sp)]

#tlp for pist is NA. Removing pist (because of the tlp NA) gives different results.
#trees_all <- trees_all[!trees_all$sp == "pist", ]

tlp_test <- trees_all[!duplicated(trees_all$tree), ]
tlp_test$tree <- as.numeric(tlp_test$tree)

ggplot(data = tlp_test) +
  aes(x = position, y = tlp) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  # facet_wrap(vars(year))


##5b. add in ring porosity qualifications ####
ring_porosity <- data.frame("sp" = c("cagl",  "caovl", "cato", "fagr", "fram", "juni",  "litu",  "pist",  "qual",  "qupr",  "quru",  "quve", "caco", "frni"), "rp" = c("ring", "ring", "ring", "diffuse", "ring", "semi-ring", "diffuse", NA, "ring", "ring", "ring", "ring", "ring", "ring"))

trees_all$rp <- ring_porosity$rp[match(trees_all$sp, ring_porosity$sp)]

#gives count of each rp value
rp_test <- trees_all[!duplicated(trees_all$tree), ]
rp_test$tree <- as.numeric(rp_test$tree)
ggplot(data = rp_test) +
  aes(x = rp) +
  geom_bar(fill = "#0c4c8a") +
  theme_minimal() +
  facet_wrap(vars(position))


##5c. add in elevation data ####
elev <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/spatial_data/elevation/full_stem_elevation_2013.csv"))

trees_all$elev.m <- elev$dem_sigeo[match(trees_all$tree, elev$tag)]

##5d. add in distance to water ####
## mapping code here is taken from survey_maps.R in Dendrobands Rscripts folder.

## I have not found a way to make this not involve personal directories without moving all the data to my folder, which I'm hesitant about doing due to data redundancy.
scbi_plot <- readOGR("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/spatial_data/shapefiles/20m_grid.shp")
deer <- readOGR("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/spatial_data/shapefiles/deer_exclosure_2011.shp")
roads <- readOGR("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/spatial_data/shapefiles/SCBI_roads_edits.shp")
streams <- readOGR("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/spatial_data/shapefiles/SCBI_streams_edits.shp")
NS_divide <- readOGR("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/Dendrobands/resources/maps/shapefiles/NS_divide1.shp")

#convert all shp to dataframe so that it can be used by ggplot
#if tidy isn't working, can also do: xxx_df <- as(xxx, "data.frame")
scbi_plot_df <- tidy(scbi_plot)
deer_df <- tidy(deer)
roads_df <- tidy(roads)
streams_df <- tidy(streams)
NS_divide_df <- tidy(NS_divide)

## now we get into code specific for this analysis
neil_map <- neil_list
neil_map$tag <- gsub("X", "", neil_map$tag)
neil_map$tag <- as.numeric(neil_map$tag)
neil_map <- neil_map[, c(1:6,23:24)]

map <- ggplot() +
  geom_path(data = scbi_plot_df, aes(x = long, y = lat, group = group))+
  geom_path(data=roads_df, aes(x=long, y=lat, group=group), 
            color="#996600", linetype=2)+
  geom_path(data=streams_df, aes(x=long, y=lat, group=group), color="blue")+
  geom_path(data=deer_df, aes(x=long, y=lat, group=group), size=1.1)+
  geom_point(data=neil_map, aes(x=NAD83_X, y=NAD83_Y), shape=19)+
  geom_text(data=neil_map, aes(x=NAD83_X, y=NAD83_Y, label=tag), 
            size=3, hjust=1.25, nudge_y=-1, nudge_x=1, check_overlap=TRUE)+
  theme(plot.title=element_text(vjust=0.1))+
  coord_sf(crs = "crs = +proj=merc", xlim=c(747350,747800), ylim=c(4308500, 4309125))

## calculating the distance requires some conversion. First, the points of the cored trees from neil_map must be in their own dataframe before they can be converted to a SpatialPoints object.
neil_map_sub <- neil_map[, c(7:8)]
neil_points <- SpatialPoints(neil_map_sub, proj4string = CRS(as.character("+proj=merc")))

## here, the minimum distance to water is calculated before binding with neil_map.
## A warning says that neil_points and streams are projected differently, but the output has been verified to be accurate.
distance_water <- data.frame(apply(gDistance(neil_points, streams, byid=TRUE), 2, min))
colnames(distance_water) <- "distance_water"
distance <- cbind(neil_map, distance_water)

## next, do a log transformation on the distances before adding as a column to trees_all (similar to the dbh calculations below)
distance$distance.ln.m <- log(distance$distance_water)
trees_all$distance.ln.m <- distance$distance.ln.m[match(trees_all$tree, distance$tag)]

## this is to double check the accuracy of the map.
distance_short <- distance[distance$distance_water <= 30, ]

map <- ggplot() +
  geom_path(data = scbi_plot_df, aes(x = long, y = lat, group = group))+
  geom_path(data=roads_df, aes(x=long, y=lat, group=group), 
            color="#996600", linetype=2)+
  geom_path(data=streams_df, aes(x=long, y=lat, group=group), color="blue")+
  geom_path(data=deer_df, aes(x=long, y=lat, group=group), size=1.1)+
  geom_point(data=distance_short, aes(x=NAD83_X, y=NAD83_Y), shape=19)+
  geom_text(data=distance_short, aes(x=NAD83_X, y=NAD83_Y, label=tag), 
            size=3, hjust=1.25, nudge_y=-1, nudge_x=1, check_overlap=TRUE)+
  theme(plot.title=element_text(vjust=0.1))+
  coord_sf(crs = "crs = +proj=merc", xlim=c(747350,747800), ylim=c(4308500, 4309125))


##5e. add in dbh for each year ####
###original method ####
# dbh <- trees_all[, c(1:4)]
# dbh$dbh2013 <- elev$dbh[match(dbh$tree, elev$tag)]
# 
# #create df with bark thickness log values and intercept values from Krista's paper (supplemental info)
# #https://besjournals.onlinelibrary.wiley.com/doi/epdf/10.1111/1365-2435.12470 
# #fagr does not have bark thickness measured because it is negligible
# bark <- data.frame(
#   "sp" = c("acru", "fagr", "litu", "nysy", "caco", "cagl", "caovl", "cato", "fram", "juni", "qual", "qupr", "quru", "quve", "ulru"), 
#   "bark_thick_ln" = c(-2.564, 0, -0.659, -0.611, -1.917, -0.495, -2.504, -0.945, 0.318, -0.293, -1.231, -0.647, -0.789, 1.5, 1.133),
#   "intercept" = c(0.599, 0, 0.425, 0.413, 0.503, 0.316, 0.703, 0.396, 0.295, 0.385, 0.526, 0.423, 0.341, 0.053, -0.057))
# 
# bark$bark_thick <- ifelse(bark$bark_thick_ln != 0, exp(bark$bark_thick_ln), bark$bark_thick_ln)
# 
# bark$bark_thick <- exp(bark$bark_thick_ln)
# 
# dbh$bark_thick <- bark$bark_thick[match(dbh$sp, bark$sp)]
# dbh$intercept <- bark$intercept[match(dbh$sp, bark$sp)]
# 
# #the main equation is based on ring widths. We have determined the equation to be
# # rw(pointer_year) <- 0.5*dbh2013 - bark_thick*(dbh2013^intercept) - sum(rw(pointer_year):rw(end)). The first part of the equation is here. Summing the pointer years happens with the "q" df below in the loop.
# dbh$rw_prelim <- (0.5*dbh$dbh2013) - (dbh$bark_thick*(dbh$dbh2013^dbh$intercept))
# 
# 
# dbh$dbh_old.mm <- "0" #in prep for below
# dbh$dbh_old.mm <- as.numeric(dbh$dbh_old.mm)
# 
# for (i in seq(along=widths)){
#   df <- widths[[i]] #the list "widths" comes from #4a-4b
#   colnames(df) <- gsub("A", "", colnames(df)) #remove "A"
#   colnames(df) <- gsub("^0", "", colnames(df)) #remove leading 0
#   
#   cols <- colnames(df) #define cols for below
#   colnames(df) <- gsub("^", "x", colnames(df)) #add "x" to make calling colnames below feasible
#   
#   for (j in seq(along=cols)){
#     for (k in seq(along=colnames(df))){
#       ring_ind <- cols[[j]]
#       ring_col <- colnames(df)[[k]]
#       
#       if(j==k){
#         #the output of this loop is 3 separate columns for each year's old dbh, hence why it is set to q as a dataframe before being combined below. Pointer_years_simple comes from #4d.
#         q <- data.frame(sapply(pointer_years_simple, function(x){
#           rw <- df[rownames(df)>=x, ]
#           ifelse(dbh$year == x & dbh$tree == ring_ind, 
#                  dbh$rw_prelim - sum(rw[, ring_col], na.rm=TRUE), 0)
#         }))
#         
#         q$dbh_old.mm <- q[,1] +q[,2] + q[,3] #add columns together
#         # q$dbh_old.mm <- q[,1] +q[,2] + q[,3] + q[,4]
#         dbh$dbh_old.mm <- dbh$dbh_old.mm + q$dbh_old.mm #combine with dbh
#       }
#     }
#   }
# }
# 
# # check <- dbh[dbh$dbh_old.mm == 0, ] #check if any tree was missed
# 
# trees_all$dbh_old.mm <- dbh$dbh_old.mm
# trees_all$dbh_old.mm <- ifelse(trees_all$dbh_old.mm < 0, 0, trees_all$dbh_old.mm)
# trees_all$dbh_old.mm <- ifelse(trees_all$dbh_old.mm > 0, trees_all$dbh_old.mm/10, trees_all$dbh_old.mm)
# trees_all$dbh_ln <- ifelse(trees_all$dbh_old.mm == 0, NA, log(trees_all$dbh_old.mm))


###new method ####
bark <- read.csv("data/SCBI_bark_depth.csv")
bark <- bark[bark$species %in% sp_can | bark$species %in% sp_subcan, ]

#1. Calculate diameter_nobark for 2008 = DBH.mm.2008-2*bark.depth.mm
bark$diam_nobark_2008.mm <- bark$DBH.mm.2008 - 2*bark$bark.depth.mm 

#2. log-transform both diam_nobark_2008 (x) and bark.depth.mm (y)
#3. Fit a linear model, and use model to predict log(bark.depth.mm)
source_gist("524eade46135f6348140")
ggplot(data = bark, aes(x = log(diam_nobark_2008.mm), y = log(bark.depth.mm))) +
  stat_smooth_func(geom="text",method="lm",hjust=0.16, vjust=-1,parse=TRUE) +
  geom_smooth(method="lm", se=FALSE, color="black") +
  geom_point(color = "#0c4c8a") +
  theme_minimal() +
  facet_wrap(vars(species))

#this full equation is used further down
ggplot(data = bark, aes(x = log(diam_nobark_2008.mm), y = log(bark.depth.mm))) +
  stat_smooth_func(geom="text",method="lm",hjust=0.16, vjust=-1,parse=TRUE) +
  geom_smooth(method="lm", se=FALSE, color="black") +
  geom_point(color = "#0c4c8a") +
  theme_minimal()

#no total regression equation at bottom because all species are accounted for in dataset.
bark$predict_barkthick.ln.mm <- NA
bark$predict_barkthick.ln.mm <- 
                  ifelse(bark$species == "caco", -1.56+0.416*log(bark$diam_nobark_2008.mm),
                  ifelse(bark$species == "cagl", -0.393+0.268*log(bark$diam_nobark_2008.mm),
                  ifelse(bark$species == "caovl", -2.18+0.651*log(bark$diam_nobark_2008.mm),
                  ifelse(bark$species == "cato", -0.477+0.301*log(bark$diam_nobark_2008.mm),
                  ifelse(bark$species == "fram", 0.418+0.268*log(bark$diam_nobark_2008.mm),
                  ifelse(bark$species == "juni", 0.346+0.279*log(bark$diam_nobark_2008.mm),
                  ifelse(bark$species == "litu", -1.14+0.463*log(bark$diam_nobark_2008.mm),
                  ifelse(bark$species == "qual", -2.09+0.637*log(bark$diam_nobark_2008.mm),
                  ifelse(bark$species == "qupr", -1.31+0.528*log(bark$diam_nobark_2008.mm),
                  ifelse(bark$species == "quru", -0.593+0.292*log(bark$diam_nobark_2008.mm),
                  ifelse(bark$species == "quve", 0.245+0.219*log(bark$diam_nobark_2008.mm),
                           bark$predict_barkthick.ln)))))))))))

#4. Take exponent of bark.depth.mm and make sure predicted values look good.
bark$predict_barkthick.mm <- exp(bark$predict_barkthick.ln.mm)

range(bark$predict_barkthick.mm - bark$bark.depth.mm)

#5. Get mean bark thickness per species in 2008.
## The equation for calculating old dbh, using 1999 as an example, is
## dbh1999 = dbh2008 - 2(ring.width2013 - ring.width1999) - 2(bark.depth2008) + 2(bark.depth1999)

## using the dataset from calculating the regression equations, we can get mean bark thickness per species in 2008.

##set up dbh dataframe
dbh <- trees_all[, c(1:4)]
scbi.stem1 <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/scbi.stem1.csv"))
dbh$dbh2008.mm <- scbi.stem1$dbh[match(dbh$tree, scbi.stem1$tag)]

mean_bark <- aggregate(bark$bark.depth.mm, by=list(bark$species), mean) #mm
colnames(mean_bark) <- c("sp", "mean_bark_2008.mm")

dbh$mean_bark_2008.mm <- ifelse(dbh$sp %in% mean_bark$sp, mean_bark$mean_bark_2008.mm[match(dbh$sp, mean_bark$sp)], mean(bark$bark.depth.mm))
dbh$mean_bark_2008.mm <- round(dbh$mean_bark_2008.mm, 2)

#6.Thus, the only value we're missing is bark depth in 1999.
## This is ok, because we can calculate from the regression equation per each species (all we need is diam_nobark_1999).Calculate diam_nobark_1999 using
## diam_nobark_1999 = dbh2008 - 2*(bark.depth2008) - 2*(sum(ring.width1999:ring.width2008))

##define this column before loop

dbh$diam_nobark_old.mm <- 0
for (i in seq(along=widths)){
  df <- widths[[i]] #the list "widths" comes from #4a-4b
  colnames(df) <- gsub("A", "", colnames(df)) #remove "A"
  colnames(df) <- gsub("^0", "", colnames(df)) #remove leading 0
  
  cols <- colnames(df) #define cols for below
  colnames(df) <- gsub("^", "x", colnames(df)) #add "x" to make calling colnames below feasible
  
  for (j in seq(along=cols)){
    for (k in seq(along=colnames(df))){
      ring_ind <- cols[[j]]
      ring_col <- colnames(df)[[k]]
      
      if(j==k){
        #the output of this loop is 3 separate columns for each year's old dbh, hence why it is set to q as a dataframe before being combined below. Pointer_years_simple comes from #4d.
        q <- data.frame(sapply(pointer_years_simple, function(x){
          rw <- df[rownames(df)>=x, ]
          ifelse(dbh$year == x & dbh$tree == ring_ind, 
                 dbh$dbh2008.mm - 2*(dbh$mean_bark_2008.mm) - sum(rw[rownames(rw) %in% c(x:2008), ring_col], na.rm=TRUE), 0)
        }))
        
        q$diam_nobark_old.mm <- q[,1] +q[,2] + q[,3] #add columns together
        # q$dbh_old.mm <- q[,1] +q[,2] + q[,3] + q[,4]
        dbh$diam_nobark_old.mm <- dbh$diam_nobark_old.mm + q$diam_nobark_old.mm #combine with dbh (it's the same order of rows) #mm
      }
    }
  }
}
    
#7. Calculate bark thickness using regression equation per appropriate sp
## log(bark.depth.1999) = intercept + log(diam_nobark)*constant
## bark.depth.1999 = exp(log(bark.depth.1999))

#the full equation at the bottom is the regression equation for all these species put together. "fagr" is given a bark thickness of 0 because it is negligble
#these equations are the same as above in #3 of this code section
dbh$bark_thick_old.ln.mm <- NA
dbh$bark_thick_old.ln.mm <- ifelse(dbh$sp == "caco", -1.56+0.416*log(dbh$diam_nobark_old.mm),
                      ifelse(dbh$sp == "cagl", -0.393+0.268*log(dbh$diam_nobark_old.mm),
                      ifelse(dbh$sp == "caovl", -2.18+0.651*log(dbh$diam_nobark_old.mm),
                      ifelse(dbh$sp == "cato", -0.477+0.301*log(dbh$diam_nobark_old.mm),
                      ifelse(dbh$sp == "fram", 0.418+0.268*log(dbh$diam_nobark_old.mm),
                      ifelse(dbh$sp == "juni", 0.346+0.279*log(dbh$diam_nobark_old.mm),
                      ifelse(dbh$sp == "litu", -1.14+0.463*log(dbh$diam_nobark_old.mm),
                      ifelse(dbh$sp == "qual", -2.09+0.637*log(dbh$diam_nobark_old.mm),
                      ifelse(dbh$sp == "qupr", -1.31+0.528*log(dbh$diam_nobark_old.mm),
                      ifelse(dbh$sp == "quru", -0.593+0.292*log(dbh$diam_nobark_old.mm),
                      ifelse(dbh$sp == "quve", 0.245+0.219*log(dbh$diam_nobark_old.mm),
                      ifelse(dbh$sp == "fagr", 0,
                                          -1.01+0.213*log(dbh$diam_nobark_old.mm)))))))))))))

dbh$bark_thick_old.mm <- ifelse(dbh$sp == "fagr", 0, exp(dbh$bark_thick_old.ln.mm))

#8. Add to solution from #6 to get full dbh1999
## dbh1999 = diam_nobark_1999 + 2*bark.depth.1999
dbh$dbh_old.mm <- dbh$diam_nobark_old.mm + 2*dbh$bark_thick_old.mm

##NOTE
##The first time I ran this code I was getting NaNs for one tree (140939), because the dbh in 2008 was listed as 16.9. I double-checked this, and that was the second stem, which we obviously didn't core at a size of 1.69 cm (or 2.2 cm in 2013). The dbh is meant to be the first stem. However, there was confusion with the dbh in the field. 

trees_all$dbh_old.mm <- dbh$dbh_old.mm[match(trees_all$tree, dbh$tree) & match(trees_all$year, dbh$year)] #mm
trees_all$dbh_old.cm <- trees_all$dbh_old.mm/10
trees_all$dbh.ln.cm <- log(trees_all$dbh_old.cm)

##5f. add in ratio of sapwood area to total wood ####
### It has been determined that since sapwood ratio is so tied to DBH (in other words, testing it in a model is akin to testing DBH again), that we are going to leave it out of the full models. However, I'm leaving the code here in case we want anything with it later.

#calculate sapwood area
##sapwood area[iii] = tree area (minus bark)[i] - heartwood area[ii]
sap <- read.csv("data/SCBI_Sapwood_Data.csv", stringsAsFactors = FALSE)
sap <- sap[,c(1:5,8:10,24)]
sap$sp <- paste0(gsub("^(..).*", "\\1", sap$Latin), 
                       gsub("^.* (..).*", "\\1", sap$Latin))
sap$sp <- tolower(sap$sp)

##subtract bark thickness from dbh
##NOTE bark thickness is from 2008, even tho sap data collected 2010
##[[i]]
sap$dbh_nobark.mm <- 0
for (i in seq(along=mean_bark$mean_bark_2008)){
  sub <- mean_bark[mean_bark$mean_bark_2008[[i]] == mean_bark$mean_bark_2008, ]
  sap$dbh_nobark.mm <- ifelse(sap$sp == sub$sp, sap$DBH-sub$mean_bark_2008, sap$dbh_nobark)
}

#[ii]
#heartwood radius = 0.5*dbh – sapwood depth (mm)
sap$hw_rad.mm <- 0.5*sap$dbh_nobark.mm - sap$sapwood.depth..mm.

#Heartwood area = pi*(heartwood radius)^2 (mm^2)
sap$hw_area.mm2 <- pi*(sap$hw_rad.mm)^2

#[iii]
#Sapwood area = pi*((0.5*dbh)^2) – heartwood area (cm^2 with the /100)
sap$sap_area.cm2 <- (pi*(0.5*sap$dbh_nobark.mm)^2 - sap$hw_area.mm2)/100

sap <- sap[sap$sp %in% sp_can | sap$sp %in% sp_subcan, ]

#ratio = sapwood area:area without bark
##calculate ratio to find the regression equations
sap$dbh_nobark.cm <- sap$dbh_nobark.mm/10
sap$total_wood_area.cm2 <- pi*(sap$dbh_nobark.cm/2)^2
sap$sap_ratio <- sap$sap_area.cm2/sap$total_wood_area.cm2

#these equations are for getting the sapwood_area for the dbh df, which are then converted to sap_ratio for that df.
source_gist("524eade46135f6348140")
#DBH = mm, sap_area = cm^2
ggplot(data = sap, aes(x = log(DBH), y = log(sap_area.cm2))) +
  stat_smooth_func(geom="text",method="lm",hjust=0.16, vjust=0.8,parse=TRUE) +
  geom_smooth(method="lm", se=FALSE, color="black") +
  geom_point(color = "#0c4c8a") +
  theme_minimal() +
  facet_wrap(vars(sp))

ggplot(data = sap, aes(x = log(DBH), y = log(sap_area.cm2))) +
  stat_smooth_func(geom="text",method="lm",hjust=0.16, vjust=-1,parse=TRUE) +
  geom_smooth(method="lm", se=FALSE, color="black") +
  geom_point(color = "#0c4c8a") +
  theme_minimal()

#original equations. I'm not sure what happened, but I think after changing some units, I forgot to update these. Nevertheless, I'm leaving them as is in case I messed up.
# #the bottom equation is the total regression equation
# dbh$sapwood_area.ln <- NA
# dbh$sapwood_area.ln <- ifelse(dbh$sp == "caco", 6.17-0.419*log(dbh$dbh_old.mm),
#                        ifelse(dbh$sp == "cagl", 5.32-0.26*log(dbh$dbh_old.mm),
#                        ifelse(dbh$sp == "cato", 6.51-0.444*log(dbh$dbh_old.mm),
#                        ifelse(dbh$sp == "fram", 2.19+0.326*log(dbh$dbh_old.mm),
#                        ifelse(dbh$sp == "juni", 5.53-0.404*log(dbh$dbh_old.mm),
#                        ifelse(dbh$sp == "litu", 4.31-0.0718*log(dbh$dbh_old.mm),
#                        ifelse(dbh$sp == "qual", 7.09-0.692*log(dbh$dbh_old.mm),
#                        ifelse(dbh$sp == "qupr", 4.99-0.305*log(dbh$dbh_old.mm),
#                        ifelse(dbh$sp == "quru", 4.27-0.282*log(dbh$dbh_old.mm),
#                        ifelse(dbh$sp == "quve", 5.1-0.402*log(dbh$dbh_old.mm),
#                                    6.6-0.543*log(dbh$dbh_old.mm)))))))))))

#the bottom equation is the total regression equation
dbh$sapwood_area.ln <- NA
dbh$sapwood_area.ln <- ifelse(dbh$sp == "caco", -3.41+1.6*log(dbh$dbh_old.mm),
                       ifelse(dbh$sp == "cagl", -4.34+1.77*log(dbh$dbh_old.mm),
                       ifelse(dbh$sp == "cato", -3.14+1.59*log(dbh$dbh_old.mm),
                       ifelse(dbh$sp == "fram", -7.75+2.4*log(dbh$dbh_old.mm),
                       ifelse(dbh$sp == "juni", -4.23+1.64*log(dbh$dbh_old.mm),
                       ifelse(dbh$sp == "litu", -5.5+1.98*log(dbh$dbh_old.mm),
                       ifelse(dbh$sp == "qual", -2.66+1.35*log(dbh$dbh_old.mm),
                       ifelse(dbh$sp == "qupr", -4.89+1.76*log(dbh$dbh_old.mm),
                       ifelse(dbh$sp == "quru", -5.35+1.74*log(dbh$dbh_old.mm),
                       ifelse(dbh$sp == "quve", -4.57+1.63*log(dbh$dbh_old.mm),
                                   -3.13+1.5*log(dbh$dbh_old.mm)))))))))))

dbh$sapwood_area.cm2 <- exp(dbh$sapwood_area.ln)

#calculate ratio for each tree using regression equations
##prepare: get radius.w/o.bark (/2) and convert to cm (/10).
dbh$radius_nobark.cm <- (dbh$diam_nobark_old/2)/10

##total wood area = pi*(radius.nobark)^2 (cm^2)
dbh$total_wood_area.cm2 <- pi*(dbh$radius_nobark.cm)^2

dbh$sap_ratio <- dbh$sapwood_area.cm2/dbh$total_wood_area.cm2
trees_all$sap_ratio <- dbh$sap_ratio[match(trees_all$tree, dbh$tree) & match(trees_all$year, dbh$year)]

##5g. add in tree heights ####
## taken from the canopy_heights script
#the full equation is using all points for which we have data to create the equation, despite that for several species we don't have enough data to get a sp-specific equation
trees_all$height.ln.m <- ifelse(trees_all$sp == "caco", (0.55+0.766*trees_all$dbh.ln.cm),
                      ifelse(trees_all$sp == "cagl", (0.652+0.751*trees_all$dbh.ln.cm),
                      ifelse(trees_all$sp == "caovl", (0.9+0.659*trees_all$dbh.ln.cm),
                      ifelse(trees_all$sp == "cato", (0.879+0.668*trees_all$dbh.ln.cm),
                      ifelse(trees_all$sp == "fagr", (0.513+0.712*trees_all$dbh.ln.cm),
                      ifelse(trees_all$sp == "litu", (1.57+0.488*trees_all$dbh.ln.cm),
                      ifelse(trees_all$sp == "quru", (1.13+0.54*trees_all$dbh.ln.cm),
                             (1.11+0.573*trees_all$dbh.ln.cm))))))))
                             #0.849+0.659*trees_all$dbh_ln.cm -> original equation only using points from the species for which we had equations. This was yielding predicted heights for some trees of about 54m.

trees_all$height.m <- exp(trees_all$height.ln.m) #m, because these equations come from a plotting of log(DBH in cm) against log(height in m).

##5h. add in all crown positions ####

positions <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_dimensions/tree_crowns/cored_dendroband_crown_position_data/dendro_cored_full.csv"))

trees_all$position_all <- positions$crown.position[match(trees_all$tree, positions$tag)]
trees_all$position_all <- gsub("D", "dominant", trees_all$position_all)
trees_all$position_all <- gsub("C", "co-dominant", trees_all$position_all)
trees_all$position_all <- gsub("I", "intermediate", trees_all$position_all)
trees_all$position_all <- gsub("S", "suppressed", trees_all$position_all)

#this has been proven to be roughly equivalent to position_all, so we're sticking with position_all
# trees_all$illum <- positions$illum[match(trees_all$tree, positions$tag)]
# trees_all$illum <- as.character(trees_all$illum)

#this csv has avg/min/max dbh for each canopy position by sp
# positionsp <- read.csv("data/core_chronologies_by_crownposition.csv")

##5i. remove all NAs and one bad tree ####
trees_all <- trees_all[complete.cases(trees_all), ]

##fram 140939 has been mislabeled. It is recorded as having a small dbh when that is the second stem. In terms of canopy position, though, it fell between time of coring and when positions were recorded, thus we do not know its position.
trees_all <- trees_all[!trees_all$tree == 140939, ]

##5j. remove resistance values >2 ####
trees_all <- trees_all[trees_all$resist.value <=2,]
##5k. make subsets for individual years, combine all to list ####
# x1964 <- trees_all[trees_all$year == 1964, ]
x1966 <- trees_all[trees_all$year == 1966, ]
x1977 <- trees_all[trees_all$year == 1977, ]
x1999 <- trees_all[trees_all$year == 1999, ]

model_df <- list(trees_all, x1966, x1977, x1999)
names(model_df) <- c("all_years", "x1966", "x1977", "x1999")
##############################################################################################
#6. mixed effects model for output of #5. ####
library(lme4)
library(AICcmodavg) #aictab function
library(car)
library(piecewiseSEM) #for R^2 values for all model outputs in a list
library(MuMIn) #for R^2 values of one model output
library(stringr)
library(dplyr)

##6a. Determine best model to use with AICc ####
##6ai. test predictions for paper and put in table
##initial table with models based on github issue predictions ####
summary_models <- data.frame(
  "prediction" = c("1.0", "1.1", "1.2a", "1.2b", "1.2c1, 1.3a1", "1.2c2", "1.3b1", "1.3a2", "1.3b2", "2.1", "2.2", "4"), 
  "model_vars_all_years" = 
    c("resist.value ~ dbh.ln.cm+year+(1|sp/tree)", 
       "resist.value ~ height.ln.m+year+(1|sp/tree)", 
       "resist.value ~ position_all+year+(1|sp/tree)", 
       "resist.value ~ position_all+height.ln.m+year+(1|sp/tree)", 
       "resist.value ~ elev.m+height.ln.m+year+(1|sp/tree)", 
       "resist.value ~ elev.m*height.ln.m+height.ln.m+year+(1|sp/tree)",
       "resist.value ~ elev.m*height.ln.m+height.ln.m+year+(1|sp/tree)",
       "resist.value ~ distance.ln.m+height.ln.m+year+(1|sp/tree)", 
       "resist.value ~ distance.ln.m*height.ln.m+height.ln.m+year+(1|sp/tree)", 
       "resist.value ~ tlp+height.ln.m+year+(1|sp/tree)", 
       "resist.value ~ rp+height.ln.m+year+(1|sp/tree)",
        "resist.value ~ sap_ratio+position_all+height.ln.m+height.ln.m*elev.m+distance.ln.m+tlp+rp+year+(1|sp/tree)"),
  "null_model_all_years" = NA,
  "model_vars_sep_years" = NA,
  "null_model_sep_years" = NA,
  "response_predict" = c(-1, -1, -1, -1, 1, 1, -1, 1, -1, -1, 1, NA),
  "response_sign" = c("-", "-", "dominant < codominant < intermediate < supressed", "dominant < codominant < intermediate < supressed", "+", "+", "-", "+", "-", "-", "ring>diffuse", "+"),
   "dAIC_all_years" = NA,
    "response_obs_all" = NA,
    "coef_all_years" = NA,
   "dAIC_1964.1966" = NA,
    "response_obs_1964.1966" = NA,
    "coef_1964.1966" = NA,
   "dAIC_1977" = NA, 
    "response_obs_1977" = NA,
    "coef_1977" = NA,
   "dAIC_1999" = NA,
    "response_obs_1999" = NA,  
    "coef_1999" = NA,
    "notes" = "",
    "coef_all_big" = NA,
    "coef_1966_big" = NA,
    "coef_1977_big" = NA,
    "coef_1999_big" = NA)

# change factor columns to character
summary_models %>% mutate_if(is.factor, as.character) -> summary_models

# fill in other columns
summary_models[c(1:3), 3] <- "resist.value ~ year+(1|sp/tree)"
summary_models[c(4:12), 3] <- "resist.value ~ height.ln.m+year+(1|sp/tree)"
summary_models$model_vars_sep_years <- gsub("year\\+|/tree", "", summary_models$model_vars_all_years)
summary_models$null_model_sep_years <- gsub("year\\+|/tree", "", summary_models$null_model_all_years)

#define vectors to be used in loop
summary_mod_vars_all <- summary_models$model_vars_all_years
summary_mod_vars_sep <- summary_models$model_vars_sep_years
summary_mod_null_all <- summary_models$null_model_all_years
summary_mod_null_sep <- summary_models$null_model_sep_years

##this loop goes through each mix of effects from each prediction (nrow(summary_models)), and runs those models for each of the datasets (all years and the three individual ones). For each iteration (44 total), it calculates dAIC (AIC of model with variable defined in model_vars columns minus the AIC of the null model) and the coefficients before putting them in the table created above.
for (i in seq_along(model_df)){
  for (h in seq(along=summary_mod_vars_all)){
    if (i==1){
      #structure of creating the model strings come from 6a above.
      response <- gsub(" ~.*", "", summary_mod_vars_all[[h]])
      effects <- unlist(strsplit(summary_mod_vars_all[[h]], "\\+|~ "))[-1]
      
      #create all combinations of random / fixed effects
      effects_comb <- 
        unlist( sapply( seq_len(length(effects)), 
                        function(i) {
                          apply( combn(effects,i), 2, function(x) paste(x, collapse = "+"))
                        }))
      #make table
      var_comb <- expand.grid(response, effects_comb) 
      var_comb <- var_comb[grepl("1", var_comb$Var2), ] #only keep in fixed/random combos
      var_comb <- var_comb[grepl("year", var_comb$Var2), ] #keep year in for drought sake
      
    } 
    else {
      
      #define response and effects
      response <- gsub(" ~.*", "", summary_mod_vars_sep[[h]])
      effects <- unlist(strsplit(summary_mod_vars_sep[[h]], "\\+|~ "))[-1]
      # all fixed effects <- c("position", "tlp", "rp", "elev.m", "dbh_ln", "height_ln.m")
      
      #create all combinations of random / fixed effects
      effects_comb <- 
        unlist( sapply( seq_len(length(effects)), 
                        function(i) {
                          apply( combn(effects,i), 2, function(x) paste(x, collapse = "+"))
                        }))
      #=make table
      var_comb <- expand.grid(response, effects_comb) 
      var_comb <- var_comb[grepl("1", var_comb$Var2), ] #only keep in fixed+random combos
    }
    
    #formulas for all combinations.
    formula_vec <- sprintf("%s ~ %s", var_comb$Var1, var_comb$Var2)
    
    # create list of model outputs
    lmm_all <- lapply(formula_vec, function(x){
      fit1 <- lmer(x, data = model_df[[i]], REML=FALSE, 
                   control = lmerControl(optimizer ="Nelder_Mead"))
      return(fit1)
    })
    names(lmm_all) <- formula_vec
    
    var_aic <- aictab(lmm_all, second.ord=TRUE, sort=TRUE) #rank based on AICc
    r <- rsquared(lmm_all) #gives R^2 values for models. "Marginal" is the R^2 for just the fixed effects, "Conditional" is the R^2 for everything.
    
    #fill in table
    if(i == 1){
      #isolate the AIC values of the target (sub) and null models, then math
      var_aic_sub <- var_aic[var_aic$Modnames == summary_mod_vars_all[[h]], ]
      var_aic_null <- var_aic[var_aic$Modnames == summary_mod_null_all[[h]], ]
      summary_models[,8][[h]] <- round(var_aic_null$Delta_AICc - var_aic_sub$Delta_AICc, 2)
      var_aic_sub$Modnames <- as.character(var_aic_sub$Modnames)
      
      #this loop says for the models run for this iteration of h, take the model output represented by the target model (1). Get the coefficients and put in df (2). Rename variables such that you only pull what you need (3) and extract the coefficient for the variable you want (4).
      for (z in seq(along = lmm_all)){
        if (z == rownames(var_aic_sub)){ ##1
          coeff <- data.frame(coef(summary(lmm_all[[z]]))[ , "Estimate"]) ##2
          coeff[,2] <- rownames(coeff)
          colnames(coeff) <- c("value", "model_var")
          
          if (h == 3){ ##3
            coeff$model_var <- gsub("subcanopy", "", coeff$model_var)
          } else if (h == 4){
            coeff$model_var <- gsub("subcanopy", "", coeff$model_var)
            coeff$model_var <- gsub("height", "ht_nat_ln", coeff$model_var)
          } else if (h %in% 5:10){
            coeff$model_var <- gsub("height", "ht_nat_ln", coeff$model_var)
          } else if (h == 11){
            coeff$model_var <- gsub("ring", "", coeff$model_var)
            coeff$model_var <- gsub("height", "ht_nat_ln", coeff$model_var)
          } else if (h == 12){
            coeff$model_var <- gsub("subcanopy", "", coeff$model_var)
            coeff$model_var <- gsub("ring", "", coeff$model_var)
            coeff$model_var <- gsub("height", "ht_nat_ln", coeff$model_var)
            
            full_mod <- var_aic[!duplicated(var_aic$Delta_AICc), ]
            full_mod <- full_mod[1:10, c(1,4)]
            full_mod[11,] <- var_aic_sub[,c(1,4)]
            colnames(full_mod) <- c("Modnames_all_years", "dAIC_all_years")
          }
          
          for (y in seq(along = coeff$model_var)){ ##4
            same <- coeff$model_var[[y]]
            
            if(grepl(same, summary_mod_vars_all[[h]])){
              coeff_sub <- coeff[coeff$model_var == same, ]
              }
            }
          }
        }
      
      #update the coefficient value
       summary_models[,9][[h]] <- ifelse(coeff_sub$value<0, -1, 1)
       summary_models[,10][[h]] <- ifelse(h == 12, NA, coeff_sub$value)

      #update the table. If the sign conventions of the coefficient and the predicted response do not match, assign NA.
      # if (h!=12){
      #   summary_models[,8][[h]] <- ifelse(
      #     (coeff_sub$value <0 & summary_models$response_predict[[h]] <0) |
      #       (coeff_sub$value >0 & summary_models$response_predict[[h]] >0),
      #     summary_models[,8][[h]], NA)
      # }
      
      if(h==12){
        coeff <- coeff[-1,]
        coeff_max <- coeff[coeff$value == max(coeff$value), ]
        summary_models[,21][[h]] <- coeff_max$model_var
      }
    } 
    else if (i == 2) {
      #isolate the AIC values of the target (sub) and null models, then math
      var_aic_sub <- var_aic[var_aic$Modnames == summary_mod_vars_sep[[h]], ]
      var_aic_null <- var_aic[var_aic$Modnames == summary_mod_null_sep[[h]], ]
      summary_models[,11][[h]] <- round(var_aic_null$Delta_AICc - var_aic_sub$Delta_AICc, 2)
      var_aic_sub$Modnames <- as.character(var_aic_sub$Modnames)
      
      #this loop says for the models run for this iteration of h, take the model output represented by the target model (1). Get the coefficients and put in df (2). Rename variables such that you only pull what you need (3) and extract the coefficient for the variable you want (4).
      for (z in seq(along = lmm_all)){
        if (z == rownames(var_aic_sub)){ ##1
          coeff <- data.frame(coef(summary(lmm_all[[z]]))[ , "Estimate"]) ##2
          coeff[,2] <- rownames(coeff)
          colnames(coeff) <- c("value", "model_var")
          
          if (h == 3){ ##3
            coeff$model_var <- gsub("subcanopy", "", coeff$model_var)
          } else if (h == 4){
            coeff$model_var <- gsub("subcanopy", "", coeff$model_var)
            coeff$model_var <- gsub("height", "ht_nat_ln", coeff$model_var)
          } else if (h %in% 5:10){
            coeff$model_var <- gsub("height", "ht_nat_ln", coeff$model_var)
          } else if (h == 11){
            coeff$model_var <- gsub("ring", "", coeff$model_var)
            coeff$model_var <- gsub("height", "ht_nat_ln", coeff$model_var)
          } else if (h == 12){
            coeff$model_var <- gsub("subcanopy", "", coeff$model_var)
            coeff$model_var <- gsub("ring", "", coeff$model_var)
            coeff$model_var <- gsub("height", "ht_nat_ln", coeff$model_var)
            
            full_mod_1966 <- var_aic[!duplicated(var_aic$Delta_AICc), ]
            full_mod_1966 <- full_mod_1966[1:10, c(1,4)]
            full_mod_1966[11,] <- var_aic_sub[,c(1,4)]
            colnames(full_mod_1966) <- c("Modnames_1966", "dAIC_1966")
          }
          
          for (y in seq(along = coeff$model_var)){ ##4
            same <- coeff$model_var[[y]]
            
            if(grepl(same, summary_mod_vars_all[[h]])){
              coeff_sub <- coeff[coeff$model_var == same, ]
            }
          }
        }
      }
      
      #update the coefficient value
      summary_models[,12][[h]] <- ifelse(coeff_sub$value<0, -1, 1)
      summary_models[,13][[h]] <- ifelse(h == 12, NA, coeff_sub$value)
      
      #update the table. If the sign conventions of the coefficient and the predicted response do not match, assign NA.
      # if (h!=12){
      #   summary_models[,10][[h]] <- ifelse(
      #     (coeff_sub$value <0 & summary_models$response_predict[[h]] <0) |
      #       (coeff_sub$value >0 & summary_models$response_predict[[h]] >0),
      #     summary_models[,10][[h]], NA) 
      # }
     
      if(h==12){
        coeff <- coeff[-1,]
        coeff_max <- coeff[coeff$value == max(coeff$value), ]
        summary_models[,22][[h]] <- coeff_max$model_var
      }
    } 
    else if (i == 3){
      #isolate the AIC values of the target (sub) and null models, then math
      var_aic_sub <- var_aic[var_aic$Modnames == summary_mod_vars_sep[[h]], ]
      var_aic_null <- var_aic[var_aic$Modnames == summary_mod_null_sep[[h]], ]
      summary_models[,14][[h]] <- round(var_aic_null$Delta_AICc - var_aic_sub$Delta_AICc, 2)
      var_aic_sub$Modnames <- as.character(var_aic_sub$Modnames)
      
      #this loop says for the models run for this iteration of h, take the model output represented by the target model (1). Get the coefficients and put in df (2). Rename variables such that you only pull what you need (3) and extract the coefficient for the variable you want (4).
      for (z in seq(along = lmm_all)){
        if (z == rownames(var_aic_sub)){ ##1
          coeff <- data.frame(coef(summary(lmm_all[[z]]))[ , "Estimate"]) ##2
          coeff[,2] <- rownames(coeff)
          colnames(coeff) <- c("value", "model_var")
          
          if (h == 3){ ##3
            coeff$model_var <- gsub("subcanopy", "", coeff$model_var)
          } else if (h == 4){
            coeff$model_var <- gsub("subcanopy", "", coeff$model_var)
            coeff$model_var <- gsub("height", "ht_nat_ln", coeff$model_var)
          } else if (h %in% 5:10){
            coeff$model_var <- gsub("height", "ht_nat_ln", coeff$model_var)
          } else if (h == 11){
            coeff$model_var <- gsub("ring", "", coeff$model_var)
            coeff$model_var <- gsub("height", "ht_nat_ln", coeff$model_var)
          } else if (h == 12){
            coeff$model_var <- gsub("subcanopy", "", coeff$model_var)
            coeff$model_var <- gsub("ring", "", coeff$model_var)
            coeff$model_var <- gsub("height", "ht_nat_ln", coeff$model_var)
            
            full_mod_1977 <- var_aic[!duplicated(var_aic$Delta_AICc), ]
            full_mod_1977 <- full_mod_1977[1:10, c(1,4)]
            full_mod_1977[11,] <- var_aic_sub[,c(1,4)]
            colnames(full_mod_1977) <- c("Modnames_1977", "dAIC_1977")
          }
          
          for (y in seq(along = coeff$model_var)){ ##4
            same <- coeff$model_var[[y]]
            
            if(grepl(same, summary_mod_vars_all[[h]])){
              coeff_sub <- coeff[coeff$model_var == same, ]
            }
          }
        }
      }
      
      #update the coefficient value
      summary_models[,15][[h]] <- ifelse(coeff_sub$value<0, -1, 1)
      summary_models[,16][[h]] <- ifelse(h == 12, NA, coeff_sub$value)
      
      #update the table. If the sign conventions of the coefficient and the predicted response do not match, assign NA.
      # if(h!=12){
      #   summary_models[,12][[h]] <- ifelse(
      #     (coeff_sub$value <0 & summary_models$response_predict[[h]] <0) |
      #       (coeff_sub$value >0 & summary_models$response_predict[[h]] >0),
      #     summary_models[,12][[h]], NA)
      # }
      
      if(h==12){
        coeff <- coeff[-1,]
        coeff_max <- coeff[coeff$value == max(coeff$value), ]
        summary_models[,23][[h]] <- coeff_max$model_var
      }
    } 
    else if (i == 4){
      #isolate the AIC values of the target (sub) and null models, then math
      var_aic_sub <- var_aic[var_aic$Modnames == summary_mod_vars_sep[[h]], ]
      var_aic_null <- var_aic[var_aic$Modnames == summary_mod_null_sep[[h]], ]
      summary_models[,17][[h]] <- round(var_aic_null$Delta_AICc - var_aic_sub$Delta_AICc, 2)
      var_aic_sub$Modnames <- as.character(var_aic_sub$Modnames)
      
      #this loop says for the models run for this iteration of h, take the model output represented by the target model (1). Get the coefficients and put in df (2). Rename variables such that you only pull what you need (3) and extract the coefficient for the variable you want (4).
      for (z in seq(along = lmm_all)){
        if (z == rownames(var_aic_sub)){ ##1
          coeff <- data.frame(coef(summary(lmm_all[[z]]))[ , "Estimate"]) ##2
          coeff[,2] <- rownames(coeff)
          colnames(coeff) <- c("value", "model_var")
          
          if (h == 3){ ##3
            coeff$model_var <- gsub("subcanopy", "", coeff$model_var)
          } else if (h == 4){
            coeff$model_var <- gsub("subcanopy", "", coeff$model_var)
            coeff$model_var <- gsub("height", "ht_nat_ln", coeff$model_var)
          } else if (h %in% 5:10){
            coeff$model_var <- gsub("height", "ht_nat_ln", coeff$model_var)
          } else if (h == 11){
            coeff$model_var <- gsub("ring", "", coeff$model_var)
            coeff$model_var <- gsub("height", "ht_nat_ln", coeff$model_var)
          } else if (h == 12){
            coeff$model_var <- gsub("subcanopy", "", coeff$model_var)
            coeff$model_var <- gsub("ring", "", coeff$model_var)
            coeff$model_var <- gsub("height", "ht_nat_ln", coeff$model_var)
            
            full_mod_1999 <- var_aic[!duplicated(var_aic$Delta_AICc), ]
            full_mod_1999 <- full_mod_1999[1:10, c(1,4)]
            full_mod_1999[11,] <- var_aic_sub[,c(1,4)]
            colnames(full_mod_1999) <- c("Modnames_1999", "dAIC_1999")
            
            full_mod_all <- cbind(full_mod, full_mod_1966, full_mod_1977, full_mod_1999)
          }
          
          for (y in seq(along = coeff$model_var)){ ##4
            same <- coeff$model_var[[y]]
            
            if(grepl(same, summary_mod_vars_all[[h]])){
              coeff_sub <- coeff[coeff$model_var == same, ]
            }
          }
        }
      }
      
      #update the coefficient value
      summary_models[,18][[h]] <- ifelse(coeff_sub$value<0, -1, 1)
      summary_models[,19][[h]] <- ifelse(h == 12, NA, coeff_sub$value)
      
      #update the table. If the sign conventions of the coefficient and the predicted response do not match, assign NA.
      # if (h!=12){
      #   summary_models[,14][[h]] <- ifelse(
      #     (coeff_sub$value <0 & summary_models$response_predict[[h]] <0) |
      #       (coeff_sub$value >0 & summary_models$response_predict[[h]] >0),
      #     summary_models[,14][[h]], NA)
      # }
      
      if (h==12){
        coeff <- coeff[-1,]
        coeff_max <- coeff[coeff$value == max(coeff$value), ]
        summary_models[,24][[h]] <- coeff_max$model_var
      }
    }
  }
}

#csv has a 1 in the title to make sure any notes in current file are not overwritten
write.csv(summary_models, "manuscript/results_individual_years1.csv", row.names=FALSE)
write.csv(full_mod_all, "manuscript/full_models_dAIC1.csv", row.names=FALSE)

##table looking at only full model over all years ####
##we ran all variables (aka a full model) against all years combined and found that position, height*elev, tlp, and rp were the variables in the best model. Using this knowledge, here we created a dfferent version of the original table.
summary_models <- data.frame(
  "prediction" = c("1.1", "1.2b", "1.2c1, 1.3a1", "1.2c2", "1.3b1", "1.2c2,1.3b1", "2.1", "2.2"), 
  "model_vars_all_years" = 
    c("resist.value ~ position+height_ln.m+elev.m+tlp+rp+year+(1|sp/tree)",
      "resist.value ~ position+height_ln.m+elev.m+height_ln.m*elev.m+tlp+rp+year+(1|sp/tree)",
      "resist.value ~ position+height_ln.m+elev.m+tlp+rp+year+(1|sp/tree)",
      "resist.value ~ position+height_ln.m+elev.m+height_ln.m*elev.m+tlp+rp+year+(1|sp/tree)",
      "resist.value ~ position+height_ln.m+elev.m+height_ln.m*elev.m+tlp+rp+year+(1|sp/tree)",
      "resist.value ~ position+height_ln.m+elev.m+height_ln.m*elev.m+tlp+rp+year+(1|sp/tree)",
      "resist.value ~ position+height_ln.m+elev.m+height_ln.m*elev.m+tlp+rp+year+(1|sp/tree)",
      "resist.value ~ position+height_ln.m+elev.m+height_ln.m*elev.m+tlp+rp+year+(1|sp/tree)"),
  "null_model_all_years" = 
    c("resist.value ~ position+elev.m+tlp+rp+year+(1|sp/tree)",
      "resist.value ~ height_ln.m*elev.m+tlp+rp+year+(1|sp/tree)",
      "resist.value ~ position+height_ln.m+tlp+rp+year+(1|sp/tree)",
      "resist.value ~ position+height_ln.m+elev.m+tlp+rp+year+(1|sp/tree)",
      "resist.value ~ position+height_ln.m+elev.m+tlp+rp+year+(1|sp/tree)",
      "resist.value ~ position+height_ln.m+tlp+rp+year+(1|sp/tree)",
      "resist.value ~ position+height_ln.m*elev.m+rp+year+(1|sp/tree)",
      "resist.value ~ position+height_ln.m*elev.m+tlp+year+(1|sp/tree)"),
  "response_predict" = c(1, 1, -1, 1, -1, 1, -1, 1),
  "response_sign" = c("+", "canopy<subcanopy", "-", "+", "-", "+", "-", "ring>diffuse"),
  "dAIC_all_years" = NA,
  "coef_all_years" = NA,
  "coef_var" = NA,
  "coef_all_big" = NA,
  "notes" = "")

# change factor columns to character
summary_models %>% mutate_if(is.factor, as.character) -> summary_models

#define vectors to be used in loop
summary_mod_vars_all <- summary_models$model_vars_all_years
summary_mod_null_all <- summary_models$null_model_all_years

##this loop goes through each mix of effects from each prediction (nrow(summary_models)), and runs those models for the full years. it calculates dAIC (AIC of model with variable defined in model_vars columns minus the AIC of the null model) and the coefficients before putting them in the table created above.
for (i in seq_along(model_df)){
  for (h in seq(along=summary_mod_vars_all)){
    if (i==1){
      #structure of creating the model strings come from 6a above.
      response <- gsub(" ~.*", "", summary_mod_vars_all[[h]])
      effects <- unlist(strsplit(summary_mod_vars_all[[h]], "\\+|~ "))[-1]
      
      #create all combinations of random / fixed effects
      effects_comb <- 
        unlist( sapply( seq_len(length(effects)), 
                        function(i) {
                          apply( combn(effects,i), 2, function(x) paste(x, collapse = "+"))
                        }))
      #make table
      var_comb <- expand.grid(response, effects_comb) 
      var_comb <- var_comb[grepl("1", var_comb$Var2), ] #only keep in fixed/random combos
      var_comb <- var_comb[grepl("year", var_comb$Var2), ] #keep year in for drought sake
      
    } 
    
    #formulas for all combinations.
    formula_vec <- sprintf("%s ~ %s", var_comb$Var1, var_comb$Var2)
    
    # create list of model outputs
    lmm_all <- lapply(formula_vec, function(x){
      fit1 <- lmer(x, data = model_df[[i]], REML=FALSE, 
                   control = lmerControl(optimizer ="Nelder_Mead"))
      return(fit1)
    })
    names(lmm_all) <- formula_vec
    
    var_aic <- aictab(lmm_all, second.ord=TRUE, sort=TRUE) #rank based on AICc
    r <- rsquared(lmm_all) #gives R^2 values for models. "Marginal" is the R^2 for just the fixed effects, "Conditional" is the R^2 for everything.
    
    #fill in table
    if(i == 1){
      #isolate the AIC values of the target (sub) and null models, then math
      var_aic_sub <- var_aic[var_aic$Modnames == summary_mod_vars_all[[h]], ]
      var_aic_null <- var_aic[var_aic$Modnames == summary_mod_null_all[[h]], ]
      summary_models[,6][[h]] <- round(var_aic_null$Delta_AICc - var_aic_sub$Delta_AICc, 2)
      var_aic_sub$Modnames <- as.character(var_aic_sub$Modnames)

      ## get coefficients and put in table
      for (z in seq(along = lmm_all)){
        if (z == rownames(var_aic_sub)){ ##1
          coeff <- data.frame(coef(summary(lmm_all[[z]]))[ , "Estimate"]) ##2
          coeff[,2] <- rownames(coeff)
          colnames(coeff) <- c("value", "model_var")
          
          if (h == 1){
            for (y in seq(along = coeff$model_var)){ ##4
              same <- coeff$model_var[[y]]
              
              if(same == "height_ln.m"){
                coeff_sub <- coeff[coeff$model_var == same, ]
                summary_models[,8][[h]] <- same
              }
            }
          }
          else if (h == 2){
            for (y in seq(along = coeff$model_var)){ ##4
              same <- coeff$model_var[[y]]
              
              if(same == "positionsubcanopy"){
                coeff_sub <- coeff[coeff$model_var == same, ]
                summary_models[,8][[h]] <- same
              }
            }
          }
          else if (h == 3 |h == 6){
            for (y in seq(along = coeff$model_var)){ ##4
              same <- coeff$model_var[[y]]
              
              if(same == "elev.m"){
                coeff_sub <- coeff[coeff$model_var == same, ]
                summary_models[,8][[h]] <- same
              }
            }
          }
          else if (h == 4 |h == 5){
            for (y in seq(along = coeff$model_var)){ ##4
              same <- coeff$model_var[[y]]
              
              if(same == "height_ln.m:elev.m"){
                coeff_sub <- coeff[coeff$model_var == same, ]
                summary_models[,8][[h]] <- same
              }
            }
          }
          else if (h == 7){
            for (y in seq(along = coeff$model_var)){ ##4
              same <- coeff$model_var[[y]]
              
              if(same == "tlp"){
                coeff_sub <- coeff[coeff$model_var == same, ]
                summary_models[,8][[h]] <- same
              }
            }
          }
          else if (h == 8){
            for (y in seq(along = coeff$model_var)){ ##4
              same <- coeff$model_var[[y]]
              
              if(same == "rpring"){
                coeff_sub <- coeff[coeff$model_var == same, ]
                summary_models[,8][[h]] <- same
              }
            }
          }
        }
      }
      
      #update the coefficient value
      summary_models[,7][[h]] <- coeff_sub$value
      
      #update the table. If the sign conventions of the coefficient and the predicted response do not match, assign NA.
        # summary_models[,6][[h]] <- ifelse(
        #   (coeff_sub$value <0 & summary_models$response_predict[[h]] <0) |
        #     (coeff_sub$value >0 & summary_models$response_predict[[h]] >0),
        #   summary_models[,6][[h]], NA)
      
        coeff <- coeff[-1,]
        coeff_max <- coeff[coeff$value == max(coeff$value), ]
        summary_models[,9][[h]] <- coeff_max$model_var
    } 
  }
}

#csv has a 1 in the title to make sure any notes in current file are not overwritten
write.csv(summary_models, "manuscript/results_full_models_combined_years.csv", row.names=FALSE)

##6aii. coefficients ####
best <- lmm_all[[64]]
coef(summary(best))[ , "Estimate"]

lm_new <- lm(resist.value ~ dbh_ln*distance_ln.m, data=trees_all, REML=FALSE)

q <- sapply(lmm_all, anova, simplify=FALSE)
mapply(anova, lmm_all, SIMPLIFY = FALSE)

#subset by only the top result (the minimum AICc value)
aic_top <- var_aic %>%
  filter(AICc == min(AICc))

##6aiii. base code for running multiple models through AICc eval ####
#define response and effects
response <- "resist.value"
effects <- c("position_all", "sap_ratio", "tlp", "rp", "elev.m", "distance.ln.m", "height.ln.m", "year", "(1|sp/tree)")

#create all combinations of random / fixed effects
effects_comb <- 
  unlist( sapply( seq_len(length(effects)), 
                  function(i) {
                    apply( combn(effects,i), 2, function(x) paste(x, collapse = "+"))
                  }))

# pair response with effect and sub out combinations that don't include random effects
#in general, if two variables are >70% correlated, you can toss one of them without significantly affecting the results
var_comb <- expand.grid(response, effects_comb) 
var_comb <- var_comb[grepl("1", var_comb$Var2), ] #only keep in fixed/random combos
var_comb <- var_comb[grepl("year", var_comb$Var2), ] #keep year in for drought sake

# formulas for all combinations. $Var1 is the response, and $Var2 is the effect
# for good stats, you should have no more total parameters than 1/10th the number of observations in your dataset
formula_vec <- sprintf("%s ~ %s", var_comb$Var1, var_comb$Var2)

# create list of model outputs
lmm_all <- lapply(formula_vec, function(x){
  fit1 <- lmer(x, data = trees_all, REML=FALSE, 
               control = lmerControl(optimizer ="Nelder_Mead"))
  return(fit1)
})
names(lmm_all) <- formula_vec

var_aic <- aictab(lmm_all, second.ord=TRUE, sort=TRUE) #rank based on AICc
r <- rsquared(lmm_all) #gives R^2 values for models. "Marginal" is the R^2 for just the fixed effects, "Conditional" is the R^2 for everything.

##6b. determine the best model from anova (using the model candidates above) ####
#interestingly, this gives a similar result to running AICc, with Pr(>Chisq) acting as a kind of p-value for showing which model is best to use.
anova(lmm.nullsp, lmm.nullyear, lmm.random, lmm.positionsp, lmm.positionyear, lmm.full)
      #lmm.nullyear, lmm.random, lmm.positionsp, lmm.positionyear, lmm.full) 
      #lmm.rpsp, lmm.rpyear, lmm.fixedsp, lmm.fixedyear, lmm.combined)

#                   Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# lmm.nullsp        3 2706.2 2724.2 -1350.1   2700.2                             
# lmm.nullyear      3 2775.8 2793.8 -1384.9   2769.8   0.00      0          1    
# lmm.random        4 2654.7 2678.7 -1323.4   2646.7 123.12      1     <2e-16 ***
# lmm.positionsp    4 2706.1 2730.1 -1349.0   2698.1   0.00      0          1    
# lmm.positionyear  4 2767.7 2791.6 -1379.8   2759.7   0.00      0          1    
# lmm.full          5 2653.8 2683.8 -1321.9   2643.8 115.81      1     <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

##6c. Run the best model, changing REML to TRUE ####
lmm.full <- lmer(resist.value ~ position + (1 | sp) + (1 | year), data=trees_all)
summary(lmm.full)
# Fixed effects:
#                   Estimate Std. Error t value
# (Intercept)        0.87991    0.03858  22.809
# positionsubcanopy  0.02534    0.01495   1.695

#here, the model is saying that where the position is canopy (Intercept), the subcanopy will differ by 0.025

# there isn't much purpose to running anova if I already used AICc to determine which model was the best one to run. If I had used crossed anova to determine the best model to run, then I would need to report p-value.
vari_anova <- Anova(lmm.full)

q <- qqp(residuals(lmm.full), "norm", main="resistance_residuals")
print(q)


########################################################################################
#7. interpreting the outcomes ####
library(ggplot2)

##height and canopy position by size class ####
scbi.stem3 <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/scbi.stem3_TEMPORARY.csv"))
scbi.stem3$Tree_ID_Num <- gsub("_.*$", "", scbi.stem3$Tree_ID_Num)
current_ht <- trees_all[!duplicated(trees_all$tree), ]
current_ht$year <- 2018

current_ht$dbh_old.cm <- scbi.stem3$DBHcm[match(current_ht$tree, scbi.stem3$Tree_ID_Num)]
current_ht$dbh_ln.cm <- log(current_ht$dbh_old.cm)
current_ht$sap_ratio <- NA

#linear log-log regression
#the full equation is using all points for which we have data to create the equation, despite that for several species we don't have enough data to get a sp-specific equation
current_ht$height_ln.m <- ifelse(current_ht$sp == "caco", (0.55+0.766*current_ht$dbh_ln.cm),
                        ifelse(current_ht$sp == "cagl", (0.652+0.751*current_ht$dbh_ln.cm),
                        ifelse(current_ht$sp == "caovl", (0.9+0.659*current_ht$dbh_ln.cm),
                        ifelse(current_ht$sp == "cato", (0.879+0.668*current_ht$dbh_ln.cm),
                        ifelse(current_ht$sp == "fagr", (0.513+0.712*current_ht$dbh_ln.cm),
                        ifelse(current_ht$sp == "litu", (1.57+0.488*current_ht$dbh_ln.cm),
                        ifelse(current_ht$sp == "quru", (1.13+0.54*current_ht$dbh_ln.cm),
                                 (1.11+0.573*current_ht$dbh_ln.cm))))))))
current_ht$height.m <- exp(current_ht$height_ln.m)

#power function Height = intercept*(diameter^slope)
current_ht$height_power_ln <- 
                      ifelse(current_ht$sp == "caco", (0.55*(current_ht$dbh_ln.cm^0.766)),
                      ifelse(current_ht$sp == "cagl", (0.652*(current_ht$dbh_ln.cm^0.751)),
                      ifelse(current_ht$sp == "caovl", (0.9*(current_ht$dbh_ln.cm^0.659)),
                      ifelse(current_ht$sp == "cato", (0.879*(current_ht$dbh_ln.cm^0.668)),
                      ifelse(current_ht$sp == "fagr", (0.513*(current_ht$dbh_ln.cm^0.712)),
                      ifelse(current_ht$sp == "litu", (1.57*(current_ht$dbh_ln.cm^0.488)),
                      ifelse(current_ht$sp == "quru", (1.13*(current_ht$dbh_ln.cm^0.54)),
                                   (0.849*(current_ht$dbh_ln.cm^0.659)))))))))
current_ht$height_power.m <- exp(current_ht$height_power_ln)

current_ht <- rbind(current_ht, trees_all)
current_ht <- current_ht[order(current_ht$tree, current_ht$year), ]

pdf("graphs_plots/current_dbh_height_all_years.pdf", width=12)
#with dbh
ggplot(data = current_ht) +
  aes(x = year, y = dbh_old.cm, fill = position_all) +
  # aes(x=position_all, y=dbh_old.cm, fill=year) +
  geom_boxplot() +
  ggtitle("DBH vs crown position")+
  xlab("year") +
  ylab("DBH(cm)") +
  theme_minimal()

#with height
ggplot(data = current_ht) +
  aes(x = year, y = height.m, fill = position_all) +
  # aes(x=position_all, y=height.m, fill=year) +
  geom_boxplot() +
  ggtitle("Height vs crown position")+
  xlab("year") +
  ylab("height(m)") +
  theme_minimal()
dev.off()

#other graphs ####

trees_all <- group_by(trees_all, year, position)

#density graph of resistance value distribution by year by canopy position
ggplot(trees_all, aes(x=resist.value)) +
  geom_density() +
  facet_wrap(year ~ position, ncol=2)

#graph showing resistance value by species by year by canopy position
ggplot(data = trees_all) +
  aes(x = year, y = resist.value, color = sp) +
  geom_point() +
  scale_color_brewer(palette="Paired") +
  theme_minimal() +
  facet_wrap(vars(position))

#graphs looking at results from "best" AICc model (residuals, norm line, etc)
plot(lmm_all[[31]])
resid(lmm_all[[31]])
plot(density(resid(lmm_all[[31]]))) #A density plot
qqnorm(resid(lmm_all[[31]])) # A quantile normal plot - good for checking normality
qqline(resid(lmm_all[[31]]))


#this plot shows regression line for certain variables against resistance values, separated by year and species
ggplot(trees_all, aes(x = tlp, y = resist.value, color=year)) +
  geom_point() +
  #scale_color_manual(values=c("skyblue", "blue", "navy")) + 
  scale_color_distiller(palette = "Spectral") +
  theme_classic() +
  #geom_line(data = cbind(trees_all, pred = predict(lmm_all[[32]])), aes(y = pred)) +
  geom_smooth(method="lm") +
  ylab("(growth during drought) / (growth prior to drought)") +
  xlab("DBH (log-transformed)") +
  facet_wrap(~sp, nrow=4)

#regression line with all data values together
ggplot(trees_all, aes(x = tlp, y = resist.value)) +
  geom_point() +
  theme_classic() +
  geom_smooth(method="lm") +
  ylab("(growth during drought) / (growth prior to drought)") +
  xlab("TLP")


#What this plot does is create a dashed horizontal line representing zero: an average of zero deviation from the best-fit line. It also creates a solid line that represents the residual deviation from the best-fit line.
# If the solid line doesn't cover the dashed line, that would mean the best-fit line does not fit particularly well.
plot(fitted(lmm_all[[32]]), residuals(lmm_all[[32]]), xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, lty=2)
lines(smooth.spline(fitted(lmm_all[[32]]), residuals(lmm_all[[32]])))

#
boxplot(resist.value ~ sp, data=trees_all)

library(plotly)
p <- qqp(residuals(lmm_all[[13]]), "norm")
ggplotly(p)


#count of values>1 for each species, and for each canopy position
#take out 1947, 1911, 1991


