#canopy position analysis from tree cores

#1. full script set-up ####
cru1901 <- read.csv("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/climate_sensitivity_cores/results/canopy_vs_subcanopy/1901_2009/tables/monthly_correlation/correlation_with_CRU_SCBI_1901_2016_climate_data.csv", stringsAsFactors = FALSE)

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
setwd("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/tree-growth-and-traits")
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

pdf("Canopy_subcanopy_correlation.pdf", width=10)
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
#3. mixed effects model ####
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
library(pointRes)
library(dplR)
library(data.table)

##to be clear, I wrote this code before I realized that some of the work done in these loops had already been done in the outputs of res.comp (specifically out.select). However, since the code runs well, and I double-checked that it was giving the same outputs as analyzing out.select, I'm keeping it as is.

##4a. canopy ####
setwd("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/tree_cores/chronologies/current_chronologies/complete/separated by canopy position/canopy_cores")

dirs_can <- dir("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/tree_cores/chronologies/current_chronologies/complete/separated by canopy position/canopy_cores", pattern = "_canopy.rwl")

dirs_can <- dirs_can[dirs_can != "frni_canopy.rwl" & dirs_can != "frni_drop_canopy.rwl" & dirs_can != "caco_drop_canopy.rwl"]

sp_can <- gsub("_drop_canopy.rwl", "", dirs_can)

canopy <- list()
widths_can <- list()
canopy_table <- NULL
for (i in seq(along=dirs_can)){
  for (j in seq(along=sp_can)){
    if (i==j){
      file <- dirs_can[[i]]
      rings <- read.rwl(file) #read in rwl file
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
setwd("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/tree_cores/chronologies/current_chronologies/complete/separated by canopy position/subcanopy_cores")

dirs_subcan <- dir("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/tree_cores/chronologies/current_chronologies/complete/separated by canopy position/subcanopy_cores", pattern = "_subcanopy.rwl")

#dirs_subcan <- dirs_subcan[dirs_subcan != "frni_drop_subcanopy.rwl" & dirs_subcan != "caco_drop_subcanopy.rwl"]

sp_subcan <- gsub("_drop_subcanopy.rwl", "", dirs_subcan)

subcanopy <- list()
widths_sub <- list()
subcanopy_table <- NULL
for (i in seq(along=dirs_subcan)){
  for (j in seq(along=sp_subcan)){
    if (i==j){
      file <- dirs_subcan[[i]]
      rings <- read.rwl(file) #read in rwl file
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

widths <- c(widths_can, widths_sub) #combine into one, then delete. For use in #5d
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
years_bysp <- pointers[pointers$year %in% c(1966, 1977, 1999), ]
years_bysp <- years_bysp[, c(1,13,14,2:12)]
years_bysp <- years_bysp[order(years_bysp$year, years_bysp$sp), ]

setwd("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/McGregor_climate-sensitivity-variation")
#write.csv(pointers, "occurrence_of_pointer_yrs.csv", row.names=FALSE)

##4d. resistance metrics for all trees ####
neil_list <- read.csv("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/McGregor_climate-sensitivity-variation/core_list_for_neil.csv", stringsAsFactors = FALSE)

neil_list$tag <- paste0("X", neil_list$tag) #to match the colnames of can_resist below

# pointer_years <- head(years_point$yr) #from above in #4c
# pointer_years <- pointer_years[!pointer_years %in% c(1911, 1947, 1991)]
pointer_years <- c(1964, 1966, 1977, 1999)

###canopy ####
#this loop says, for the different species in the list "canopy" (names(canopy)), create a dataframe of only the resistance index. Make a list of the colnames, which are the individual trees. Then, assign species identifiers for each one from Neil's core list, subset by the defined pointer years, and melt the data before rbinding.

tag_n <- names(canopy)
trees_canopy <- NULL
for (i in seq(along=1:length(tag_n))){
  can_resist <- data.frame(canopy[[i]]$resist)
  years <- rownames(can_resist)
  colnames(can_resist) <- gsub("A", "", colnames(can_resist))
  tree_series <- colnames(can_resist)
  
  # for (j in seq(along=tree_series)){
  #  trees <- tree_series[[j]]
  ind <- can_resist
  ind_neil <- neil_list[neil_list$tag %in% tree_series, ]
  
  #  colnames(ind) <- trees
  ind$year <- years
  ind$sp <- unique(ind_neil$sp)
  ind$position <- "canopy"
  
  ind <- ind[ind$year %in% pointer_years, ]
  
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
 
  # for (j in seq(along=tree_series)){
  #  trees <- tree_series[[j]]
    ind <- sub_resist
    ind_neil <- neil_list[neil_list$tag %in% tree_series, ]
    
  #  colnames(ind) <- trees
    ind$year <- years
    ind$sp <- unique(ind_neil$sp)
    ind$position <- "subcanopy"
    
    ind <- ind[ind$year %in% pointer_years, ]
    
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
library(tools)
library(dplyr)

setwd("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/tree_cores/chronologies/current_chronologies/complete/separated by canopy position/canopy_cores")

dirs_can <- dir("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/tree_cores/chronologies/current_chronologies/complete/separated by canopy position/canopy_cores", pattern = "_canopy.rwl")

dirs_can <- dirs_can[dirs_can != "frni_canopy.rwl" & dirs_can != "frni_drop_canopy.rwl" & dirs_can != "caco_drop_canopy.rwl"]

sp_can <- gsub("_drop_canopy.rwl", "", dirs_can)

bai_table_can <- NULL
for (i in seq(along=dirs_can)){
  for (j in seq(along=sp_can)){
    if (i==j){
      file <- dirs_can[[i]]
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

setwd("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/tree_cores/chronologies/current_chronologies/complete/separated by canopy position/subcanopy_cores")

dirs_subcan <- dir("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/tree_cores/chronologies/current_chronologies/complete/separated by canopy position/subcanopy_cores", pattern = "_subcanopy.rwl")

#dirs_subcan <- dirs_subcan[dirs_subcan != "frni_drop_subcanopy.rwl" & dirs_subcan != "caco_drop_subcanopy.rwl"]

sp_subcan <- gsub("_drop_subcanopy.rwl", "", dirs_subcan)

bai_table_sub <- NULL
for (i in seq(along=dirs_subcan)){
  for (j in seq(along=sp_subcan)){
    if (i==j){
      file <- dirs_subcan[[i]]
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
plot_ly(q, x=q$year, y=q$resid, type="scatter", mode="lines")

pdsi <- read.csv("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/McGregor_climate-sensitivity-variation/pdsi_value_comparison.csv")

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
plot_ly(pdsi_true, x=pdsi_true$year, y=pdsi_true$resid, type="scatter", mode="line")

##########################################################################################
#5. add in climate and growth variables ####
library(SciViews)

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
elev <- read.csv("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/spatial_data/elevation/full_stem_elevation_2013.csv")

trees_all$elev_m <- elev$dem_sigeo[match(trees_all$tree, elev$tag)]

##5d. add in distance to water ####
setwd("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/Dendrobands/resources/maps")

neil_map <- neil_list
neil_map$tag <- gsub("X", "", neil_map$tag)
neil_map$tag <- as.numeric(neil_map$tag)

dendro_trees <- read.csv("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/Dendrobands/data/dendro_trees.csv")

##this should be fixed when 131352 is found with 2018 data!!!!

library(ggplot2)
library(rgdal)
library(broom) #for the tidy function
library(sf) #for mapping
library(ggthemes) #for removing graticules when making pdf

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

map <- ggplot() +
  geom_path(data = scbi_plot_df, aes(x = long, y = lat, group = group))+
  geom_path(data=roads_df, aes(x=long, y=lat, group=group), 
            color="#996600", linetype=2)+
  geom_path(data=streams_df, aes(x=long, y=lat, group=group), color="blue")+
  geom_path(data=deer_df, aes(x=long, y=lat, group=group), size=1.1)+
  geom_point(data=neil_list, aes(x=NAD83_X, y=NAD83_Y), shape=19)+
  geom_text(data=neil_list, aes(x=NAD83_X, y=NAD83_Y, label=tag), 
            size=3, hjust=1.25, nudge_y=-1, nudge_x=1, check_overlap=TRUE)+
  theme(plot.title=element_text(vjust=0.1))+
  coord_sf(crs = "crs = +proj=merc", xlim=c(747350,747800), ylim=c(4308500, 4309125))

library(geosphere)
streams_dd <- spTransform(streams, CRS("+proj=longlat +datum=WGS84"))
streams_dd_df <- tidy(streams_dd)

neil_map_sub <- neil_map[, c(25:26)]
streams_sub <- streams_dd_df[,c(1:2)]
dist_water <- data.frame(dist2Line(neil_map_sub, streams_sub))



##5e. add in dbh in each year 1999 ####
dbh <- trees_all[, c(1:4)]
dbh$dbh2013 <- elev$dbh[match(dbh$tree, elev$tag)]

#create df with bark thickness log values and intercept values from Krista's paper (supplemental info)
#https://besjournals.onlinelibrary.wiley.com/doi/epdf/10.1111/1365-2435.12470 
#fagr does not have bark thickness measured because it is negligible
bark <- data.frame(
  "sp" = c("acru", "fagr", "litu", "nysy", "caco", "cagl", "caovl", "cato", "fram", "juni", "qual", "qupr", "quru", "quve", "ulru"), 
  "bark_thick" = c(-2.564, 0, -0.659, -0.611, -1.917, -0.495, -2.504, -0.945, 0.318, -0.293, -1.231, -0.647, -0.789, 1.5, 1.133),
  "intercept" = c(0.599, 0, 0.425, 0.413, 0.503, 0.316, 0.703, 0.396, 0.295, 0.385, 0.526, 0.423, 0.341, 0.053, -0.057))

dbh$bark_thick <- bark$bark_thick[match(dbh$sp, bark$sp)]
dbh$intercept <- bark$intercept[match(dbh$sp, bark$sp)]

#the main equation is based on ring widths. We have determined the equation to be
# rw(pointer_year) <- 0.5*dbh2013 - bark_thick*(dbh2013^intercept) - sum(rw(pointer_year):rw(end)). The first part of the equation is here. Summing the pointer years happens with the "q" df below in the loop.
dbh$rw_prelim <- (0.5*dbh$dbh2013) - (dbh$bark_thick*(dbh$dbh2013^dbh$intercept))


dbh$dbh_old <- "0" #in prep for below
dbh$dbh_old <- as.numeric(dbh$dbh_old)

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
        #the output of this loop is 3 separate columns for each year's old dbh, hence why it is set to q as a dataframe before being combined below
        q <- data.frame(sapply(pointer_years, function(x){
          rw <- df[rownames(df)>=x, ]
          ifelse(dbh$year == x & dbh$tree == ring_ind, 
                 dbh$rw_prelim - sum(rw[, ring_col], na.rm=TRUE), 0)
        }))
        
        # q$dbh_old <- q[,1] +q[,2] + q[,3] #add columns together
        q$dbh_old <- q[,1] +q[,2] + q[,3] + q[,4]
        dbh$dbh_old <- dbh$dbh_old + q$dbh_old #combine with dbh
      }
    }
  }
}

# check <- dbh[dbh$dbh_old == 0, ] #check if any tree was missed

trees_all$dbh_old <- dbh$dbh_old
trees_all$dbh_old <- ifelse(trees_all$dbh_old < 0, 0, trees_all$dbh_old)
trees_all$dbh_ln <- ifelse(trees_all$dbh_old == 0, NA, ln(trees_all$dbh_old))

##5f. remove all NAs ####
trees_all <- trees_all[complete.cases(trees_all), ]
##5g. remove resistance values >2 ####
trees_all <- trees_all[trees_all$resist.value <=2,]
##5h. subset to only include certain years ####
x1964 <- trees_all[trees_all$year == 1964, ]
x1966 <- trees_all[trees_all$year == 1966, ]
x1977 <- trees_all[trees_all$year == 1977, ]
x1999 <- trees_all[trees_all$year == 1999, ]

##############################################################################################
#6. mixed effects model for output of #5. ####
library(lme4)
library(AICcmodavg) #aictab function
library(car)
library(piecewiseSEM) #for R^2 values for all model outputs in a list
library(MuMIn) #for R^2 values of one model output

##6a. Determine best model to use with AICc ####
#define response and effects
response <- "resist.value"
effects <- c("dbh_ln", "rp", "(1|sp/tree)")
# effects <- c("position", "tlp", "rp", "elev_m", "dbh_ln", "year", "(1 | sp)")

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
# var_comb <- var_comb[grepl("year", var_comb$Var2), ] #keep year in for drought sake

# formulas for all combinations. $Var1 is the response, and $Var2 is the effect
# for good stats, you should have no more total parameters than 1/10th the number of observations in your dataset
formula_vec <- sprintf("%s ~ %s", var_comb$Var1, var_comb$Var2)

# create list of model outputs
lmm_all <- lapply(formula_vec, function(x){
  fit1 <- lmer(x, data = trees_all, REML=FALSE)
  #fit1$coefficients <- coef( summary(fit1))
  return(fit1)
})
names(lmm_all) <- formula_vec

lm_new <- lm(resist.value ~ dbh_ln + rp + year, data=trees_all, REML=FALSE)

var_aic <- aictab(lmm_all, second.ord=TRUE, sort=TRUE) #rank based on AICc
r <- rsquared(lmm_all) #gives R^2 values for models. "Marginal" is the R^2 for just the fixed effects, "Conditional" is the R^2 for everything.

best <- lmm_all[[2]]
coef(summary(best))[ , "Estimate"]

q <- sapply(lmm_all, anova, simplify=FALSE)
mapply(anova, lmm_all, SIMPLIFY = FALSE)

#subset by only the top result (the minimum AICc value)
aic_top <- var_aic %>%
  filter(AICc == min(AICc))

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


