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

#Note about FRNI and CACO
#originally, getting the pointer years was done without caco because of too few cores. Now that we have the pointer years, we're adding them back in for the full analysis.
#we thought about including frni (combining the canopy and subcanopy cores because only 1 canopy), but we excluded for two reasons
##1. including frni doesn't impact the model much, and
##2. frni is the 19th most productive species, whereas every other species we have is the top 12.

#Note about PIST
##we originally included pist but ultimately we have no leaf trait data for this species, so we excluded it

##4a. determine pointer years all cores grouped together ####
## ONLY DO THIS all cores together ####
#this is a combination of the two canopy and subcanopy groupings below

rings <- read.rwl("data/core_files/all_species_except_FRNI_PIST.rwl") #read in rwl file
widths <- rings #for consistency with original code
area <- bai.in(rings) #convert to bai.in
resil_metrics <- res.comp(area, nb.yrs=5, res.thresh.neg = 30, series.thresh = 25) #get resilience metrics

resil_pointers <- data.frame(resil_metrics$out)
resil_pointers <- resil_pointers[resil_pointers$nb.series >=4, ]

pointers <- resil_pointers[resil_pointers$nature == -1, ]
pointers <- pointers[,c(1:3,5)]
pointers <- pointers[order(pointers$nb.series, decreasing=TRUE), ]
rownames(pointers) <- 1:nrow(pointers)

#get specific resist values for all trees
neil_list <- read.csv("data/core_list_for_neil.csv", stringsAsFactors = FALSE)
neil_sp <- unique(neil_list$sp)

neil_list$tag <- paste0("X", neil_list$tag) #to match the colnames of can_resist below

pointer_years_simple <- c(1966, 1977, 1999)

resist <- data.frame(resil_metrics$resist)
years <- rownames(resist)
colnames(resist) <- gsub("A", "", colnames(resist))
tree_series <- colnames(resist)

ind <- resist
ind_neil <- neil_list[neil_list$tag %in% tree_series, ]
ind$year <- years
ind$year <- as.numeric(ind$year)
ind <- ind[ind$year %in% c(1966,1977,1999), ]

ind$year <- as.character(ind$year) #to melt
change <- melt(ind)
setnames(change, old=c("variable", "value"), new=c("tree", "resist.value"))


#bring in species from neil_list
change$sp <- ind_neil$sp[match(change$tree, ind_neil$tag)]

change$tree <- gsub("X", "", change$tree)
change$tree <- gsub("^0", "", change$tree)

#this is trees_all
trees_all <- change[complete.cases(change), ]

#

##4ai. determine pointer years by canopy/subcanopy groupings ####
### this (4ai and 4b) was the original method for this research. We have since decided to not have these groupings, thus the code is here for reference
### canopy ####
dirs_can <- dir("data/core_files/canopy_cores", pattern = "_canopy.rwl")
dirs_can <- dirs_can[!dirs_can %in% c("pist_drop_canopy.rwl", "frni_drop_canopy.rwl")]

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
      
      if(sp_can[[j]] != "caco" & sp_can[[j]] != "frni"){
        testr_table <- data.frame(testr$out)
        testr_table <- testr_table[testr_table$nb.series > 4, ] #remove where there are < 4 series
        testr_table$sp <- sp_can[[j]]
        testr_table$position <- "canopy"
        
        canopy_table <- rbind(canopy_table, testr_table)
      }
    }
  }
}
values <- paste0(sp_can, "_can_res")
names(canopy) <- values
values <- paste0(sp_can, "_canopy")
names(widths_can) <- values

###subcanopy ####
dirs_subcan <- dir("data/core_files/subcanopy_cores", pattern = "_subcanopy.rwl")
dirs_subcan <- dirs_subcan[!dirs_subcan %in% c("pist_drop_subcanopy.rwl", "frni_drop_subcanopy.rwl")]

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
names(all_bai_subcan) <- values

widths <- c(widths_can, widths_sub) #combine into one, then delete individual. For use in #5d
widths_can <- NULL
widths_subcan <- NULL

### df for pointer years of all trees combined ####
full_ind <- rbind(canopy_table, subcanopy_table) #full table of indices for canopy and subcanopy cores

pointers <- full_ind[full_ind$nature == -1, ]

years_point <- count(pointers, vars=year) #counts the occurrences of each unique year
colnames(years_point) <- c("yr", "n.pointer")
years_point <- years_point[order(years_point$n.pointer, decreasing=TRUE), ]

#top drought years by species and canopy position
years_bysp <- pointers[pointers$year %in% c(1964, 1965, 1966, 1977, 1999), ]
years_bysp <- years_bysp[, c(1,13,14,2:12)]
years_bysp <- years_bysp[order(years_bysp$year, years_bysp$sp), ]

#write.csv(pointers, "data/occurrence_of_pointer_yrs.csv", row.names=FALSE)

##4b. resistance metrics for all trees ####
neil_list <- read.csv("data/core_list_for_neil.csv", stringsAsFactors = FALSE)
neil_sp <- unique(neil_list$sp)

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
  
  ##for original code here, see below in the subcanopy loop
  
  ind$year <- years
  ind$year <- as.numeric(ind$year)
  ind$sp <- unique(ind_neil$sp)
  ind$position <- "canopy"

  ind <- ind[ind$year %in% c(1959:1963,1964:1966,1977,1999), ]
  
  ## these lines of code get the mean of 1959:163 for pre-drought, and then do the mean of the drought growth itself (1964:1966). Then you get the resistance value by doing:
  ###[average growth 1964-66] / [ average growth 1959 - 1963]
  ## since it was a multi-year drought. We're calling it 1966 for simplicity.
  df <- ind[, 1:(ncol(ind) -3)]
  df_pre <- df[rownames(df) %in% c(1959:1963), ]
  df_dro <- df[rownames(df) %in% c(1964:1966), ]
  
  df_pre <- as.data.frame(t(data.frame(colMeans(df_pre))))
  colnames(df_pre) <- colnames(df)
  
  df_dro <- as.data.frame(t(data.frame(colMeans(df_dro))))
  colnames(df_dro) <- colnames(df)
  
  df_resist <- df_dro/df_pre
  df_resist <- round(df_resist, 2)
  
  rownames(df_resist) <- 1966
  df_resist$year <- 1966
  df_resist$sp <- unique(ind$sp)
  df_resist$position <- unique(ind$position)
  
  ind <- ind[-c(1:8), ]
  ind <- rbind(df_resist, ind)
  ind$year <- as.character(ind$year)
  
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
    
    ## this code was the original we used to make the averages. we've since changed it to include the ratio of 1964:1966 to 1959:1963, but I'm keeping it here just in case.
    # ind$year <- years
    # ind$sp <- unique(ind_neil$sp)
    # ind$position <- "subcanopy"
    # 
    # ind <- ind[ind$year %in% pointer_years, ]
    # 
    # ## these three lines of code are for taking the mean of 1964-1966, 
    # ## since it was a multi-year drought. We're calling it 1966 for simplicity.
    # ind["1966", 1:(ncol(ind) -3)] <- colMeans(ind[c(1:3), 1:(ncol(ind) -3)])
    # ind <- ind[-c(1,2), ]
    # ind[, 1:(ncol(ind) -3)] <- round(ind[, 1:(ncol(ind) -3)], 2)
    
    ind$year <- years
    ind$year <- as.numeric(ind$year)
    ind$sp <- unique(ind_neil$sp)
    ind$position <- "canopy"
    
    ind <- ind[ind$year %in% c(1959:1963,1964:1966,1977,1999), ]
    
    ## these lines of code get the mean of 1959:163 for pre-drought, and then do the mean of the drought growth itself (1964:1966). Then you get the resistance value by doing:
    ###[average growth 1964-66] / [ average growth 1959 - 1963]
    ## since it was a multi-year drought. We're calling it 1966 for simplicity.
    df <- ind[, 1:(ncol(ind) -3)]
    df_pre <- df[rownames(df) %in% c(1959:1963), ]
    df_dro <- df[rownames(df) %in% c(1964:1966), ]
    
    df_pre <- as.data.frame(t(data.frame(colMeans(df_pre))))
    colnames(df_pre) <- colnames(df)
    
    df_dro <- as.data.frame(t(data.frame(colMeans(df_dro))))
    colnames(df_dro) <- colnames(df)
    
    df_resist <- df_dro/df_pre
    df_resist <- round(df_resist, 2)
    
    rownames(df_resist) <- 1966
    df_resist$year <- 1966
    df_resist$sp <- unique(ind$sp)
    df_resist$position <- unique(ind$position)
    
    ind <- ind[-c(1:8), ]
    ind <- rbind(df_resist, ind)
    ind$year <- as.character(ind$year)
    
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

##4c. determine proportion of resistance values per sp ####
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



##4d. comparing residuals of PDSI values and BAI of all trees ####
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

##4e. Statistics for paper ####

#these numbers are
#1. subset for year
#2. proportion of all trees that show growth deficit meeting the threshold we defined
#3. proportion of species-canopy classes that have >50% trees meeting growth deficit threshold

sum_table <- data.frame("year" = c(1966, 1977, 1999), 
                        "prop_growth" = NA, 
                        "prop_pointer" = NA)

for (i in seq(along=sum_table$year)){
  full_ind_sub <- full_ind[full_ind$year %in% sum_table$year[[i]],  ]
  
  sum_table$prop_growth[[i]] <- (sum(full_ind_sub$nb.series*(full_ind_sub$perc.neg/100))/sum(full_ind_sub$nb.series))*100
  
  sum_table$prop_growth[[i]] <- round(sum_table$prop_growth[[i]], 2)
  
  sum_table$prop_pointer[[i]] <- (nrow(full_ind_sub[full_ind_sub$nature == -1, ])/nrow(full_ind_sub))*100
  
  sum_table$prop_pointer[[i]] <- round(sum_table$prop_pointer[[i]], 2)
}

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
library(readxl)
library(raster) #for TWI
library(elevatr) #for TWI
library(dynatopmodel) #for TWI
library(stringi) #for heights

##5a. add in ring porosity qualifications ####
ring_porosity <- data.frame("sp" = c("cagl",  "caovl", "cato", "fagr", "fram", "juni",  "litu",  "pist",  "qual",  "qupr",  "quru",  "quve", "caco", "frni"), "rp" = c("ring", "ring", "ring", "diffuse", "ring", "diffuse", "diffuse", NA, "ring", "ring", "ring", "ring", "ring", "ring"))

trees_all$rp <- ring_porosity$rp[match(trees_all$sp, ring_porosity$sp)]

#gives count of each rp value
rp_test <- trees_all[!duplicated(trees_all$tree), ]
rp_test$tree <- as.numeric(rp_test$tree)
ggplot(data = rp_test) +
  aes(x = rp) +
  geom_bar(fill = "#0c4c8a") +
  theme_minimal() +
  facet_wrap(vars(position))


##5b. add in leaf traits ####
#this comes from the hydraulic traits repo, "SCBI_all_traits_table_species_level.csv"
##leaf traits gained from this include PLA_dry_percent, LMA_g_per_m2, Chl_m2_per_g, and WD [wood density]

leaf_traits <- read.csv(text=getURL("https://raw.githubusercontent.com/EcoClimLab/HydraulicTraits/master/data/SCBI/processed_trait_data/SCBI_all_traits_table_species_level.csv?token=AJNRBEMLSA45Z6X45SIMTNK5ZLAYC"), stringsAsFactors = FALSE)

leaf_traits <- leaf_traits[, c(1,8,12,26,28)]

for (i in seq(along=2:ncol(leaf_traits))){
  trait <- colnames(leaf_traits[2:ncol(leaf_traits)])
  trees_all[, trait[[i]]] <- leaf_traits[, trait[[i]]][match(trees_all$sp, leaf_traits$sp)]
}

##5bi. add in SLA data (here for reference) ####
### I initially was going to include SLA but Krista mentioned that SLA is the inverse of LMA, so for the purposes of this modelling, they're equal. We're focused more on having SCBI-specific data when possible, so we're using LMA.
# traits_sla <- read_excel("data/traits/photosynthesis_traits.xlsx", sheet = "Data")
# traits_sla$species <- paste(traits_sla$Genus, traits_sla$Species)
# SLA <- traits_sla
# SLA$Genus.spp <- paste0(gsub("^(..).*", "\\1", SLA$Genus.spp), 
#                         gsub("^.* (..).*", "\\1", SLA$Genus.spp))
# SLA$Genus.spp <- tolower(SLA$Genus.spp)
# SLA <- SLA[SLA$Genus %in% c("Carya", "Fraxinus", "Fagus", "Juglans", "Liriodendron", "Pinus", "Quercus"), ]
# SLA <- SLA[SLA$Genus.spp %in% neil_sp & !is.na(SLA$SLA), ]
# SLA <- SLA[order(SLA$Genus.spp), ]
# SLA <- SLA[c(10,20,28)]
# unique(SLA$Genus.spp)
# 
# mean_SLA <- 
#   group_by(SLA, Genus.spp) %>%
#   summarize(SLA_mean = mean(SLA))
# 
# trees_all$SLA_mean <- mean_SLA$SLA_mean[match(trees_all$sp, mean_SLA$Genus.spp)]

##5bii. add in p50 and p88 (here for reference) ####
#after review, we have decided to not focus on p50 and p80
# #get P50 from traits table
# hydra <- read.csv(text=getURL("https://raw.githubusercontent.com/EcoClimLab/HydraulicTraits/master/results/SCBI_best_fits.csv?token=AJNRBEP62SALMQHAV45TP2S5HCCPK"))
# 
# #Anderegg 2018 found that p50 and p80 came out significant in modelling
# trees_all$p50.MPa <- hydra$psi_0.5_kl50[match(trees_all$sp, hydra$data.type)]
# trees_all$p80.MPa <- hydra$psi_0.5_kl80[match(trees_all$sp, hydra$data.type)]

#after review, we have decided to not focus on pmin or HSM
# trees_all$Pmin.MPa <- 1.1122*trees_all$mean_TLP_Mpa + 0.3849
# trees_all$hsm.MPa <- trees_all$Pmin.MPa - trees_all$p50.MPa

# meh <- trees_all[!duplicated(trees_all$sp), ]
# meh <- meh[, c("sp", "mean_TLP_Mpa", "Pmin.MPa")]
# meh$psi_Kmax_0.5 <- hydra$psi_Kmax_0.5[match(meh$sp, hydra$data.type)]
#
##5c. add in elevation data ####
elev <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/spatial_data/elevation/full_stem_elevation_2013.csv"))

trees_all$elev.m <- elev$dem_sigeo[match(trees_all$tree, elev$tag)]

##5d. add in distance to water (here for reference) ####
# ## mapping code here is taken from survey_maps.R in Dendrobands Rscripts folder.
# 
# ## I have not found a way to make this not involve personal directories without moving all the data to my folder, which I'm hesitant about doing due to data redundancy.
# scbi_plot <- readOGR("D:/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/spatial_data/shapefiles/20m_grid.shp")
# deer <- readOGR("D:/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/spatial_data/shapefiles/deer_exclosure_2011.shp")
# roads <- readOGR("D:/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/spatial_data/shapefiles/SCBI_roads_edits.shp")
# streams <- readOGR("D:/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/spatial_data/shapefiles/SCBI_streams_edits.shp")
# NS_divide <- readOGR("D:/Dropbox (Smithsonian)/Github_Ian/Dendrobands/resources/maps/shapefiles/NS_divide1.shp")
# 
# #convert all shp to dataframe so that it can be used by ggplot
# #if tidy isn't working, can also do: xxx_df <- as(xxx, "data.frame")
# scbi_plot_df <- tidy(scbi_plot)
# deer_df <- tidy(deer)
# roads_df <- tidy(roads)
# streams_df <- tidy(streams)
# NS_divide_df <- tidy(NS_divide)
# 
# ## now we get into code specific for this analysis
# neil_map <- neil_list
# neil_map$tag <- gsub("X", "", neil_map$tag)
# neil_map$tag <- as.numeric(neil_map$tag)
# neil_map <- neil_map[, c(1:6,23:24)]
# 
# map <- ggplot() +
#   geom_path(data = scbi_plot_df, aes(x = long, y = lat, group = group))+
#   geom_path(data=roads_df, aes(x=long, y=lat, group=group), 
#             color="#996600", linetype=2)+
#   geom_path(data=streams_df, aes(x=long, y=lat, group=group), color="blue")+
#   geom_path(data=deer_df, aes(x=long, y=lat, group=group), size=1.1)+
#   geom_point(data=neil_map, aes(x=NAD83_X, y=NAD83_Y), shape=19)+
#   geom_text(data=neil_map, aes(x=NAD83_X, y=NAD83_Y, label=tag), 
#             size=3, hjust=1.25, nudge_y=-1, nudge_x=1, check_overlap=TRUE)+
#   theme(plot.title=element_text(vjust=0.1))+
#   coord_sf(crs = "crs = +proj=merc", xlim=c(747350,747800), ylim=c(4308500, 4309125))
# 
# ## calculating the distance requires some conversion. First, the points of the cored trees from neil_map must be in their own dataframe before they can be converted to a SpatialPoints object.
# neil_map_sub <- neil_map[, c(7:8)]
# neil_points <- SpatialPoints(neil_map_sub, proj4string = CRS(as.character("+proj=merc")))
# 
# ## here, the minimum distance to water is calculated before binding with neil_map.
# ## A warning says that neil_points and streams are projected differently, but the output has been verified to be accurate.
# distance_water <- data.frame(apply(gDistance(neil_points, streams, byid=TRUE), 2, min))
# colnames(distance_water) <- "distance_water"
# distance <- cbind(neil_map, distance_water)
# 
# ## next, do a log transformation on the distances before adding as a column to trees_all (similar to the dbh calculations below)
# distance$distance.ln.m <- log(distance$distance_water)
# trees_all$distance.ln.m <- distance$distance.ln.m[match(trees_all$tree, distance$tag)]
# 
# ## this is to double check the accuracy of the map.
# distance_short <- distance[distance$distance_water <= 30, ]
# 
# map <- ggplot() +
#   geom_path(data = scbi_plot_df, aes(x = long, y = lat, group = group))+
#   geom_path(data=roads_df, aes(x=long, y=lat, group=group), 
#             color="#996600", linetype=2)+
#   geom_path(data=streams_df, aes(x=long, y=lat, group=group), color="blue")+
#   geom_path(data=deer_df, aes(x=long, y=lat, group=group), size=1.1)+
#   geom_point(data=distance_short, aes(x=NAD83_X, y=NAD83_Y), shape=19)+
#   geom_text(data=distance_short, aes(x=NAD83_X, y=NAD83_Y, label=tag), 
#             size=3, hjust=1.25, nudge_y=-1, nudge_x=1, check_overlap=TRUE)+
#   theme(plot.title=element_text(vjust=0.1))+
#   coord_sf(crs = "crs = +proj=merc", xlim=c(747350,747800), ylim=c(4308500, 4309125))
# 

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
bark <- read.csv("data/traits/SCBI_bark_depth.csv")
# bark <- bark[bark$species %in% sp_can | bark$species %in% sp_subcan, ]
bark <- bark[bark$species %in% unique(trees_all$sp), ]

#1. Calculate diameter_nobark for 2008 = DBH.mm.2008-2*bark.depth.mm
bark$diam_nobark_2008.mm <- bark$DBH.mm.2008 - 2*bark$bark.depth.mm 

#2. log-transform both diam_nobark_2008 (x) and bark.depth.mm (y)
#3. Fit a linear model, and use model to predict log(bark.depth.mm)
source_gist("524eade46135f6348140")
ggplot(data = bark, aes(x = log(diam_nobark_2008.mm), y = log(bark.depth.mm))) +
  stat_smooth_func(geom="text",method="lm",hjust=0.16, vjust=-0.5,parse=TRUE) +
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
##these equations are also found in tableS1_bark_regression.csv
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
scbi.stem1 <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/scbi.stem1.csv"), stringsAsFactors = FALSE)
scbi.stem1$dbh <- as.numeric(scbi.stem1$dbh)
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

df <- rings #the original file read-in from read.rwl
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


##code for original canopy/subcanopy groupings using list made in #4a.
# dbh$diam_nobark_old.mm <- 0
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
#                  dbh$dbh2008.mm - 2*(dbh$mean_bark_2008.mm) - sum(rw[rownames(rw) %in% c(x:2008), ring_col], na.rm=TRUE), 0)
#         }))
#         
#         q$diam_nobark_old.mm <- q[,1] +q[,2] + q[,3] #add columns together
#         # q$dbh_old.mm <- q[,1] +q[,2] + q[,3] + q[,4]
#         dbh$diam_nobark_old.mm <- dbh$diam_nobark_old.mm + q$diam_nobark_old.mm #combine with dbh (it's the same order of rows) #mm
#       }
#     }
#   }
# }
    
#7. Calculate bark thickness using regression equation per appropriate sp
## log(bark.depth.1999) = intercept + log(diam_nobark)*constant
## bark.depth.1999 = exp(log(bark.depth.1999))

#the full equation at the bottom is the regression equation for all these species put together. "fagr" is given a bark thickness of 0 because it is negligble
#these equations are the same as above in #3 of this code section
dbh$bark_thick_old.ln.mm <- NA
dbh$bark_thick_old.ln.mm <- 
  ifelse(dbh$sp == "caco", -1.56+0.416*log(dbh$diam_nobark_old.mm),
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
                      0)))))))))))

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
sap <- read.csv("data/traits/SCBI_Sapwood_Data.csv", stringsAsFactors = FALSE)
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

sap <- sap[sap$sp %in% unique(trees_all$sp), ]
# sap <- sap[sap$sp %in% sp_can | sap$sp %in% sp_subcan, ]

#ratio = sapwood area:total wood area (without bark)
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
height_regr <- read.csv("manuscript/tables_figures/tableS2_height_regression.csv", stringsAsFactors = FALSE)

trees_all$height.ln.m <- NA
for(w in seq(along=height_regr$sp)){
  sp_foc <- height_regr$sp[[w]]
  ht_eq <- height_regr[height_regr$sp == sp_foc, ]
  num <- gsub("\\*x", "", ht_eq$Equations)
  num1 <- as.numeric(stri_extract_first_regex(num, "[[:digit:]].[[:digit:]]+"))
  num2 <- as.numeric(stri_extract_last_regex(num, "[[:digit:]].[[:digit:]]+"))
  
  if(sp_foc %in% trees_all[,"sp"]){
    trees_all$height.ln.m <- 
      ifelse(trees_all$sp == sp_foc,
             num1 + num2*trees_all$dbh.ln.cm,
             trees_all$height.ln.m)
  } else {
    trees_all$height.ln.m <- 
      ifelse(trees_all$sp != sp_foc,
             num1 + num2*trees_all$dbh.ln.cm,
             trees_all$height.ln.m)
  }
  
}

trees_all$height.m <- exp(trees_all$height.ln.m) #m, because these equations come from a plotting of log(DBH in cm) against log(height in m).

#see mean of height changes
ht_change <- trees_all %>%
  group_by(tree) %>%
  summarize(max(height.m) - min(height.m))

ht_change <- ht_change[ht_change$`max(height.m) - min(height.m)` > 0, ]
mean(ht_change$`max(height.m) - min(height.m)`, na.rm=TRUE)

#cap values at max for different species.
# heights_full <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_dimensions/tree_heights/SCBI_tree_heights.csv"), stringsAsFactors = FALSE)
# 
# max_ht <- aggregate(height.m ~ sp, data=heights_full, FUN=max)

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

##5i. add in topographic wetness index ####
### this code comes from topo_wetness_index in ForestGEO-Data
ext <- extent(747370.6, 747785.8, 4308505.5, 4309154.8)
xy <- abs(apply(as.matrix(bbox(ext)), 1, diff))
n <- 5
r <- raster(ext, ncol=xy[1]*n, nrow=xy[2]*n)
proj4string(r) <- CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

#2 Get elevation raster from online
q <- get_elev_raster(r, z=14)

#3 Crop online raster to the dimensions of the empty raster, set resolution to 5m
r <- raster(ext, res = 5)
q <- resample(q, r)
res(q)
proj4string(q) <- CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0") #q lost its crs in the resample function
plot(q)

#4 Get hydrological features of landscape (upslope area and topographical wetness index)
layers <- build_layers(q)
sp::plot(layers, main=c("Elevation AMSL (m)", "Upslope area (log(m^2/m))", "TWI ((log(m^2/m))"))

#5 get TWI values for trees
twi_trees <- read.csv("data/core_list_for_neil.csv")
twi_trees <- twi_trees[, c(1,23:24)]
twi_trees1 <- twi_trees[, c(2:3)]
twi <- extract(layers[[3]], twi_trees1, method="simple")
twi_trees$TWI <- twi

#6 add to trees_all
trees_all$TWI <- twi_trees$TWI[match(trees_all$tree, twi_trees$tag)]
trees_all$TWI.ln <- log(trees_all$TWI)

##5j. remove one bad tree & resistance values >2 ####
##fram 140939 has been mislabeled. It is recorded as having a small dbh when that is the second stem. In terms of canopy position, though, it fell between time of coring and when positions were recorded, thus we do not know its position.
trees_all <- trees_all[!trees_all$tree == 140939, ]
trees_all <- trees_all[trees_all$resist.value <=2,]
##5j1. write trees_all to csv for use in manuscript code ####
write.csv(trees_all, "manuscript/tables_figures/trees_all.csv", row.names=FALSE)
##5k. subset for either leaf hydraulic traits or biophysical ####
trees_all_traits <- trees_all[complete.cases(trees_all), ]

##take out columns that are unnecessary for model runs
trees_all_sub <- trees_all[, !colnames(trees_all) %in% c("p50.MPa", "p80.MPa", "dbh_old.mm",  "dbh_old.cm", "sap_ratio", "height.m")]

##get rid of missing data
trees_all_sub <- trees_all_sub[complete.cases(trees_all_sub), ]
write.csv(trees_all_sub, "manuscript/tables_figures/trees_all_sub.csv", row.names=FALSE)

##5ki. NOTE ####
##originally I was separating out trees_all into 2 datasets (trees_all_bio and trees_all_full) because I thought that there was alternative missing data depending on if you were looking at biophysical traits compared to leaf traits. It turns out this was mainly due to p50 and p80, which we have since determined will not be in the model runs at all. Thus, I'm only doing one subset here.

# 
# trees_all_bio <- trees_all[c("year", "sp", "tree", "position", "resist.value", "elev.m", "distance.ln.m", "dbh.ln.cm", "height.ln.m", "position_all", "TWI")]
# trees_all_bio <- trees_all_bio[complete.cases(trees_all_bio), ]
# write.csv(trees_all_bio, "manuscript/tables_figures/trees_all_bio.csv", row.names=FALSE)
# 
# #take out p50 and p80, then keep all leaf traits and additionally height plus position
# trees_all_full <- trees_all[!colnames(trees_all) %in% c("p50.MPa", "p80.MPa")]
# trees_all_full <- trees_all_full[, c(1:10,17:20)]
# trees_all_full<- trees_all_full[complete.cases(trees_all_full), ]
# write.csv(trees_all_full, "manuscript/tables_figures/trees_all_full.csv", row.names=FALSE)

##5l. make subsets for individual years, combine all to list ####
x1966 <- trees_all_sub[trees_all_sub$year == 1966, ]
x1977 <- trees_all_sub[trees_all_sub$year == 1977, ]
x1999 <- trees_all_sub[trees_all_sub$year == 1999, ]

model_df <- list(trees_all_sub, x1966, x1977, x1999)
names(model_df) <- c("trees_all_sub", "x1966", "x1977", "x1999")

##5m. get base stats for everything from trees_all_sub ####
trees_all_sub$dbh.cm <- exp(trees_all_sub$dbh.ln.cm)
trees_all_sub$height.m <- exp(trees_all_sub$height.ln.m)

stats <- NULL
for(i in seq(along=trees_all_sub[,c(5,7:10,16,18,19)])){
  name_trait <- trees_all_sub[,c(5,7:10,16,18,19)][i]
  tree_stats <- 
    trees_all_sub %>%
    summarize(median = median(trees_all_sub[, which(colnames(trees_all_sub) == colnames(name_trait))]),
              min = min(trees_all_sub[, which(colnames(trees_all_sub) == colnames(name_trait))]),
              max = max(trees_all_sub[, which(colnames(trees_all_sub) == colnames(name_trait))])
              )
  tree_stats$trait <- colnames(name_trait)
  stats <- rbind(stats, tree_stats)
}

##############################################################################################
#6. mixed effects model for output of #5. ####
library(lme4)
library(AICcmodavg) #aictab function
library(car)
library(piecewiseSEM) #for R^2 values for all model outputs in a list
library(MuMIn) #for R^2 values of one model output
library(stringr)
library(purrr)

##6a. test each variable individually for each drought scenario ####
sum_mod_traits <- data.frame(
  "prediction" = c(3.1, 2.1, 2.2, 2.3, 1.2, 1.3, 2.4, 3.1, 3.2, 3.3, 3.4, 3.5),
  "variable" = c("year", "dbh.ln.cm", "height.ln.m", "position_all", "position_all", "height.ln.m*TWI.ln", "TWI.ln", "rp", "PLA_dry_percent", "LMA_g_per_m2", "mean_TLP_Mpa", "WD_g_per_cm3"), 
  "variable_description" = c("drought.year", "ln[DBH]", "ln[height]", "crown.position alone", "crown.position w/height", "ln[height]*ln[topographic.wetness.index]", "ln[topographic.wetness.index]", "ring.porosity", "percent.loss.area", "leaf.mass.area", "mean.turgor.loss.point", "wood.density"),
  "null_model" = 
    c("resist.value ~ (1|sp/tree)",
      "resist.value ~ year+(1|sp/tree)",
      "resist.value ~ year+(1|sp/tree)",
      "resist.value ~ year+(1|sp/tree)",
      "resist.value ~ height.ln.m+year+(1|sp/tree)",
      "resist.value ~ height.ln.m+TWI.ln+year+(1|sp/tree)",
      "resist.value ~ height.ln.m+year+(1|sp/tree)",
      "resist.value ~ height.ln.m+year+(1|sp/tree)",
      "resist.value ~ height.ln.m+year+(1|sp/tree)",
      "resist.value ~ height.ln.m+year+(1|sp/tree)",
      "resist.value ~ height.ln.m+year+(1|sp/tree)",
      "resist.value ~ height.ln.m+year+(1|sp/tree)"),
  "tested_model" = NA)

sum_mod_traits[, c("null_model_year", "tested_model_year", "dAIC_all", "coef_all", "coef_var_all", "dAIC_1964.1966", "coef_1964.1966", "coef_var_1964.1966", "dAIC_1977", "coef_1977", "coef_var_1977", "dAIC_1999", "coef_1999", "coef_var_1999")] <- NA

# change factor columns to character
sum_mod_traits <- sum_mod_traits %>% mutate_if(is.factor, as.character) 

##loop to create table of individually-tested traits
##For each variable, it compares the variable's effects in the null model and the tested model. The loop defines these models, then has two parts. If the full data (all years) model is being run [j,h,k,l == 1], it calculates the dAIC for null minus tested and the coefficient of the variable, either a "+" or a "-". If the individual year models are being run, it does the same thing but using different models from before [e.g. models_yr], to specifically exclude the "year" and "1/tree" effects.

coeff_all <- NULL
for (i in seq_along(1:12)){
  null_mod <- sum_mod_traits$null_model[[i]] #all years
  var <- sum_mod_traits$variable[[i]]
  
  #if the variable is in the null model, then take it out, otherwise add it in
  sum_mod_traits$tested_model[[i]] <- 
    ifelse(grepl(var, null_mod),
           gsub(paste0(var, "[[:punct:]]"), "", sum_mod_traits$null_model[[i]]),
           paste0(null_mod, "+", var))
  
  sum_mod_traits$null_model_year[[i]] <- 
    gsub("year\\+|/tree", "", sum_mod_traits$null_model[[i]])
  sum_mod_traits$tested_model_year[[i]] <- 
    gsub("year\\+|/tree", "", sum_mod_traits$tested_model[[i]])
  
  #obviously, the tested model of the "year" effect doesn't work over the individual years
  if (i == 1){
    sum_mod_traits$tested_model_year[[i]] <- sum_mod_traits$null_model_year[[i]]
  }
  if (i == 6){
    sum_mod_traits$tested_model[[i]] <- "resist.value~height.ln.m*TWI.ln+year+(1|sp/tree)"
    sum_mod_traits$tested_model_year[[i]] <- "resist.value~height.ln.m*TWI.ln+(1|sp)"
  }
  
  test_mod <- sum_mod_traits$tested_model[[i]] #all years
  test_mod_yr <- sum_mod_traits$tested_model_year[[i]] #individual years
  null_mod_yr <- sum_mod_traits$null_model_year[[i]] #individual years
  
  models <- c(null_mod, test_mod) #all years
  models_yr <- c(null_mod_yr, test_mod_yr) #ndividual years
  
  for (j in seq(along=model_df)){
    for (h in seq(along=sum_mod_traits[,c(8,11,14,17)])){ #dAIC
      column <- colnames(sum_mod_traits[,c(8,11,14,17)][h])
      
      for (k in seq(along=sum_mod_traits[,c(9,12,15,18)])){ #coefficients direction
        column_cof <- colnames(sum_mod_traits[,c(9,12,15,18)][k])
        
        for (l in seq(along=sum_mod_traits[,c(10,13,16,19)])){ #coefficient values
          column_cof_val <- colnames(sum_mod_traits[,c(10,13,16,19)][l])
          
          #ALL YEARS
          if(j == 1 & h == 1 & k == 1 & l == 1){
            lmm_all <- lapply(models, function(x){
              fit1 <- lmer(x, data = model_df[[j]], REML=FALSE, 
                           control = lmerControl(optimizer ="Nelder_Mead"))
              return(fit1)
            })
            names(lmm_all) <- models
            var_aic <- aictab(lmm_all, second.ord=TRUE, sort=FALSE) #rank based on AICc
            
            #put AIC value in table (#null - test)
            sum_mod_traits[,column][[i]] <- var_aic$AICc[[1]] - var_aic$AICc[[2]] 
            sum_mod_traits[,column][[i]] <- round(sum_mod_traits[,column][[i]], 3)
            
            for (z in seq(along = lmm_all)){
                if (names(lmm_all[z]) == test_mod){
                  coeff <- data.frame(coef(summary(lmm_all[[z]]))[ , "Estimate"]) ##2
                  coeff[,2] <- rownames(coeff)
                  colnames(coeff) <- c("value", "model_var")
                  coeff$value <- round(coeff$value, 4)
                  coeff$combo <- paste0(coeff$model_var, " (", coeff$value, ")")
                  coeff$mod <- gsub("coef_", "", column_cof)
                  
                  if(i == 6){
                    coeff <- coeff[grepl("height.ln.m:", coeff$combo), ]
                  } else {
                    coeff <- coeff[grepl(sum_mod_traits$variable[[i]], coeff$combo), ]
                  }
                  coeff_vec <- coeff$combo
                  
                  #this rbind is to get a full df showing all coefficient values from the entire for-loop
                  coeff_all <- rbind(coeff_all, coeff)
                  
                  #put coefficients in table
                  sum_mod_traits[,column_cof][[i]] <- ifelse(any(coeff$value < 0), "-", "+")
                  sum_mod_traits[,column_cof_val][[i]] <- paste(coeff_vec, collapse = ", ")
                }
              }
            
            #INDIVIDUAL YEARS
          } else if (j == h & h == k & k == l){ 
            if(i == 1){
              sum_mod_traits[,column][[i]] <- NA
              sum_mod_traits[,column_cof][[i]] <- NA
              sum_mod_traits[,column_cof_val][[i]] <- NA
            }
            
            lmm_all <- lapply(models_yr, function(x){
              fit1 <- lmer(x, data = model_df[[j]], REML=FALSE, 
                           control = lmerControl(optimizer ="Nelder_Mead"))
              return(fit1)
            })
            
            names(lmm_all) <- models_yr
            var_aic <- aictab(lmm_all, second.ord=TRUE, sort=FALSE) #rank based on AICc
            
            #put AIC value in table (null - tested)
            sum_mod_traits[,column][[i]] <- var_aic$AICc[[1]] - var_aic$AICc[[2]]
            sum_mod_traits[,column][[i]] <- round(sum_mod_traits[,column][[i]], 3)
            
            for (z in seq(along = lmm_all)){
                if (names(lmm_all[z]) == test_mod_yr){
                  coeff <- data.frame(coef(summary(lmm_all[[z]]))[ , "Estimate"]) ##2
                  coeff[,2] <- rownames(coeff)
                  colnames(coeff) <- c("value", "model_var")
                  coeff$value <- round(coeff$value, 4)
                  coeff$combo <- paste0(coeff$model_var, " (", coeff$value, ")")
                  coeff$mod <- gsub("coef_", "", column_cof)
                  
                  if(i == 6){
                    coeff <- coeff[grepl("height.ln.m:", coeff$combo), ]
                  } else {
                    coeff <- coeff[grepl(sum_mod_traits$variable[[i]], coeff$combo), ]
                  }
                  coeff_vec <- coeff$combo
                  
                  #this rbind is to get a full df showing all coefficient values from the entire for-loop
                  coeff_all <- rbind(coeff_all, coeff)
                  
                  #put coefficients in table
                  sum_mod_traits[,column_cof][[i]] <- ifelse(any(coeff$value < 0), "-", "+")
                  sum_mod_traits[,column_cof_val][[i]] <- paste(coeff_vec, collapse = ", ")
              }
            }
          }
        }
      }
    }
  }
}

cand_full <- NULL
for (i in seq(along=sum_mod_traits[,c(8,11,14,17)])){
  column <- colnames(sum_mod_traits[,c(8,11,14,17)])[[i]]
  
  cand <- sum_mod_traits[sum_mod_traits[,column] > 1 & 
                            !sum_mod_traits$variable %in% c("dbh.ln.cm"), c(1:3)]
  cand$top_model <- c("all", "1966", "1977", "1999")[[i]]
  
  cand_full <- rbind(cand_full, cand)
  cand_full <- cand_full[order(cand_full$prediction), ]
}

write.csv(sum_mod_traits, "manuscript/tables_figures/tested_traits_all.csv", row.names=FALSE)
write.csv(cand_full, "manuscript/tables_figures/tableS3_candidate_traits.csv", row.names=FALSE)

##6b. determine the best full model ####
best_mod_traits <- data.frame("best_model" = NA,
                              "scenario" = c("all droughts", "1966", "1977", "1999")
)
best_mod_full <- c(unique(cand_full$variable), "(1|sp/tree)")
best_mod_full_year <- gsub("/tree", "", best_mod_full)
best_mod_full_year <- best_mod_full_year[!best_mod_full_year %in% c("year")]

#this loop determines the best full model for each scenario, using the traits from cand_full in #6a, and populates the table created above
mods <- names(model_df)

top_models <- NULL
coeff_list <- list()
for (i in seq(along=c(1:4))){
  for (j in seq(along=model_df)){
    #ALL YEARS
    if(j == 1 & i == 1){
      response <- "resist.value"
      effects <- best_mod_full
      
      #create all combinations of random / fixed effects
      effects_comb <- 
        unlist(sapply(seq_len(length(effects)), function(i) {
          apply( combn(effects,i), 2, function(x) paste(x, collapse = "+"))
        }))
      
      #pair response with effect and sub out combinations that don't include random effects
      #in general, if two variables are >70% correlated, you can toss one of them without significantly affecting the results
      var_comb <- expand.grid(response, effects_comb) 
      var_comb <- var_comb[grepl("1", var_comb$Var2), ] #only keep in fixed/random combos
      var_comb$Var2 <- as.character(var_comb$Var2)
      
      #can't have height and TWI separately when the interaction is in
      for (q in seq(along=var_comb$Var2)){
        cell <- var_comb$Var2[[q]]
        if(grepl("\\*", cell)){
          if(grepl("\\+TWI.ln", cell)){
            var_comb$Var2[[q]] <- gsub("\\+TWI.ln", "", var_comb$Var2[[q]])
          }
          if(grepl("\\+height.ln.m", cell)){
          var_comb$Var2[[q]] <- gsub("\\+height.ln.m\\+", "\\+", var_comb$Var2[[q]])
          }
        }
      }
      var_comb <- unique(var_comb[,1:2])
      
      # formulas for all combinations. $Var1 is the response, and $Var2 is the effect
      # for good stats, you should have no more total parameters than 1/10th the number of observations in your dataset
      formula_vec <- sprintf("%s ~ %s", var_comb$Var1, var_comb$Var2)
      
      
      lmm_all <- lapply(formula_vec, function(x){
        fit1 <- lmer(x, data = model_df[[j]], REML=FALSE, 
                     control = lmerControl(optimizer ="Nelder_Mead"))
        return(fit1)
      })
      names(lmm_all) <- formula_vec
      var_aic <- aictab(lmm_all, second.ord=TRUE, sort=TRUE) #rank based on AICc
      
      var_aic$Modnames <- as.character(var_aic$Modnames)
      best_mod_traits$best_model[[i]] <- var_aic$Modnames[[1]]
      
      #get all mods <2 dAIC
      var_aic <- var_aic[var_aic$Delta_AICc <= 2, ]
      var_aic$mod_no <- rownames(var_aic)
      top <- var_aic[,c(1,4)]
      top$Delta_AICc <- round(top$Delta_AICc, 2)
      top$scenario <- mods[[i]]
      top$coef <- NA
      
      for (z in seq(along = lmm_all)){
        for (w in seq(along=1:nrow(var_aic))){
          if (names(lmm_all[z]) == var_aic$Modnames[[w]]){
         
           #run the best model alone with REML=TRUE
           fit1 <- lmer(formula_vec[[z]], data = model_df[[j]], REML=TRUE, 
                        control = lmerControl(optimizer ="Nelder_Mead"))
           
           #get coefficients and put in table
           coeff <- data.frame(coef(summary(fit1))[ , "Estimate"]) ##2
           coeff[,2] <- rownames(coeff)
           coeff[,1] <- round(coeff[,1], 3)
           colnames(coeff) <- c(paste0("[All years] ","Model #", w), "model_var")
           
           #put r2 in table
           delta <- data.frame(var_aic$Delta_AICc[[w]])
           colnames(delta) <- paste0("[All years] ","Model #", w)
           delta$model_var <- "dAICc"
           
           r <- rsquared(fit1) #gives R^2 values for models. "Marginal" is the R^2 for just the fixed effects, "Conditional" is the R^2 for everything.
           r <- data.frame(r[,6])
           colnames(r) <- paste0("[All years] ","Model #", w)
           r$model_var <- "r^2"
           r[,1] <- round(r[,1], 2)
           
           coeff <- rbind(delta, r, coeff)
           
           coeff_list[[paste0("coeff_", names(model_df[j]), "_", w)]] <- coeff
          }
        }
      }
      
      #INDIVIDUAL YEARS
    } else if (j == i){ 
      response <- "resist.value"
      effects <- best_mod_full_year
      
      #create all combinations of random / fixed effects
      effects_comb <- 
        unlist( sapply( seq_len(length(effects)), 
                        function(i) {
                          apply( combn(effects,i), 2, function(x) paste(x, collapse = "+"))
                        }))
      
      var_comb <- expand.grid(response, effects_comb) 
      var_comb <- var_comb[grepl("1", var_comb$Var2), ] #only keep in fixed/random combos
      
      var_comb$Var2 <- as.character(var_comb$Var2)
      
      #can't have height and TWI separately when the interaction is in
      for (q in seq(along=var_comb$Var2)){
        cell <- var_comb$Var2[[q]]
        if(grepl("\\*", cell)){
          if(grepl("\\+TWI.ln", cell)){
            var_comb$Var2[[q]] <- gsub("\\+TWI.ln", "", var_comb$Var2[[q]])
          }
          if(grepl("\\+height.ln.m", cell)){
            var_comb$Var2[[q]] <- gsub("\\+height.ln.m\\+", "\\+", var_comb$Var2[[q]])
          }
        }
      }
      var_comb <- unique(var_comb[,1:2])
      
      formula_vec <- sprintf("%s ~ %s", var_comb$Var1, var_comb$Var2)
      
      lmm_all <- lapply(formula_vec, function(x){
        fit1 <- lmer(x, data = model_df[[j]], REML=FALSE, 
                     control = lmerControl(optimizer ="Nelder_Mead"))
        return(fit1)
      })
      
      names(lmm_all) <- formula_vec
      var_aic <- aictab(lmm_all, second.ord=TRUE, sort=TRUE) #rank based on AICc
      
      var_aic$Modnames <- as.character(var_aic$Modnames)
      best_mod_traits$best_model[[i]] <- var_aic$Modnames[[1]]
      
      #get all mods <2 dAIC
      var_aic <- var_aic[var_aic$Delta_AICc <= 2, ]
      top <- var_aic[,c(1,4)]
      top$Delta_AICc <- round(top$Delta_AICc, 2)
      top$scenario <- mods[[i]]
      top$coef <- NA
      
      for (z in seq(along = lmm_all)){
        for (w in seq(along=1:nrow(var_aic))){
          if (names(lmm_all[z]) == var_aic$Modnames[[w]]){
            
            #run the best model alone with REML=TRUE
            fit1 <- lmer(formula_vec[[z]], data = model_df[[j]], REML=TRUE, 
                         control = lmerControl(optimizer ="Nelder_Mead"))
            
            #get coefficients and put in table
            coeff <- data.frame(coef(summary(fit1))[ , "Estimate"]) ##2
            coeff[,2] <- rownames(coeff)
            coeff[,1] <- round(coeff[,1], 3)
            colnames(coeff) <- c(paste0("[", names(model_df[j]), "] ","Model #", w), "model_var")
            
            #put r2 in table
            delta <- data.frame(var_aic$Delta_AICc[[w]])
            colnames(delta) <- paste0("[", names(model_df[j]), "] ","Model #", w)
            delta$model_var <- "dAICc"
            
            r <- rsquared(fit1) #gives R^2 values for models. "Marginal" is the R^2 for just the fixed effects, "Conditional" is the R^2 for everything.
            r <- data.frame(r[,6])
            colnames(r) <- paste0("[", names(model_df[j]), "] ","Model #", w)
            r$model_var <- "r^2"
            r[,1] <- round(r[,1], 2)
            
            coeff <- rbind(delta, r, coeff)
            
            coeff_list[[paste0("coeff_", names(model_df[j]), "_", w)]] <- coeff
          }
        }
      }
    }
  }
  top_models <- rbind(top_models, top)
}

write.csv(best_mod_traits, "manuscript/tables_figures/tested_traits_best.csv", row.names=FALSE)
write.csv(top_models, "manuscript/tables_figures/tableS4_top_models_dAIC.csv", row.names=FALSE)

##make table of coefficients and r2, then reorder table
#reorder the list 
# coeff_list <- coeff_list[c(4,2,6,3,1,5, #trees_all_sub
#                            9,8,10,7,11, #x1966
#                            13,12,14,15, #x1977
#                            22,23,19,17,21,20,18,24,16)] #x1999

ord_lab <- c("trees_all", "x1966", "x1977", "x1999")

coeff_list1 <- list()
for(q in seq(along=ord_lab)){
  coeff_list_temp <- coeff_list[grepl(ord_lab[[q]], names(coeff_list))]
  coeff_list_temp <- 
    coeff_list_temp[
      order(
        match(
          as.numeric(
            str_extract(names(coeff_list_temp)[grepl(ord_lab[[q]], names(coeff_list_temp))], "[[:digit:]]$")), 
          ord))]
  
  coeff_list1 <- c(coeff_list1, coeff_list_temp)
}

merge.all <- function(x, y) {
  merge(x, y, all=TRUE, by="model_var")
}

coeff_table <- Reduce(merge.all, coeff_list1)

coeff_table[,2:ncol(coeff_table)] <- round(coeff_table[,2:ncol(coeff_table)], 3)

coeff_new <- as.data.frame(t(coeff_table[,-1]))
colnames(coeff_new) <- coeff_table$model_var

# coeff_new$year1966 <- ifelse(!is.na(coeff_new$year1977), 0, NA) #only applicable if "year" is a significant variable
coeff_new$codominant <- ifelse(!is.na(coeff_new$position_alldominant), 0, NA)
coeff_new$rpdiffuse <- ifelse(!is.na(coeff_new$rpring), 0, NA)

# coeff_new <- coeff_new[, c("dAICc","r^2", "(Intercept)", "height.ln.m",
#                            "position_alldominant", "codominant", "position_allintermediate","position_allsuppressed", 
#                           "rpdiffuse", "rpring", "rpsemi-ring", "TWI.ln", "PLA_dry_percent", "mean_TLP_Mpa")]
coeff_new <- coeff_new[, c("dAICc","r^2", "(Intercept)", "height.ln.m", 
                           "position_alldominant", "codominant", "position_allintermediate","position_allsuppressed", 
                           "rpdiffuse", "rpring", "TWI.ln", "PLA_dry_percent", "mean_TLP_Mpa")]

# colnames(coeff_new) <- c("dAICc", "r^2", "Intercept","ln[H]", "D", "C", "I", "S", "diffuse", "ring", "semi-ring", "ln[TWI]", "PLA", "TLP")
colnames(coeff_new) <- c("dAICc", "r^2", "Intercept","ln[H]", "D", "C", "I", "S", "diffuse", "ring", "ln[TWI]", "PLA", "TLP")

coeff_new <- setDT(coeff_new, keep.rownames = TRUE)[]
setnames(coeff_new, old="rn", new="rank")

patterns <- c("\\[", "x", "\\]")
for(i in seq(along=patterns)){
  coeff_new$rank <- gsub(patterns[[i]], "", coeff_new$rank)
}

write.csv(coeff_new, "manuscript/tables_figures/tested_traits_best_coeff.csv", row.names=FALSE)

##6c. standalone code to get coefficients and r2 #### 
##(for paper, should do ONLY w/REML=TRUE) 
best <- lmm_all[[58]]
cof <- data.frame("value" = coef(summary(best))[ , "Estimate"])
cof$value <- round(cof$value, 3)

r <- rsquared(best) #gives R^2 values for models. "Marginal" is the R^2 for just the fixed effects, "Conditional" is the R^2 for everything.


##6d. test each leaf trait against only height ####
scbi.stem3 <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/scbi.stem3.csv"), stringsAsFactors = FALSE)

scbi.stem3$dbh <- as.numeric(scbi.stem3$dbh)
scbi.stem3$dbh <- ifelse(is.na(scbi.stem3$dbh), 0, scbi.stem3$dbh)
scbi.stem3 <- scbi.stem3[scbi.stem3$dbh>100 & !scbi.stem3$status %in% c("D", "G"), ]

current_ht <- scbi.stem3[,c(2:5,11)]
current_ht$year <- 2018

current_ht$dbh_orig.cm <- current_ht$dbh/10
current_ht$dbh.ln.cm <- log(current_ht$dbh_orig.cm)

#linear log-log regression
#the full equation is using all points for which we have data to create the equation, despite that for several species we don't have enough data to get a sp-specific equation
current_ht$height.ln.m <- 
  ifelse(current_ht$sp == "caco", (0.348+0.808*current_ht$dbh.ln.cm),
  ifelse(current_ht$sp == "cagl", (0.681+0.704*current_ht$dbh.ln.cm),
  ifelse(current_ht$sp == "caovl", (0.621+0.722*current_ht$dbh.ln.cm),
  ifelse(current_ht$sp == "cato", (0.776+0.701*current_ht$dbh.ln.cm),
  ifelse(current_ht$sp == "fagr", (0.708+0.662*current_ht$dbh.ln.cm),
  ifelse(current_ht$sp == "fram", (0.715+0.619*current_ht$dbh.ln.cm),
  ifelse(current_ht$sp == "juni", (1.22+0.49*current_ht$dbh.ln.cm),
  ifelse(current_ht$sp == "litu", (1.32+0.524*current_ht$dbh.ln.cm),
  ifelse(current_ht$sp == "qual", (1.14+0.548*current_ht$dbh.ln.cm),
  ifelse(current_ht$sp == "qupr", (0.44+0.751*current_ht$dbh.ln.cm),
  ifelse(current_ht$sp == "quru", (1.17+0.533*current_ht$dbh.ln.cm),
  ifelse(current_ht$sp == "quve", (0.864+0.585*current_ht$dbh.ln.cm),
        (0.791+0.645*current_ht$dbh.ln.cm)))))))))))))

current_ht$height.m <- exp(current_ht$height.ln.m)
current_ht <- current_ht[order(current_ht$tag, current_ht$year), ]

for (i in seq(along=trees_all[,c(6:10)])){
  col <- colnames(trees_all[,c(6:10)][i])
  
  temp <- trees_all[,c("sp", col)]
  
  current_ht[,col] <- temp[,col][match(current_ht$sp, temp$sp)]
}

current_ht$rp <- as.character(current_ht$rp)
current_ht$rp_no <- ifelse(current_ht$rp == "ring", 1,
                           ifelse(current_ht$rp == "semi-ring", 1.5,
                           2))

trait <- c("WD_g_per_cm3", "LMA_g_per_m2", "rp_no", "PLA_dry_percent", "mean_TLP_Mpa")

trait_ht <- data.frame("Prediction" = c("2.1a", "2.1b", "2.1c", "2.1d", "2.1e"),
                       "variable" = NA,
                       "model" = NA,
                       "coefficient_of_ht" = NA,
                       "p.value_of_ht" = NA)
for(i in seq(along=trait)){
  l <- lm(current_ht[,trait[[i]]] ~ current_ht[,"height.ln.m"])
  
  trait_ht[,2][[i]] <- trait[[i]]
  
  trait_ht$model[[i]] <- paste0(trait[[i]], "~height.ln.m")
  
  lm_cof <- data.frame(l$coefficients)
  trait_ht[,4][[i]] <- lm_cof[,1][[2]]
  
  lm_anova <- anova(l)
  trait_ht[,5][[i]] <- lm_anova$`Pr(>F)`[1]
}

write.csv(trait_ht, "manuscript/tables_figures/tableS5_tested_traits_height.csv", row.names=FALSE)

##6e. original table with models based on github issue predictions ####
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
write.csv(summary_models, "tables_figures/results_individual_years1.csv", row.names=FALSE)
write.csv(full_mod_all, "tables_figures/full_models_dAIC1.csv", row.names=FALSE)

##6ei. table looking at only full model over all years ####
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
write.csv(summary_models, "tables_figures/results_full_models_combined_years.csv", row.names=FALSE)

##6eii. base code for running multiple models through AICc eval ####
#define response and effects
response <- "resist.value"

# effects <- c("height.ln.m", "position_all", "TWI", "rp", "PLA_dry_percent", "mean_TLP_Mpa",  "LMA_g_per_m2", "WD_g_per_cm3", "year", "(1|sp/tree)")
effects <- c("height.ln.m", "position_all", "TWI", "rp", "PLA_dry_percent", "mean_TLP_Mpa", "LMA_g_per_m2", "WD_g_per_cm3", "(1|sp)")

# effects <- c("height.ln.m", "position_all", "TWI", "PLA_dry_percent", "mean_TLP_Mpa", "(1|sp)")
# effects <- c("height.ln.m", "position_all", "TWI", "PLA_dry_percent", "mean_TLP_Mpa", "year", "(1|sp/tree)")

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
  fit1 <- lmer(x, data = x1999, REML=FALSE, 
               control = lmerControl(optimizer ="Nelder_Mead"))
  return(fit1)
})
names(lmm_all) <- formula_vec

var_aic <- aictab(lmm_all, second.ord=TRUE, sort=TRUE) #rank based on AICc
r <- rsquared(lmm_all) #gives R^2 values for models. "Marginal" is the R^2 for just the fixed effects, "Conditional" is the R^2 for everything.

#subset by only the top result (the minimum AICc value)
aic_top <- var_aic %>%
  filter(AICc == min(AICc))

##6eiii. coefficients ####
best <- lmm_all[[150]]
coef(summary(best))[ , "Estimate"]
cof <- data.frame("value" = coef(summary(best))[ , "Estimate"])

lm_new <- lm(resist.value ~ dbh_ln*distance_ln.m, data=trees_all, REML=FALSE)

##6f. determine the best model from anova (using the model candidates above) ####
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

########################################################################################
#7. interpreting the outcomes ####
library(ggplot2)
library(RCurl)
library(tidyr)
library(grid)
library(gridExtra)

trees_all_full <- read.csv("manuscript/tables_figures/trees_all_full.csv", stringsAsFactors = FALSE)

graph_traits <- colnames(trees_all_full[, 7:11])
color <- c("dark green", "blue", "gold", "purple", "magenta")

for(i in seq(along=graph_traits)){
  trait <- graph_traits[[i]]
  trees_all_full$trait <- trees_all_full[, trait]
 
   p <- ggplot(trees_all_full) +
    geom_point(aes(x = trait, y = height.ln.m), color = color[[i]]) +
    xlab(print(trait)) +
    theme_minimal()
  
  assign(paste0(trait, "_plot"), p)
}

#arrange all graphs together and save image
png("manuscript/tables_figures/traits_vs_traits.png", width = 1000, height = 1000, pointsize = 18)
graph <- grid.arrange(PLA_dry_percent_plot, LMA_g_per_m2_plot, Chl_m2_per_g_plot, mean_TLP_Mpa_plot, WD_g_per_cm3_plot, nrow=2, top = textGrob(expression(bold("Hydraulic Traits by Height"))))

dev.off()


test
#




graph_traits <- graph_traits %>%
  gather("PLA_dry_percent", "LMA_g_per_m2", "Chl_m2_per_g", "mean_TLP_Mpa", "WD_g_per_cm3", key = "trait", value = measure)

ggplot(graph_traits) +
  geom_point(aes(x=measure, y=height.ln.m)) +
  facet_wrap(~trait) +
  theme_minimal()

##compare tree drought responses between those that lived and died ####
##corresponding to issue 26 in github

data_2018 <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/scbi.stem3_TEMPORARY.csv"), stringsAsFactors=FALSE)

data_live <- data_2018[data_2018$Tree_Status == "Live", ]
data_live$tree <- gsub( "_.*$", "", data_live$Tree_ID_Num)

cores <- unique(trees_all$tree)
data_live <- data_live[data_live$tree %in% cores, ]

livepast <- unique(data_live$tree)

#do all of step 4, then just 5i and 5j
trees_all$live <- ifelse(trees_all$tree %in% livepast, 1, 0)
trees_all$live <- as.character(trees_all$live)

ggplot(data = trees_all) +
  aes(x = live, y = resist.value, color = position_all) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
##height and canopy position by size class ####
scbi.stem3 <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/scbi.stem3.csv"), stringsAsFactors = FALSE)

scbi.stem3$dbh <- as.numeric(scbi.stem3$dbh)

current_ht <- trees_all[!duplicated(trees_all$tree), ]
current_ht$year <- 2018
current_ht <- current_ht[,c(1:4,15:17,19:21)]

current_ht$dbh_old.mm <- scbi.stem3$dbh[match(current_ht$tree, scbi.stem3$tag)]
current_ht$dbh_old.cm <- current_ht$dbh_old.mm/10
current_ht$dbh.ln.cm <- log(current_ht$dbh_old.cm)

#linear log-log regression
#the full equation is using all points for which we have data to create the equation, despite that for several species we don't have enough data to get a sp-specific equation
current_ht$height.ln.m <- 
                      ifelse(current_ht$sp == "caco", (0.348+0.808*current_ht$dbh.ln.cm),
                      ifelse(current_ht$sp == "cagl", (0.681+0.704*current_ht$dbh.ln.cm),
                      ifelse(current_ht$sp == "caovl", (0.621+0.722*current_ht$dbh.ln.cm),
                      ifelse(current_ht$sp == "cato", (0.776+0.701*current_ht$dbh.ln.cm),
                      ifelse(current_ht$sp == "fagr", (0.708+0.662*current_ht$dbh.ln.cm),
                      ifelse(current_ht$sp == "fram", (0.715+0.619*current_ht$dbh.ln.cm),
                      ifelse(current_ht$sp == "juni", (1.22+0.49*current_ht$dbh.ln.cm),
                      ifelse(current_ht$sp == "litu", (1.32+0.524*current_ht$dbh.ln.cm),
                      ifelse(current_ht$sp == "qual", (1.14+0.548*current_ht$dbh.ln.cm),
                      ifelse(current_ht$sp == "qupr", (0.44+0.751*current_ht$dbh.ln.cm),
                      ifelse(current_ht$sp == "quru", (1.17+0.533*current_ht$dbh.ln.cm),
                      ifelse(current_ht$sp == "quve", (0.864+0.585*current_ht$dbh.ln.cm),
                              (0.791+0.645*current_ht$dbh.ln.cm)))))))))))))
current_ht$height.m <- exp(current_ht$height.ln.m)

# power function Height = intercept*(diameter^slope) #for reference

current_ht <- rbind(current_ht, trees_all) #run this line to get full picture going back in time
current_ht <- current_ht[order(current_ht$tree, current_ht$year), ]

#graphing height by crown position (for paper)
current_ht <- current_ht[!is.na(current_ht$position_all), ]
current_ht$position_all <- factor(current_ht$position_all, levels = c("dominant", "co-dominant", "intermediate", "suppressed"))

ggplot(data = current_ht) +
  aes(x = position_all, y = height.m, fill = position_all, group = position_all) +
  # aes(x=position_all, y=height.m, fill=year) +
  geom_boxplot() +
  ggtitle("Current height vs crown position")+
  xlab("year") +
  ylab("height(m)") +
  theme_minimal()


# pdf of multiple graphs
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

ggplot(data = census3_sub) +
  aes(x = DBH, fill = position.crown) +
  geom_histogram(bins = 30) +
  theme_minimal() +
  facet_wrap(vars(position.crown), ncol=1)

ggplot(data = census3_sub) +
  aes(x = DBH, fill = position.crown) +
  geom_histogram(bins = 30) +
  theme_minimal()

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
