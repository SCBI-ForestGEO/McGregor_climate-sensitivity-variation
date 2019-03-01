#canopy position analysis from tree cores

cru1901 <- read.csv("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/climate_sensitivity_cores/results/canopy_vs_subcanopy/1901_2009/tables/monthly_correlation/correlation_with_CRU_SCBI_1901_2016_climate_data.csv", stringsAsFactors = FALSE)

library(ggplot2)
library(ggpubr)

#subset out caco, cato, and frni because they don't have pair of canopy and subcanopy
cru1901 <- cru1901[!(cru1901$Species %in% c("CACO_subcanopy", "CATO_subcanopy", "FRNI_subcanopy")), ]

cru1901_loop <- cru1901

#create separate identifier
cru1901_loop$position <- ifelse(grepl("subcanopy", cru1901$Species), "subcanopy", "canopy")
cru1901_loop$Species <- gsub("_[[:alpha:]]+$", "", cru1901$Species)

#1. box plots ####
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


#2. mixed effects model ####
library(car)
library(MASS)
library(lme4)
library(AICcmodavg)
library(data.table)
library(dplyr)

cru1901_loop$corr <- cru1901_loop$coef + 1 #since we have negative values

##2a. descriptions of running the mixed effects model ####

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

##2b. finding the right model to use ####
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

#subset by only the top result for each variable
aic_top <- all_models_aic[seq(1,nrow(all_models_aic), 4), ]
bic_top <- all_models_bic[seq(1,nrow(all_models_bic), 4), ]

##2c. run the best model ####
#the overall top result appears to be the full model. Thus, we use that model below.
clim_anova <- NULL
clim_BIC <- NULL
for (v in seq(along=clim)){
  cru_sub <- cru1901_lmm[cru1901_lmm$variable %in% clim[[v]], ]
  
  lmm.full <- lmer(corr ~ position + month + (1 | Species), data=cru_sub, REML=FALSE)
  
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
#3. resilience metrics ####
library(pointRes)
library(dplR)

##3a. canopy ####
setwd("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/tree_cores/chronologies/current_chronologies/complete/separated by canopy position/canopy_cores")

dirs_can <- dir("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/tree_cores/chronologies/current_chronologies/complete/separated by canopy position/canopy_cores", pattern = "_canopy.rwl")

dirs_can <- dirs_can[dirs_can != "frni_canopy.rwl" & dirs_can != "frni_drop_canopy.rwl" & dirs_can != "caco_drop_canopy.rwl"]

sp_can <- gsub("_drop_canopy.rwl", "", dirs_can)

canopy <- list()
canopy_table <- NULL
for (i in seq(along=dirs_can)){
  for (j in seq(along=sp_can)){
    if (i==j){
      file <- dirs_can[[i]]
      rings <- read.rwl(file) #read in rwl file
      area <- bai.in(rings) #convert to bai.in
      testr <- res.comp(area, nb.yrs=5, res.thresh.neg = 30, series.thresh = 50) #get resilience metrics
      canopy[[i]] <- testr
      
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

##3b. subcanopy ####
setwd("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/tree_cores/chronologies/current_chronologies/complete/separated by canopy position/subcanopy_cores")

dirs_subcan <- dir("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/tree_cores/chronologies/current_chronologies/complete/separated by canopy position/subcanopy_cores", pattern = "_subcanopy.rwl")

dirs_subcan <- dirs_subcan[dirs_subcan != "frni_drop_subcanopy.rwl" & dirs_subcan != "caco_drop_subcanopy.rwl"]

sp_subcan <- gsub("_drop_subcanopy.rwl", "", dirs_subcan)

subcanopy <- list()
subcanopy_table <- NULL
for (i in seq(along=dirs_subcan)){
  for (j in seq(along=sp_subcan)){
    if (i==j){
      file <- dirs_subcan[[i]]
      rings <- read.rwl(file) #read in rwl file
      area <- bai.in(rings) #convert to bai.in
      test <- res.comp(area, nb.yrs=5, res.thresh.neg = 20, series.thresh = 50) #get resilience metrics
      subcanopy[[i]] <- test

      test_table <- data.frame(test$out)
      test_table <- test_table[test_table$nb.series > 4, ] #remove where there are < 4 series
      test_table$sp <- sp_subcan[[j]]
      test_table$position <- "subcanopy"
      
      subcanopy_table <- rbind(subcanopy_table, test_table)
    }
  }
}
values_sub <- paste0(sp_subcan, "_can_res")
names(subcanopy) <- values_sub


full_ind <- rbind(canopy_table, subcanopy_table) #full table of indices for canopy and subcanopy cores

pointers <- 

pointers_can <- canopy_table[canopy_table$nature == -1, ]
pointers_sub <- subcanopy_table[subcanopy_table$nature == -1, ]

years <- data.frame(unique(sort(point_years$year)))
colnames(years) <- "yr"

library(dplyr)
for (i in seq(along=years$yr)){
  index <- years$yr[[i]]
  years$n.occur.can <- ifelse(pointers_can$year == index, , 0)
  years$n.occur.sub <- ifelse(pointers_sub$year == index, count(pointers_sub, vars=year), 0)
}

pos <- full_ind$position

q <- as.data.frame(table(pointers_can$year))
colnames(q) <- c("yr", "canopy")
z <- as.data.frame(table(pointers_sub$year))
colnames(z) <- c("yr", "subcanopy")

comb <- merge(q,z, all=TRUE)

setwd("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/tree-growth-and-traits")
write.csv(comb, "occurrence_of_pointer_yrs_by_canopy_position.csv", row.names=FALSE)

#it seems that unless we define pointer years, then we are unable to plot the resilience metrics in the same way that is done for Lloret et al (https://onlinelibrary.wiley.com/doi/epdf/10.1111/j.1600-0706.2011.19372.x). Trying to run the function below consistently gives an error that either we have <5 series for each pointer year, or we have no pointer years.

#The problem lies with what we consider a disturbance year. The default for a year to be considered major enough to be a pointer year is a negative growth of 40% (determined from res.comp using the bai). Our cores show almost no negative growth above 35%, and even then it's sparse.

#from the Palmer Drought Severity Index from a Winchester station, major drought (<-3 on scale) consecutively occurred (1949-2017) 
#Nov 1953 - June 1953
#May 1965 - June 1967
#Aug 1991 - March 1992
#Nov 1998 - Aug 1999

for (i in seq(along=names(canopy))){
  
}


litu <- read.rwl(dirs_can[[6]])
litu_bai <- bai.in(litu)
litu_res <- res.comp(litu_bai, nb.yrs=5, res.thresh.neg = 30, series.thresh = 50) #if >60% trees experienced growth less than 40% (40=default) in that year, then that year is given a -1 for a pointer year
litu_table <- data.frame(litu_res$out)
litu_table <- litu_table[litu_table$nb.series > 4, ]
litu_table$sp <- "litu"
litu_table$position <- "canopy"

res.plot(litu_res)

#this below is a test of using the methdology from Lloret et al 2011, where for fagr I consider significant whether the mean BAI values are >=25% lower than the median of the last 5 years. Then these were compared to the drought years above.
#However, still no pointer years. The output appears to be the same as before.
fagr <- read.rwl(dirs_can[[7]])
fagr_bai <- bai.in(fagr)

library(zoo)
fagr_bai$mean <- rowMeans(fagr_bai, na.rm=TRUE)
fagr_bai$sig <- ifelse(fagr_bai$mean < (0.75*rollmedian(fagr_bai$mean, k=5)), 1, 0)
fagr_bai$year <- row.names(fagr_bai)
fagr_bai <- fagr_bai[fagr_bai$year %in% c(1961:1971, 1972:1984, 1994:2004), ]

fagr_bai$mean <- NULL
fagr_bai$sig <- NULL
fagr_bai$year <- NULL

fagr_res <- res.comp(fagr_bai, nb.yrs=5, series.thresh = 60)
res.plot(fagr_res)

View(fagr_res$out)

