######################################################
# Purpose: Analysis of tree cores with relation to tree characteristics, in order to determine causes of drought susceptibility (using ForestGEO cores)
# Developed by: Ian McGregor, contact Anderson-Teixeira (teixeirak@si.edu)
# R version 3.5.2 - First created February 2019, streamlined Feb. 2020
######################################################

# for ease of reading, collapse the sections ahead of time

#1. finding pointer years and resistance metrics
## necessary packages ####
rm(list=ls())
library(pointRes)
library(dplR)
library(data.table)
library(tools)
library(dplyr)
library(reshape2)

#NB ####
##I wrote this code before I realized that some of the work done in these loops had already been done in the outputs of res.comp (specifically out.select). However, since the code runs well, and I double-checked that it was giving the same outputs as analyzing out.select, I'm keeping it as is.

#Note about FRNI and CACO
#originally, getting the pointer years was done without caco because of too few cores. Now that we have the pointer years, we're adding them back in for the full analysis.
#we thought about including frni (combining the canopy and subcanopy cores because only 1 canopy), but we excluded for two reasons
##1. including frni doesn't impact the model much, and
##2. frni is the 19th most productive species, whereas every other species we have is the top 12.

#Note about PIST
##we originally included pist but ultimately we have no leaf trait data for this species, so we excluded it

##1a. determine pointer years all cores grouped together ####
#this is a combination of the two canopy and subcanopy groupings in the original code file

rings <- read.rwl("data/core_files/all_species_except_FRNI_PIST.rwl") #read in rwl file
widths <- rings #for consistency with original code
area <- bai.in(rings) #convert to bai.in

## IF DO ARIMA SKIP DOWN NOW.

resil_metrics <- res.comp(area, nb.yrs=5, res.thresh.neg = 30, series.thresh = 25) #get resilience metrics

resil_pointers <- data.frame(resil_metrics$out)
resil_pointers <- resil_pointers[resil_pointers$nb.series >=4, ]

pointers <- resil_pointers[resil_pointers$nature == -1, ]
pointers <- pointers[,c(1:3,5)]
pointers <- pointers[order(pointers$nb.series, decreasing=TRUE), ]
rownames(pointers) <- 1:nrow(pointers)

#get specific resist values for all trees
neil_list <- read.csv("data/core_files/core_list.csv", stringsAsFactors = FALSE)
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

ind$year <- as.character(ind$year) #to melt
change <- reshape2::melt(ind)
setnames(change, old=c("variable", "value"), new=c("tree", "resist.value"))


#bring in species from neil_list
change$sp <- ind_neil$sp[match(change$tree, ind_neil$tag)]

change$tree <- gsub("X", "", change$tree)
change$tree <- gsub("^0", "", change$tree)

change <- change[change$year %in% c("1966","1977","1999"), ] #for models


#this is trees_all
trees_all <- change[complete.cases(change), ]
trees_all <- trees_all[trees_all$resist.value <=2,] #constrain resistance values

#

## intervention analysis ####
# There are 4 main steps here. See the following two sites for guidance.
## https://online.stat.psu.edu/stat510/lesson/9/9.2
## https://datascienceplus.com/time-series-analysis-using-arima-model-in-r/

#Step 0 is preparing the data. We are working with mean BAI across all species.
library(forecast)
area$year <- rownames(area)
area1 <- reshape2::melt(area)

area1 <- as.data.table(area1)
area1[, variable := as.character(variable)
      ][, year := as.numeric(year)]
area2 <- area1[,.(val = mean(value, na.rm=TRUE)), by=.(year)
               ][, year := as.numeric(year)]

droughts <- c(1966, 1977, 1999)
trees <- unique(area1$variable)

indtree <- NULL
for(j in seq(along=trees)){
  area_sub <- area1[variable == trees[j], ]
  for(i in seq(along=droughts)){
    ## get subset of data to include 10 years before and after intervention
    area_dt <- area_sub[year <= droughts[i] & year >= droughts[i]-10, ]
    
    ts_dt <- ts(area_dt$value, frequency=1, start=area_dt$year[1]) ##overall ts
    
    #Step 1. Make time series, and do ARIMA model on data 
    #BEFORE the intervention
    if(!all(is.na(ts_dt)) & length(ts_dt[!is.na(ts_dt)]) > 1){
      arima_dt <- auto.arima(
        ts(area_dt$value[area_dt$year < droughts[i]], 
           frequency=1, start=area_dt$year[1]), trace=FALSE)
      
      #make trace true if want to see exact arima model chosen
      
      #Step 2. Use this model to forecast the values AFTER the intervention
      future <- forecast(arima_dt, h=1, level=c(99.5))
      # plot(future)
      
      #3. Get the ratio between the observed and predicted
      areaobs <- area_dt[area_dt$year == droughts[i], ]
      diff <- round(areaobs$val / as.numeric(future$mean),2) #ratio
    } else {diff <- NA}
    
    inddrt <- data.frame(year = droughts[i],
                       tree = trees[j],
                       diff = diff)
    indtree <- rbind(indtree, inddrt)
    
    #Plotting arima results ####
    #3.1. Prepare data for plotting
    # orig <- data.frame(year = seq(droughts[i]-10, droughts[i]-1, by=1),
    #                    val = NA,
    #                    diff=NA)
    # areaobs <- rbind(orig[,1:2], areaobs)
    # pred <- rbind(orig, pred)

    #4. Plot / characterize change
    # plot(area_dt$year, area_dt$val, ylim=c(min(pred$diff, na.rm=TRUE),
    #                                        max(area_dt$val, na.rm=TRUE)),
    #      xlab="", ylab="")
    # lines(area_dt$year, area_dt$val)
    # points(areaobs$year, areaobs$val, col="blue") #observed
    # lines(areaobs$year, areaobs$val, col="blue")
    # points(pred$year, pred$val, col="red")        #predicted
    # lines(pred$year, pred$val, col="red")
    # points(pred$year, pred$diff, col="orange")    #difference
    # lines(pred$year, pred$diff, col="orange")
    # abline(v=droughts[i], lty=2)
    # abline(h=mean(area_dt$val[area_dt$year<droughts[i]], na.rm=TRUE),
    #        lty=3, col="brown")
    # 
    # legend("bottomleft",
    #        legend=c("ARIMA data", "observed", "predicted", "difference"),
    #        col=c("black", "blue", "red", "orange"),
    #        pch=1,
    #        cex=0.55, pt.cex=1, bty="n")
    # title(main=paste0(droughts[i], " drought \nMagnitude mean BAI change = ",
    #                   round(mean(pred$diff, na.rm=TRUE),2)),
    #       xlab="Year", ylab="BAI")
  }
}

##same code as above for resist df but now for indtree
indtree$tree <- gsub("A", "", indtree$tree)

ind <- as.data.frame(indtree)
neil_list <- read.csv("data/core_files/core_list.csv", stringsAsFactors = FALSE)
neil_sp <- unique(neil_list$sp)
ind_neil <- neil_list[neil_list$tag %in% ind$tree, ]

ind$year <- as.character(ind$year)
setnames(ind, old=c("diff"), new=c("resist.value"))

#bring in species from neil_list
ind$sp <- ind_neil$sp[match(ind$tree, ind_neil$tag)]

#this is trees_all
trees_all <- ind[complete.cases(ind), ]
trees_all <- trees_all[trees_all$resist.value != Inf &
                         trees_all$resist.value < 4 &
                         trees_all$resist.value >=0, ]

#
##1b. summary stat: determine proportion of resistance values per sp ####
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

##########################################################################################
#2. add in climate and growth variables
## necessary packages ####
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

##2a. add in ring porosity qualifications ####
ring_porosity <- data.frame("sp" = c("cagl",  "caovl", "cato", "fagr", "fram", "juni",  "litu",  "pist",  "qual",  "qupr",  "quru",  "quve", "caco", "frni"), "rp" = c("ring", "ring", "ring", "diffuse", "ring", "diffuse", "diffuse", NA, "ring", "ring", "ring", "ring", "ring", "ring"))

trees_all$rp <- ring_porosity$rp[match(trees_all$sp, ring_porosity$sp)]

#gives count of each rp value
rp_test <- trees_all[!duplicated(trees_all$tree), ]
rp_test$tree <- as.numeric(rp_test$tree)
ggplot(data = rp_test) +
  aes(x = rp) +
  geom_bar(fill = "#0c4c8a") +
  theme_minimal() +
  facet_wrap(vars(year))


##2b. add in leaf traits ####
#this comes from the hydraulic traits repo, "SCBI_all_traits_table_species_level.csv"
##leaf traits gained from this include PLA_dry_percent, LMA_g_per_m2, Chl_m2_per_g, and WD [wood density]

leaf_traits <- read.csv(text=getURL("https://raw.githubusercontent.com/EcoClimLab/HydraulicTraits/master/data/SCBI/processed_trait_data/SCBI_all_traits_table_species_level.csv?token=AJNRBELC6ZFIUD2XTF4Z7W265Y6NK"), stringsAsFactors = FALSE)

leaf_traits <- leaf_traits[, c(1,8,12,26,28)]

for (i in seq(along=2:ncol(leaf_traits))){
  trait <- colnames(leaf_traits[2:ncol(leaf_traits)])
  trees_all[, trait[[i]]] <- leaf_traits[, trait[[i]]][match(trees_all$sp, leaf_traits$sp)]
}

##2c. add in elevation data ####
elev <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/spatial_data/elevation/full_stem_elevation_2013.csv"))

trees_all$elev.m <- elev$dem_sigeo[match(trees_all$tree, elev$tag)]

##2d. add in dbh for each year ####
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

pointer_years_simple <- c(1966,1977,1999)

for (j in seq(along=cols)){
  for (k in seq(along=colnames(df))){
    ring_ind <- cols[[j]]
    ring_col <- colnames(df)[[k]]
    
    if(j==k){
      #the output of this loop is 3 separate columns for each year's old dbh, hence why it is set to q as a dataframe before being combined below. Pointer_years_simple comes from #4d.
      pointer
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

##2e. add in tree heights ####
## taken from the canopy_heights script
#the full equation is using all points for which we have data to create the equation, despite that for several species we don't have enough data to get a sp-specific equation
height_regr <- read.csv("manuscript/tables_figures/publication/tableS2_height_regression.csv", stringsAsFactors = FALSE)

height_regr$Equations <- gsub("^.*= ", "", height_regr$Equations)
height_regr$Equations <- gsub("[[:alpha:]].*$", "x", height_regr$Equations)

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

##2f. add in all crown positions ####

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
# positionsp <- read.csv("data/core_files/core_chronologies_by_crownposition.csv")

##2g. add in topographic wetness index ####
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
twi_trees <- read.csv("data/core_files/core_list.csv")
twi_trees <- twi_trees[, c(1,23:24)]
twi_trees1 <- twi_trees[, c(2:3)]
twi <- extract(layers[[3]], twi_trees1, method="simple")
twi_trees$TWI <- twi

#6 add to trees_all
trees_all$TWI <- twi_trees$TWI[match(trees_all$tree, twi_trees$tag)]
trees_all$TWI.ln <- log(trees_all$TWI)

##2h. remove one bad tree ####
##fram 140939 has been mislabeled. It is recorded as having a small dbh when that is the second stem. In terms of canopy position, though, it fell between time of coring and when positions were recorded, thus we do not know its position.
trees_all <- trees_all[!trees_all$tree == 140939, ]

##2i. if using resistance value, constrain values to be <=2 ####
trees_all <- trees_all[trees_all$resist.value <=2]

# write.csv(trees_all, "manuscript/tables_figures/trees_all.csv", row.names=FALSE)
##2i. prepare dataset for running regression models ####
##take out columns that are unnecessary for model runs
trees_all_sub <- trees_all[, !colnames(trees_all) %in% c("p50.MPa", "p80.MPa", "dbh_old.mm",  "dbh_old.cm", "sap_ratio", "height.m")]

##get rid of missing data and write to csv
trees_all_sub <- trees_all_sub[complete.cases(trees_all_sub), ]
# write.csv(trees_all_sub, "manuscript/tables_figures/trees_all_sub.csv", row.names=FALSE)
# write.csv(trees_all_sub, "manuscript/tables_figures/trees_all_sub_arima.csv", row.names=FALSE)
# write.csv(trees_all_sub, "manuscript/tables_figures/trees_all_sub_arimaratio.csv", row.names=FALSE)

##2j. make subsets for individual years, combine all to list ####
x1966 <- trees_all_sub[trees_all_sub$year == 1966, ]
x1977 <- trees_all_sub[trees_all_sub$year == 1977, ]
x1999 <- trees_all_sub[trees_all_sub$year == 1999, ]

model_df <- list(trees_all_sub, x1966, x1977, x1999)
names(model_df) <- c("trees_all_sub", "x1966", "x1977", "x1999")

#
##2k. correlation plot ####
library(corrplot)
cr <- model_df[[1]]
cr$rp <- ifelse(cr$rp == "ring", 1, 2)
cr$position_all <- ifelse(cr$position_all == "dominant", 4,
                          ifelse(cr$position_all == "co-dominant", 3,
                                 ifelse(cr$position_all == "intermediate", 2, 1)))
cr[,c("year", "rp", "position_all")] <- sapply(cr[,c("year", "rp", "position_all")], 
                                                as.numeric)
correw <- cor(cr[,-c(2,4,14)])
corrplot(correw, method="number", type="lower")

##2l. get base stats for everything from trees_all_sub ####
trees_all_sub$dbh.cm <- exp(trees_all_sub$dbh.ln.cm)
trees_all_sub$height.m <- exp(trees_all_sub$height.ln.m)

stats <- NULL
for(i in seq(along=trees_all_sub[,c(5:9,16)])){
  for(j in seq)
  name_trait <- trees_all_sub[,c(5:9,16)][i]
  tree_stats <- 
    trees_all_sub %>%
    summarize(median = median(trees_all_sub[, which(colnames(trees_all_sub) == colnames(name_trait))]),
              min = min(trees_all_sub[, which(colnames(trees_all_sub) == colnames(name_trait))]),
              max = max(trees_all_sub[, which(colnames(trees_all_sub) == colnames(name_trait))])
    )
  tree_stats$trait <- colnames(name_trait)
  stats <- rbind(stats, tree_stats)
}

#########################################################################################
#3. mixed effects model for output of #2.

##start here if just re-running model runs ####
trees_all_sub <- read.csv("manuscript/tables_figures/trees_all_sub.csv", stringsAsFactors = FALSE); arima_vals=FALSE
# trees_all_sub <- read.csv("manuscript/tables_figures/trees_all_sub_arimaratio.csv", stringsAsFactors = FALSE); arima_vals=TRUE

x1966 <- trees_all_sub[trees_all_sub$year == 1966, ]
x1977 <- trees_all_sub[trees_all_sub$year == 1977, ]
x1999 <- trees_all_sub[trees_all_sub$year == 1999, ]

model_df <- list(trees_all_sub, x1966, x1977, x1999)
names(model_df) <- c("trees_all_sub", "x1966", "x1977", "x1999")

## necessary packages ####
library(lme4)
library(AICcmodavg) #aictab function
library(car)
library(piecewiseSEM) #for R^2 values for all model outputs in a list
library(MuMIn) #for R^2 values of one model output
library(stringr)
library(purrr)
library(dplyr)
library(data.table)

##3a reform. test each variable individually (expand for fuller explanation) ####
# this code chunk tests each variable individually for each drought scenario,
# using a predefined null model and test model (i.e. all test models have height).
# After these are tested from a defined data frame (sum_mod_traits), the "candidate"
# variables (cand_full) are those variables that appear in one of the 4 scenarios'
# top models. These candidates are then used in ##3b.

# The "reform" is from Issue #95 in Github, where we decide to include the TWI*height 
# interaction plus crown position in the base model that we compare against the other 
# traits. Prior to this, the base model only had height.

sum_mod_traits <- data.frame(
  "prediction" = c(3.1, 3.2, 3.3, 3.4, 3.5),
  "variable" = c("rp", "PLA_dry_percent", "LMA_g_per_m2", "mean_TLP_Mpa", "WD_g_per_cm3"), 
  "variable_description" = c("ring.porosity", "percent.loss.area", "leaf.mass.area", "mean.turgor.loss.point", "wood.density"),
  "null_model" = 
    c("resist.value ~ height.ln.m+TWI.ln+year+(1|sp/tree)",
      "resist.value ~ height.ln.m+TWI.ln+year+(1|sp/tree)",
      "resist.value ~ height.ln.m+TWI.ln+year+(1|sp/tree)",
      "resist.value ~ height.ln.m+TWI.ln+year+(1|sp/tree)",
      "resist.value ~ height.ln.m+TWI.ln+year+(1|sp/tree)"),
  "tested_model" = NA)

sum_mod_traits[, c("null_model_year", "tested_model_year", "dAIC_all", "coef_all", "coef_var_all", "dAIC_1966", "coef_1966", "coef_var_1966", "dAIC_1977", "coef_1977", "coef_var_1977", "dAIC_1999", "coef_1999", "coef_var_1999")] <- NA

# change factor columns to character
sum_mod_traits <- sum_mod_traits %>% mutate_if(is.factor, as.character) 

##loop to create table of individually-tested traits
##For each variable, it compares the variable's effects in the null model and the tested model. The loop defines these models, then has two parts. If the full data (all years) model is being run [j,h,k,l == 1], it calculates the dAIC for null minus tested and the coefficient of the variable, either a "+" or a "-". If the individual year models are being run, it does the same thing but using different models from before [e.g. models_yr], to specifically exclude the "year" and "1/tree" effects.

coeff_all <- NULL
for (i in seq_along(1:5)){
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
  test_mod <- sum_mod_traits$tested_model[[i]] #all years
  test_mod_yr <- sum_mod_traits$tested_model_year[[i]] #individual years
  null_mod_yr <- sum_mod_traits$null_model_year[[i]] #individual years
  
  models <- c(null_mod, test_mod) #all years
  models_yr <- c(null_mod_yr, test_mod_yr) #individual years
  
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
  
  if(nrow(cand)<1){
    cand <- data.frame(prediction = NA, variable = NA, 
                       variable_description = NA)
  }
  cand$top_model <- c("all", "1966", "1977", "1999")[[i]]
  
  cand_full <- rbind(cand_full, cand)
  cand_full <- cand_full[order(cand_full$prediction), ]
}

cand_full <- cand_full[complete.cases(cand_full), ]

#The info in this table is used to update table 4 (Rt) or S4 (arimaratio)
write.csv(sum_mod_traits, "manuscript/tables_figures/tested_traits_all_lmer_CPout.csv", row.names=FALSE)
write.csv(cand_full, "manuscript/tables_figures/candidate_traits_lmer_CPout.csv", row.names=FALSE)

##3b reform. determine the best full model (expand for fuller explanation) ####
# this code chunk uses the candidate variables (cand_full) from ##6a to determine
# the best full model. In other words, we have seen which variables across all
# scenarios have the most influence. Now, we test only those variables against
# each other, again across all scenarios. The loop populates the table (best_mod_traits)
# with the results AND identifies the top models for each scenario that are <=1 dAIC 
# (top_models) from the best (best_mod_traits).

# For an explanation on what "reform" means, see the description under ##3a. 

best_mod_traits <- data.frame("best_model" = NA,
                              "scenario" = c("all droughts", "1966", "1977", "1999")
)

## ONLY KEEP PLA and TLP as top variables! See Issue #95 on github.
top_vars <- c(unique(cand_full$variable))
top_vars <- c("PLA_dry_percent+mean_TLP_Mpa") #this should be PLA and TLP
best_mod_full <- c(paste0("resist.value ~ height.ln.m*TWI.ln+",
                          "height.ln.m+TWI.ln+", top_vars,
                          "+year+(1|sp/tree)"))
best_mod_full_year <- gsub("/tree", "", best_mod_full)
best_mod_full_year <- gsub("year\\+", "", best_mod_full_year)

#beginning of loop
mods <- names(model_df)

if(arima_vals)cutoff=2 else cutoff=2

top_models <- NULL
coeff_list <- list()
for (i in seq(along=c(1:4))){
  for (j in seq(along=model_df)){
    #ALL YEARS
    if(j == 1 & i == 1){
      response <- gsub(" ~.*", "", best_mod_full)
      effects <- unlist(strsplit(best_mod_full, "\\+|~ "))[-1]

      #create all combinations of random / fixed effects
      effects_comb <-
        unlist(sapply(seq_len(length(effects)),
                        function(i) {
                          apply( combn(effects,i), 2, function(x)
                            paste(x, collapse = "+"))
                        }))
      #make table
      var_comb <- expand.grid(response, effects_comb)
      var_comb <- var_comb[grepl("1", var_comb$Var2), ] #only keep in fixed/random combos
      var_comb <- var_comb[grepl("year", var_comb$Var2), ] #keep year in for drought sake
      var_comb <- var_comb[!(str_count(var_comb$Var2, "height.ln.m") == 2) &
                             !(str_count(var_comb$Var2, "TWI.ln") == 2), ]
      #remove double instances of height or TWI (the interaction includes both)

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
      
      #get all mods < threshold dAIC
      var_aic <- var_aic[var_aic$Delta_AICc <= cutoff, ]
      var_aic$mod_no <- rownames(var_aic)
      top <- var_aic[,c(1,4)]
      top$Delta_AICc <- round(top$Delta_AICc, 2)
      top$scenario <- mods[[i]]
      top$coef <- NA
      
      #now we have the top models (<2dAIC) we're presenting, run them again
      #with REML=TRUE
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
      #define response and effects
      response <- gsub(" ~.*", "", best_mod_full_year)
      effects <- unlist(strsplit(best_mod_full_year, "\\+|~ "))[-1]

      #create all combinations of random / fixed effects
      effects_comb <-
        unlist( sapply( seq_len(length(effects)),
                        function(i) {
                          apply( combn(effects,i), 2, function(x)
                            paste(x, collapse = "+"))
                        }))
      #=make table
      var_comb <- expand.grid(response, effects_comb)
      var_comb <- var_comb[grepl("1", var_comb$Var2), ] #only keep in fixed+random combos
      var_comb <- var_comb[!(str_count(var_comb$Var2, "height.ln.m") == 2) &
                             !(str_count(var_comb$Var2, "TWI.ln") == 2), ]
      #remove double instances of height or TWI (the interaction includes both)

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
      
      #get all mods <threshold  dAIC
      var_aic <- var_aic[var_aic$Delta_AICc <= cutoff, ]
      top <- var_aic[,c(1,4)]
      top$Delta_AICc <- round(top$Delta_AICc, 2)
      top$scenario <- mods[[i]]
      top$coef <- NA
      
      #now we have the top models (<2dAIC) we're presenting, run them again
      #with REML=TRUE
      for (z in seq(along = lmm_all)){
        for (w in seq(along=1:nrow(var_aic))){
          if (names(lmm_all[z]) == var_aic$Modnames[[w]]){
            
            #run the model
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

write.csv(best_mod_traits, "manuscript/tables_figures/tested_traits_best_lmer_CPout.csv", row.names=FALSE)
write.csv(top_models, "manuscript/tables_figures/top_models_dAIC_lmer_CPout.csv", row.names=FALSE)

#
#3bi. VIF; this is for when we fully decide what our best model is!!! ####
# VIF (variance inflation factors) are used to test multicollinearity. The end result
# of this code chunk is a csv of the VIF values per variable. 
# Values that are "good" VIF values are around 1. Anything above 10 is bad.
# In a way, this is a companion to a correlation plot.
# https://www.statisticshowto.com/variance-inflation-factor/

input <- best_mod_traits$best_model
best_mod_traits$best_model <- gsub("\\*", "\\+", input)

hazel_vif <- NULL
for(i in 1:nrow(best_mod_traits)){
  mod <- glmer(best_mod_traits$best_model[i], 
                       data = model_df[[i]],
                       control = 
                 lmerControl(optimizer ="Nelder_Mead"))
  
  hazel <- as.data.frame(car::vif(mod))
  hazel$scen <- best_mod_traits$scenario[i]
  hazel_vif <- rbind(hazel_vif, hazel)
}

output_list <- list()
for(i in 1:nrow(top_models)){
  mod <- glmer(top_models$Modnames[i], 
               data = model_df[[top_models$scenario[i]]],
               control = 
                 lmerControl(optimizer ="Nelder_Mead"))
  
  output_list[[i]] <- mod
}
names(output_list) <- paste0(top_models$scenario, "_", top_models$Delta_AICc)

#we take anova of top models for each scenario. Since only one of those has >1 top model,
#that's all we do
rpo <- anova(output_list[[4]], output_list[[5]])

top_models$order_original <- 1:5
top_models$order_anova <- 
  c(1:3, as.numeric(str_extract(rownames(rpo), "[[:digit:]]")))

write.csv(hazel_vif, "manuscript/tables_figures/top_models_dAIC_VIF_reform_arimaratio.csv", row.names=FALSE)

##3c. Make table of coefficients plus r^2 from top models from ##3b. ####

# this loop takes the top models from coeff_list and reorders them by how well they
# did (i.e. reorders by the number in their individual names)
ord_lab <- c("trees_all", "x1966", "x1977", "x1999")

coeff_list1 <- list()
for(q in seq(along=ord_lab)){
  coeff_list_temp <- coeff_list[grepl(ord_lab[[q]], names(coeff_list))]
  coeff_list_temp <- 
    coeff_list_temp[
      order((as.numeric(
        str_extract(names(coeff_list_temp)
                    [grepl(ord_lab[[q]], names(coeff_list_temp))], 
                    "[[:digit:]]*$")
      )))]
  
  coeff_list1 <- c(coeff_list1, coeff_list_temp)
}

merge.all <- function(x, y) {
  merge(x, y, all=TRUE, by="model_var")
}

coeff_table <- Reduce(merge.all, coeff_list1)

coeff_table[,2:ncol(coeff_table)] <- round(coeff_table[,2:ncol(coeff_table)], 3)

coeff_new <- as.data.frame(t(coeff_table[,-1]))
colnames(coeff_new) <- coeff_table$model_var
coeff_new$year <- NULL #we ignore year because we assume it's significant

# add in 0 values for multi-option variables
coeff_new$codominant <- ifelse(!is.na(coeff_new$position_alldominant), 0, NA)
# coeff_new$rpdiffuse <- ifelse(!is.na(coeff_new$rpring), 0, NA)

if(any(grepl("1", colnames(coeff_new)))){
  colnames(coeff_new) <- gsub("1", "no_vars", colnames(coeff_new))
  
  coeff_new <- coeff_new[, c("dAICc","r^2", "(Intercept)", "height.ln.m",
                             "TWI.ln", "height.ln.m:TWI.ln",
                             "PLA_dry_percent", "mean_TLP_Mpa", "no_vars")]
  colnames(coeff_new) <- c("dAICc", "r^2", "Intercept","ln[H]", "ln[TWI]", 
                           "ln[H]*ln[TWI]",
                           "PLA", "TLP", "no_vars")
} else {
  coeff_new <- coeff_new[, c("dAICc","r^2", "(Intercept)", "height.ln.m",
                             "TWI.ln", "height.ln.m:TWI.ln",
                             "PLA_dry_percent", "mean_TLP_Mpa")]
  colnames(coeff_new) <- c("dAICc", "r^2", "Intercept","ln[H]", "ln[TWI]", 
                           "ln[H]*ln[TWI]",
                           "PLA", "TLP")
}

coeff_new <- setDT(coeff_new, keep.rownames = TRUE)[]
setnames(coeff_new, old="rn", new="rank")

patterns <- c("\\[", "x", "\\]")
for(i in seq(along=patterns)){
  coeff_new$rank <- gsub(patterns[[i]], "", coeff_new$rank)
}

#this table is used to fill in Table 5 (Rt) or S5 (arimaratio)
write.csv(coeff_new, "manuscript/tables_figures/tested_traits_best_coeff_lmer_CPout.csv", row.names=FALSE)

## END OF NORMAL ANALYSIS. 3d and 3e are extra

##3d. compare Rt values with arima_ratio ####
library(data.table)
arima_ratio <- read.csv("manuscript/tables_figures/trees_all_sub_arimaratio.csv", stringsAsFactors = FALSE)

rt <- read.csv("manuscript/tables_figures/trees_all_sub.csv", stringsAsFactors = FALSE)

years <- c(1966, 1977, 1999)

layout(matrix(1:8, nrow=2, byrow=TRUE))
res_full <- NULL
for(i in 1:4){
  cols <- c("tree", "resist.value")
  
  if(i==1){
    cols <- c("year", cols)
    compare <- rt[,cols]
    arimadf <- arima_ratio
    compare$arimart <- arimadf$resist.value[
      match(paste0(compare$year, compare$tree),
            paste0(arimadf$year, arimadf$tree))]
  } else {
    compare <- rt[rt$year == years[i-1], cols]
    arimadf <- arima_ratio[arima_ratio$year == years[i-1], ]
    compare$arimart <- arimadf$resist.value[
      match(compare$tree, arimadf$tree)]
  }
  
  compare <- compare[complete.cases(compare),]
  
  plot(compare$resist.value, compare$arimart, 
       main=if(i==1){"All years"} else {as.character(years[i-1])},
       xlab="Rt", ylab="ARIMA")
  abline(coef=c(0,1), col="red")
  
  #put together all direct comparisons
  if(i==1){
    tabs4 <- compare
    setnames(tabs6, old=c("year", "tree", "resist.value", "arimart"),
             new=c("Year", "Tree", "$Rt$", "$Rt_{ARIMA}$"))
  }
  
  #calculate top 3 +- deviations from 1:1 line
  y <- compare$arimart
  x <- compare$resist.value
  compare$resid_val <- resid(lm(y-x ~ 0))
  compare <- compare[order(compare$resid_val), ]
  hist(compare$resid_val, xlab="Residual value from 1:1 line",
       main=if(i==1){"All years"} else {as.character(years[i-1])})
  
  resids <- data.frame(rbind(head(compare, n=3), tail(compare, n=3)))
  resids$year <- if(i==1){"all"} else {years[i-1]}
  
  res_full <- rbind(res_full, resids)
}
write.csv(tabs4, "manuscript/tables_figures/Rt_arimaratio_comparison.csv", row.names=FALSE)
write.csv(res_full, "manuscript/tables_figures/top_residual_deviations.csv", row.names=FALSE)


#
##3a original. test each variable individually (expand for fuller explanation) ####
# this code chunk tests each variable individually for each drought scenario,
# using a predefined null model and test model (i.e. all test models have height).
# After these are tested from a defined data frame (sum_mod_traits), the "candidate"
# variables (cand_full) are those variables that appear in one of the 4 scenarios'
# top models. These candidates are then used in ##6b.

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
  models_yr <- c(null_mod_yr, test_mod_yr) #individual years
  
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
              fit1 <- glmer(x, data = model_df[[j]], #REML=FALSE, 
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
              fit1 <- glmer(x, data = model_df[[j]], REML=FALSE, 
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
  
  if(nrow(cand)<1){
    cand <- data.frame(prediction = NA, variable = NA, 
                       variable_description = NA)
  }
  cand$top_model <- c("all", "1966", "1977", "1999")[[i]]
  
  cand_full <- rbind(cand_full, cand)
  cand_full <- cand_full[order(cand_full$prediction), ]
}

cand_full <- cand_full[complete.cases(cand_full), ]

write.csv(sum_mod_traits, "manuscript/tables_figures/tested_traits_all.csv", row.names=FALSE)
write.csv(cand_full, "manuscript/tables_figures/candidate_traits.csv", row.names=FALSE)

##3b original. determine the best full model (expand for fuller explanation) ####
# this code chunk uses the candidate variables (cand_full) from ##6a to determine
# the best full model. In other words, we have seen which variables across all
# scenarios have the most influence. Now, we test only those variables against
# each other, again across all scenarios. The loop populates the table (best_mod_traits)
# with the results AND identifies the top models for each scenario that are <=2 dAIC 
# (top_models) from the best (best_mod_traits).

best_mod_traits <- data.frame("best_model" = NA,
                              "scenario" = c("all droughts", "1966", "1977", "1999")
)
best_mod_full <- c(unique(cand_full$variable), "(1|sp/tree)")
best_mod_full_year <- gsub("/tree", "", best_mod_full)
best_mod_full_year <- best_mod_full_year[!best_mod_full_year %in% c("year")]

#beginning of loop
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
        fit1 <- glmer(x, data = model_df[[j]], REML=FALSE, 
                      control = lmerControl(optimizer ="Nelder_Mead"))
        return(fit1)
      })
      names(lmm_all) <- formula_vec
      var_aic <- aictab(lmm_all, second.ord=TRUE, sort=TRUE) #rank based on AICc
      
      var_aic$Modnames <- as.character(var_aic$Modnames)
      best_mod_traits$best_model[[i]] <- var_aic$Modnames[[1]]
      
      #get all mods <1 dAIC
      var_aic <- var_aic[var_aic$Delta_AICc <= 1, ]
      var_aic$mod_no <- rownames(var_aic)
      top <- var_aic[,c(1,4)]
      top$Delta_AICc <- round(top$Delta_AICc, 2)
      top$scenario <- mods[[i]]
      top$coef <- NA
      
      for (z in seq(along = lmm_all)){
        for (w in seq(along=1:nrow(var_aic))){
          if (names(lmm_all[z]) == var_aic$Modnames[[w]]){
            
            #run the best model alone with REML=TRUE
            fit1 <- glmer(formula_vec[[z]], data = model_df[[j]], REML=TRUE, 
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
        fit1 <- glmer(x, data = model_df[[j]], REML=FALSE, 
                      control = lmerControl(optimizer ="Nelder_Mead"))
        return(fit1)
      })
      
      names(lmm_all) <- formula_vec
      var_aic <- aictab(lmm_all, second.ord=TRUE, sort=TRUE) #rank based on AICc
      
      var_aic$Modnames <- as.character(var_aic$Modnames)
      best_mod_traits$best_model[[i]] <- var_aic$Modnames[[1]]
      
      #get all mods <1 dAIC
      var_aic <- var_aic[var_aic$Delta_AICc <= 1, ]
      top <- var_aic[,c(1,4)]
      top$Delta_AICc <- round(top$Delta_AICc, 2)
      top$scenario <- mods[[i]]
      top$coef <- NA
      
      for (z in seq(along = lmm_all)){
        for (w in seq(along=1:nrow(var_aic))){
          if (names(lmm_all[z]) == var_aic$Modnames[[w]]){
            
            #run the best model alone with REML=TRUE
            fit1 <- glmer(formula_vec[[z]], data = model_df[[j]], #REML=TRUE,
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
write.csv(top_models, "manuscript/tables_figures/top_models_dAIC.csv", row.names=FALSE)

##################
#3 Appendix for code
##################
##3d. standalone code to get coefficients and r2 #### 
##(for paper, should do ONLY w/REML=TRUE) 
best <- lmm_all[[58]]
cof <- data.frame("value" = coef(summary(best))[ , "Estimate"])
cof$value <- round(cof$value, 3)

r <- rsquared(best) #gives R^2 values for models. "Marginal" is the R^2 for just the fixed effects, "Conditional" is the R^2 for everything.

##3e. test each leaf trait against only height ####
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

write.csv(trait_ht, "manuscript/tables_figures/tested_traits_height.csv", row.names=FALSE)

