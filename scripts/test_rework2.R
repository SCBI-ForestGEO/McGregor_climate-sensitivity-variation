######################################################
# Purpose: Original code for McGregor et al, kept here for reference
# Created by: Ian McGregor - (mcgregorian93@gmail.com)
# R version 3.5.2 - First created February 2020
######################################################

# All of this code corresponds to "canopy_position_analysis.R" and contains original
# code written for the analysis, but was not used for the final analyses.
# Instead of throwing it out, I have moved it here in case we ever
# want to reference our train of thought, or the original methods we decided to use.

#########################################################################################
# this section corresponds to the new Section 1: finding pointer years and resistance metrics
##4a. determine pointer years by canopy/subcanopy groupings ####
## this (4a and 4b) was the original method for this research. We have since decided to not have these groupings.
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

##4c. comparing residuals of PDSI values and BAI of all trees ####
# this code is here because it relies on the different canopy_subcanopy groupings, but it can be easily recreated without. However, we did not use it for final analyses

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

##4d. potential base summary statistics for paper ####
##similar to 4c, this code is here because it was designed for when I had split the initial analysis into two groupings.

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
#this corresponds to Section 2: adding climate and growth variables
##5bi. add in SLA data (here for reference) ####
### I initially was going to include SLA but Krista mentioned that SLA is the inverse of LMA, so for the purposes of this modelling, they're equal. We're focused more on having SCBI-specific data when possible, so we're using LMA.
traits_sla <- read_excel("data/traits/photosynthesis_traits.xlsx", sheet = "Data")
traits_sla$species <- paste(traits_sla$Genus, traits_sla$Species)
SLA <- traits_sla
SLA$Genus.spp <- paste0(gsub("^(..).*", "\\1", SLA$Genus.spp),
                        gsub("^.* (..).*", "\\1", SLA$Genus.spp))
SLA$Genus.spp <- tolower(SLA$Genus.spp)
SLA <- SLA[SLA$Genus %in% c("Carya", "Fraxinus", "Fagus", "Juglans", "Liriodendron", "Pinus", "Quercus"), ]
SLA <- SLA[SLA$Genus.spp %in% neil_sp & !is.na(SLA$SLA), ]
SLA <- SLA[order(SLA$Genus.spp), ]
SLA <- SLA[c(10,20,28)]
unique(SLA$Genus.spp)

mean_SLA <-
  group_by(SLA, Genus.spp) %>%
  summarize(SLA_mean = mean(SLA))

trees_all$SLA_mean <- mean_SLA$SLA_mean[match(trees_all$sp, mean_SLA$Genus.spp)]

##5bii. add in p50 and p88 (here for reference) ####
#after review, we have decided to not focus on p50 and p80

#get P50 from traits table
hydra <- read.csv(text=getURL("https://raw.githubusercontent.com/EcoClimLab/HydraulicTraits/master/results/SCBI_best_fits.csv?token=AJNRBEP62SALMQHAV45TP2S5HCCPK"))

#Anderegg 2018 found that p50 and p80 came out significant in modelling
trees_all$p50.MPa <- hydra$psi_0.5_kl50[match(trees_all$sp, hydra$data.type)]
trees_all$p80.MPa <- hydra$psi_0.5_kl80[match(trees_all$sp, hydra$data.type)]

#after review, we have decided to not focus on pmin or HSM
trees_all$Pmin.MPa <- 1.1122*trees_all$mean_TLP_Mpa + 0.3849
trees_all$hsm.MPa <- trees_all$Pmin.MPa - trees_all$p50.MPa

meh <- trees_all[!duplicated(trees_all$sp), ]
meh <- meh[, c("sp", "mean_TLP_Mpa", "Pmin.MPa")]
meh$psi_Kmax_0.5 <- hydra$psi_Kmax_0.5[match(meh$sp, hydra$data.type)]
#
##5kii. note for p50 and p88 ####
#originally I was separating out trees_all into 2 datasets (trees_all_bio and trees_all_full) because I thought that there was alternative missing data depending on if you were looking at biophysical traits compared to leaf traits. It turns out this was mainly due to p50 and p80, which we have since determined will not be in the model runs at all. Thus, I'm only doing one subset here.
##5d. add in distance to water (here for reference) ####
## mapping code here is taken from survey_maps.R in Dendrobands Rscripts folder.
## I have not found a way to make this not involve personal directories without moving all the data to my folder, which I'm hesitant about doing due to data redundancy.
scbi_plot <- readOGR("D:/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/spatial_data/shapefiles/20m_grid.shp")
deer <- readOGR("D:/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/spatial_data/shapefiles/deer_exclosure_2011.shp")
roads <- readOGR("D:/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/spatial_data/shapefiles/SCBI_roads_edits.shp")
streams <- readOGR("D:/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/spatial_data/shapefiles/SCBI_streams_edits.shp")
NS_divide <- readOGR("D:/Dropbox (Smithsonian)/Github_Ian/Dendrobands/resources/maps/shapefiles/NS_divide1.shp")

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


##5e. original method for back-calculating dbh ####
dbh <- trees_all[, c(1:4)]
dbh$dbh2013 <- elev$dbh[match(dbh$tree, elev$tag)]

#create df with bark thickness log values and intercept values from Krista's paper (supplemental info)
#https://besjournals.onlinelibrary.wiley.com/doi/epdf/10.1111/1365-2435.12470
#fagr does not have bark thickness measured because it is negligible
bark <- data.frame(
  "sp" = c("acru", "fagr", "litu", "nysy", "caco", "cagl", "caovl", "cato", "fram", "juni", "qual", "qupr", "quru", "quve", "ulru"),
  "bark_thick_ln" = c(-2.564, 0, -0.659, -0.611, -1.917, -0.495, -2.504, -0.945, 0.318, -0.293, -1.231, -0.647, -0.789, 1.5, 1.133),
  "intercept" = c(0.599, 0, 0.425, 0.413, 0.503, 0.316, 0.703, 0.396, 0.295, 0.385, 0.526, 0.423, 0.341, 0.053, -0.057))

bark$bark_thick <- ifelse(bark$bark_thick_ln != 0, exp(bark$bark_thick_ln), bark$bark_thick_ln)

bark$bark_thick <- exp(bark$bark_thick_ln)

dbh$bark_thick <- bark$bark_thick[match(dbh$sp, bark$sp)]
dbh$intercept <- bark$intercept[match(dbh$sp, bark$sp)]

#the main equation is based on ring widths. We have determined the equation to be
# rw(pointer_year) <- 0.5*dbh2013 - bark_thick*(dbh2013^intercept) - sum(rw(pointer_year):rw(end)). The first part of the equation is here. Summing the pointer years happens with the "q" df below in the loop.
dbh$rw_prelim <- (0.5*dbh$dbh2013) - (dbh$bark_thick*(dbh$dbh2013^dbh$intercept))


dbh$dbh_old.mm <- "0" #in prep for below
dbh$dbh_old.mm <- as.numeric(dbh$dbh_old.mm)

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
                 dbh$rw_prelim - sum(rw[, ring_col], na.rm=TRUE), 0)
        }))

        q$dbh_old.mm <- q[,1] +q[,2] + q[,3] #add columns together
        # q$dbh_old.mm <- q[,1] +q[,2] + q[,3] + q[,4]
        dbh$dbh_old.mm <- dbh$dbh_old.mm + q$dbh_old.mm #combine with dbh
      }
    }
  }
}

# check <- dbh[dbh$dbh_old.mm == 0, ] #check if any tree was missed

trees_all$dbh_old.mm <- dbh$dbh_old.mm
trees_all$dbh_old.mm <- ifelse(trees_all$dbh_old.mm < 0, 0, trees_all$dbh_old.mm)
trees_all$dbh_old.mm <- ifelse(trees_all$dbh_old.mm > 0, trees_all$dbh_old.mm/10, trees_all$dbh_old.mm)
trees_all$dbh_ln <- ifelse(trees_all$dbh_old.mm == 0, NA, log(trees_all$dbh_old.mm))


##5ei. quick aside for new dbh method ####
# this code was used in the new method but also for the original canopy groupings:
##code for original canopy/subcanopy groupings using list made in #4a.
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
