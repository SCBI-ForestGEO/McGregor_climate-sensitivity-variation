######################################################
# Purpose: Calculate past heights of different species in ForestGEO from regression equations
# Developed by: Ian McGregor - mcgregori@si.edu
# R version 3.5.3 - First created April 2019
######################################################
library(data.table)
library(RCurl)
library(ggplot2)
library(devtools)
library(stringr)

#1. prepare SCBI data ####
heights <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_dimensions/tree_heights/SCBI_tree_heights.csv"), stringsAsFactors = FALSE)

dbh_2008 <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/scbi.stem1.csv"), stringsAsFactors = FALSE)
dbh_2008$dbh.cm <- as.numeric(dbh_2008$dbh)/10 #cm

dbh_2013 <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/scbi.stem2.csv"), stringsAsFactors = FALSE)
dbh_2013$dbh.cm <- dbh_2013$dbh/10 #cm

dbh_2018 <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/scbi.stem3.csv"), stringsAsFactors = FALSE)
dbh_2018$dbh.cm <- as.numeric(dbh_2018$dbh)/10 #cm

heights$dbh_regr.cm <- paste0(heights$DBH.2008.cm, heights$DBH.2013.cm, heights$DBH.TLS.2015.cm, heights$DBH.2018.cm, heights$DBH.NEON.cm)
heights$dbh_regr.cm <- gsub("NA", "", heights$dbh_regr.cm)
heights$dbh_regr.cm <- as.numeric(heights$dbh_regr.cm)

heights$dbh_year <- NA
heights$dbh_year <- ifelse(heights$dbh_regr.cm %in% heights$DBH.2008.cm, 2008,
                     ifelse(heights$dbh_regr.cm %in% heights$DBH.2013.cm, 2013,
                      ifelse(heights$dbh_regr.cm %in% heights$DBH.TLS.2015.cm, 2015,
                      ifelse(heights$dbh_regr.cm %in% heights$DBH.2018.cm, 2018,
                                 heights$dbh_year))))
heights$height.year <- as.character(heights$height.year)
heights$dbh_year <- as.numeric(stringr::str_extract(heights$height.year, "^\\d{4}"))

dup <- heights$stemID[duplicated(heights$stemID)]
heights_dup <- heights[heights$stemID %in% dup, ]
heights_nodup <- heights[!heights$stemID %in% dup, ]

heights_recent <- NULL
for (i in seq(along=unique(heights_dup$stemID))){
  sub <- heights_dup[heights_dup$stemID %in% unique(heights_dup$stemID)[[i]], ]
  
  sub <- sub[sub$height.year == max(sub$height.year), ]
  
  heights_recent <- rbind(heights_recent, sub)
}

heights_all <- rbind(heights_nodup, heights_recent)
heights_all <- heights_all[c(1:5,7,9,16:17,26:27)]


## sine bias ####
#we are not applying a fix for sine bias because a number of papers that used the sine method also did not apply any corrections. Instead, they (sometimes) acknowledged the error associated with it.
#2. get neon height data ####
file_path <- file.path("data/heights/NEON/neon_heights/")
dirs_map <- dir("data/heights/NEON/neon_heights", pattern="mapping_and_tagging.*$")
dirs_ht <- dir("data/heights/NEON/neon_heights", pattern="neon_ht.*$")

neon_all <- NULL
for (i in seq(along=dirs_map)){
  for (j in seq(along=dirs_ht)){
    if (i == j){
    neon_id <- read.csv(paste(file_path, dirs_map[[i]], sep="//"), stringsAsFactors = FALSE)
    neon_ht <- read.csv(paste(file_path, dirs_ht[[j]], sep="//"), stringsAsFactors = FALSE)
      
      id <- neon_id$individualID
      
      neon_id <- neon_id[c("individualID", "taxonID", "scientificName")]
      neon_ht$sp <- neon_id$taxonID[match(neon_ht$individualID, neon_id$individualID)]
      neon_ht <- neon_ht[c("plotID", "sp", "individualID", "plantStatus", "stemDiameter", "measurementHeight", "height")]
      
      neon_ht$sp <- tolower(neon_ht$sp)
      neon_ht$dbh_year <- str_extract(dirs_ht[[j]], "[[:digit:]]*-[[:digit:]]*")
      neon_ht$dbh_year <- gsub("-", ".", neon_ht$dbh_year)
      setnames(neon_ht, old=c("stemDiameter", "measurementHeight", "height"), new=c("dbh_regr.cm", "dbh_height.cm", "height.m"))
      
      #exclude unknown id (2plant, 2plant-h), vines/shrubs (syor, vitis, ceor7, tora2, paqu2, romu, loma6, loja, rual, ruph, beth, libe), and non-sp-specific (carya, querc, fraxi, ulmus, vibur, pyrus, diosp, rubus)
      
      neon_ht <- neon_ht[!(neon_ht$sp %in% c("2plant", "2plant-h", "syor", "vitis", "ceor7", "tora2", "paqu2", "romu", "loma6", "loja", "rual", "ruph", "beth", "libe3", "carya", "querc", "fraxi", "ulmus", "vibur", "pyrus", "diosp", "rubus")), ]
      neon_ht$sp <- ifelse(neon_ht$sp == "asimi", "astr",
                    ifelse(neon_ht$sp == "fram2", "fram",
                    ifelse(neon_ht$sp == "cagl8", "cagl",
                    ifelse(neon_ht$sp == "caov3", "caovl",
                    ifelse(neon_ht$sp == "sassa", "saal",
                    ifelse(neon_ht$sp == "cato6", "cato",
                    ifelse(neon_ht$sp == "cecac", "ceca",
                    ifelse(neon_ht$sp == "caco15", "caco",
                    ifelse(neon_ht$sp == "acnen", "acne",
                    ifelse(neon_ht$sp == "prses", "prse",
                    ifelse(neon_ht$sp == "qumo4", "qupr",
                    ifelse(neon_ht$sp == "pivi2", "pivi",
                    ifelse(neon_ht$sp == "cofl2", "cofl",
                    ifelse(neon_ht$sp == "pato2", "pato", neon_ht$sp))))))))))))))
      
      #filter out saplings and weird dbh
      neon_ht <- neon_ht[!(is.na(neon_ht$dbh_height.cm)) & !(neon_ht$dbh_height.cm <110) & !(neon_ht$dbh_height.cm >150), ]
      
      neon_ht <- neon_ht[grepl("Live", neon_ht$plantStatus), ]
      neon_ht <- neon_ht[neon_ht$individualID %in% unique(neon_ht$individualID[neon_ht$dbh_year == max(neon_ht$dbh_year)]), ]
      
      #rbind to have full dataset ####
      neon_ht_sub <- neon_ht[c(2,3,5,7,8)]
      
      neon_all <- rbind(neon_all, neon_ht_sub)
    }
  }
}

neon_all <- neon_all[complete.cases(neon_all), ]
neon_all$dbh_year <- as.numeric(neon_all$dbh_year)

#get only one measurement per tree (most recent)
neon_new <- NULL
for (i in seq(along=neon_all$individualID)){
  poss <- neon_all[neon_all$individualID == neon_all$individualID[[i]], ]

  poss <- poss[poss$dbh_year == max(poss$dbh_year), ]
  
  neon_new <- rbind(neon_new, poss)
}

#get rid of duplicates again (unsure why the loop does this)
neon_new <- neon_new[!duplicated(neon_new$individualID), ]

#get rid of data outliers (impossible height or dbh given the other)
neon_new <- neon_new[!neon_new$individualID %in% c("NEON.PLA.D02.SCBI.03428", "NEON.PLA.D02.SCBI.02787"), ]

#remove individualID so can rbind below
neon_new <- neon_new[-c(2)]
neon_new$dbh_year <- as.numeric(stringr::str_extract(neon_new$dbh_year, "^\\d{4}"))


#3. rbind with general height data and determine equations ####
heights_sub <- heights[c(5,9,26,27)]


heights_all <- rbind(heights_sub, neon_new)


#check which ones need dbh from previous census because they died (should be 0)
check <- heights_all[is.na(heights_all$dbh) | heights_all$dbh ==0, ]

# heights_all$dbh.cm <- ifelse(heights_all$dbh.cm == 0 & heights_all$dbh_year == 2013, 
#                       dbh_2008$dbh.cm[match(heights_all$stemID, dbh_2008$stemID)], 
#                       ifelse(is.na(heights_all$dbh.cm) & heights_all$dbh_year == 2018, 
#                              dbh_2013$dbh[match(heights_all$stemID, dbh_2013$stemID)], 
#                               heights_all$dbh.cm))

#4. make regression equations ####
#bring in list of cored species we're using
neil_list <- read.csv("data/core_list_for_neil.csv", stringsAsFactors = FALSE)
neil_sp <- unique(neil_list$sp)

paper_heights <- heights_all[heights_all$sp %in% neil_sp, ]
paper_heights <- paper_heights[order(paper_heights$sp), ]

unique(heights_all$sp) #shows all sp that we have height data for
unique(paper_heights$sp) #shows the cored sp that we have data for
paper_heights <- paper_heights[!paper_heights$sp %in% c("fram", "juni", "quve"), ] #juni, fram, and quve have <5 measurements has only one measure

#DBH IS IN CM, HEIGHT IS IN M

source_gist("524eade46135f6348140")
ggplot(data = paper_heights, aes(x = log(dbh_regr.cm), y = log(height.m), label = log(height.m))) +
  stat_smooth_func(geom="text",method="lm",hjust=0.16, vjust=0,parse=TRUE) +
  geom_smooth(method="lm", se=FALSE, color="black") +
  geom_point(color = "#0c4c8a") +
  theme_minimal() +
  facet_wrap(vars(sp))


#equations for all species together
ggplot(data = paper_heights, aes(x = log(dbh_regr.cm), y = log(height.m), label = log(height.m))) +
  stat_smooth_func(geom="text",method="lm",hjust=0.16, vjust=-1.7,parse=TRUE) +
  geom_smooth(method="lm", se=FALSE, color="black") +
  geom_point(color = "#0c4c8a") +
  theme_minimal()

regr <- data.frame("Species" = c("Carya cordiformis", "Carya glabra", "Carya ovalis", "Carya tomentosa", "Fagus grandifolia", "Liriodendron tulipifera", "Quercus alba", "Quercus prinus", "Quercus rubra", "all"),
                   "sp" = c("caco", "cagl", "caovl", "cato", "fagr", "litu", "qual", "qupr", "quru", "all"),
                   "Equations" = c("0.332+0.808*x", #caco
                                   "0.685+0.691*x", #cagl
                                   "0.533+0.741*x", #caovl
                                   "0.726+0.713*x", #cato
                                   "0.708+0.662*x", #fagr
                                   "1.33+0.52*x", #litu
                                   "0.74+0.645*x", #qual
                                   "0.41+0.757*x", #qupr
                                   "1.00+0.574*x", #quru
                                   "0.839+0.642*x"), #all
                   "r^2" = c(0.874, 0.841, 0.924,
                             0.897, 0.857, 0.771,
                             0.719,0.886, 0.755, 
                             0.857))

write.csv(regr, "manuscript/tables_figures/tableS2_height_regression.csv", row.names=FALSE)

#########################################################################
#create plots showing height diff by year (stand-alone) ####
##this code compares measured heights from 2012-2019 with regressed heights from trees_all

library(data.table)
library(ggplot2)
trees <- read.csv("manuscript/tables_figures/trees_all.csv", stringsAsFactors = FALSE)
heights <- read.csv("E:/Github_SCBI/SCBI-ForestGEO-Data/tree_dimensions/tree_heights/SCBI_tree_heights.csv", stringsAsFactors = FALSE)

h <- heights[,c("tag", "sp", "height.year", "height.m")]
ht_trees <- trees[,c("year", "tree", "sp", "height.m", "position_all")]

h$position_all <- ht$position_all[match(h$tag, ht_trees$tree)]

library(data.table)
setnames(h, old=c("tag", "height.year"), new=c("tree", "year"))

test <- rbind(ht_trees, h)
test$year <- round(test$year)
test$year <- ifelse(test$year >=2012 & test$year<=2019, 2018, test$year)
test$year <- as.character(test$year)

test_sub <- test[complete.cases(test), ]
test_sub$position_all <- factor(test_sub$position_all, c("dominant", "co-dominant", "intermediate", "suppressed"))

comp <- as.data.table(test)
comp[, mean(height.m), by=.(year)] #bringing in NEON heights to compare with all the regressed heights show a 5m decrease in height from 1999 to 2018.

comp_sub <- as.data.table(test_sub)
comp[, mean(height.m), by=.(year)] #however, when comparing heights only with our tagged trees, we see a 6m jump in height from 1999 to 2018. Unsure what this means for how the data was collected
ggplot(comp_sub, aes(x=position_all, y=height.m)) +
  geom_boxplot(aes(fill=year)) +
  ylab("Height [m]") +
  xlab("Canopy position")

ggplot(test, aes(x=position_all, y=height.m)) +
  geom_boxplot(aes(fill=year)) +
  ylab("Height [m]") +
  xlab("Canopy position")

png("manuscript/tables_figures/height_comparison.png")
ggplot(comp_sub, aes(x=position_all, y=height.m)) +
  geom_boxplot(aes(fill=year)) +
  ylab("Height [m]") +
  xlab("Canopy position")
dev.off()

bysp <- comp[, mean(height.m), by=.(year, sp)]
ggplot(test, aes(x=position_all, y=height.m)) +
  geom_boxplot(aes(fill=year)) +
  ylab("Height [m]") +
  xlab("Canopy position") +
  facet_wrap(vars(sp))

#########################################################################################
#get quadrat and coordinates for field checking ####
heights_all$quadrat <- dbh_2013$quadrat[match(heights_all$stemID, dbh_2013$stemID)]
heights_all <- heights_all[order(heights_all$quadrat, heights_all$tag), ]
dbh_2013$lx <- dbh_2013$gx - 20*((dbh_2013$quadrat %/% 100) - 1)
dbh_2013$ly <- dbh_2013$gy - 20*((dbh_2013$quadrat %% 100) - 1)
heights_all$lx <- dbh_2013$lx[match(heights_all$stemID, dbh_2013$stemID)]
heights_all$ly <- dbh_2013$ly[match(heights_all$stemID, dbh_2013$stemID)]

#round local coordinates to nearest tenth
heights_all$lx <- round(heights_all$lx, digits=1)
heights_all$ly <- round(heights_all$ly, digits=1)

#get current dbh and live/dead status from 2018
heights_all <- heights_all[c(1:3,10:12,4:9)]
heights_all$dbh_2018.cm <- dbh_2018$DBHcm[match(heights_all$stemID, dbh_2018$stemID)]
heights_all$status <- dbh_2018$Tree_Status[match(heights_all$stemID, dbh_2018$stemID)]

heights_all_check <- heights_all[c(1:8,10:14)]
write.csv(heights_all_check, "data/heights_all_check.csv", row.names=FALSE)

############################################################################################
#figure out differences between researcher measurements #####
heights <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_dimensions/tree_heights/SCBI_tree_heights.csv"), stringsAsFactors = FALSE)

heights <- heights[,c(1:4,6,8,14:17)]

heights_dup <- heights[duplicated(heights$stemID), ]
dup <- heights_dup$stemID

heights_dup <- heights[heights$stemID %in% dup, ]
heights_dup <- heights_dup[order(heights_dup$tag), ]
heights_dup$height.year <- as.character(heights_dup$height.year)

setnames(heights, old="species.code", new="sp")

heights_dup$researcher <- ifelse(heights_dup$researcher == "Jonathan Thompson", "JT",
                            ifelse(heights_dup$researcher == "Atticus Stovall", "AS",
                            ifelse(heights_dup$researcher == "Ian McGregor", "IM",
                            ifelse(heights_dup$researcher == "Sarah Macey (GIS Lab)", "SM",
                            ifelse(heights_dup$researcher == "Jennifer McGarvey", "JM",
                                    heights_dup$researcher)))))

#graph showing difference between height measurements (by year and method) per tag
ggplot(data = heights_dup) +
  aes(x = researcher, color = method, fill=height.year, weight=height.m) +
  geom_bar() +
  theme_minimal() +
  facet_wrap(vars(tag))

#simplify to just be my re-measurements
ian <- heights_dup[heights_dup$researcher == "IM", ]
ian_tag <- ian$tag
ian <- heights_dup[heights_dup$tag %in% ian_tag, ]

#graph showing just my measurements compared to older ones
ggplot(data = ian) +
  aes(x = researcher, color = method, fill=height.year, weight=height.m) +
  geom_bar() +
  theme_minimal() +
  facet_wrap(vars(tag))

#get % difference between measurements and difference in years
ian$height.year <- as.numeric(ian$height.year)
ian$height.diff <- NA
ian$year.diff <- NA
for (i in seq(along=ian$tag)){
  sub <- ian[ian$tag == ian$tag[[i]], ]
  sub_ian <- sub[sub$researcher == "IM", ]
  sub_oth <- sub[sub$researcher != "IM", ]
  
  ian$height.diff <- ifelse(ian$tag == sub_ian$tag & ian$researcher == "IM", 
    100-(sub_oth$height.m[sub_oth$height.year == max(sub_oth$height.year)]/sub_ian$height.m)*100,
                    ian$height.diff)
  
  ian$year.diff <- ifelse(ian$tag == sub_ian$tag & ian$researcher == "IM", 
                         sub_ian$height.year-sub_oth$height.year, ian$year.diff)
}

ian_mean <- ian[ian$researcher == "IM", ]

#if sign is -1, that means my measurement was lower than a previous measurement. +1 means mine was higher.
ian_mean$height.dir <- ifelse(ian_mean$height.diff < 0, -1,
                         ifelse(ian_mean$height.diff >= 0, 1, ian_mean$height.dir))
ian_mean$height.diff <- ifelse(ian_mean$height.diff <0, -1*ian_mean$height.diff, ian_mean$height.diff)

#get mean % difference based on category
mean(ian_mean$height.diff)
mean(ian_mean$height.diff[ian_mean$height.dir == -1])
mean(ian_mean$height.diff[ian_mean$height.dir == 1])



p50 <- read.csv("data/P50/P50_data.csv")
p50_sub <- p50
p50_sub$SpeciesName <- paste0(gsub("^(..).*", "\\1", p50_sub$SpeciesName), 
                              gsub("^.* (..).*", "\\1", p50_sub$SpeciesName))
p50_sub$SpeciesName <- tolower(p50_sub$SpeciesName)
p50_sub <- p50_sub[p50_sub$SpeciesName %in% neil_sp & (grepl("Mpa", p50_sub$OrigUnitStr) | (grepl("MPa", p50_sub$OrigUnitStr))), ]
unique(p50_sub$SpeciesName)

