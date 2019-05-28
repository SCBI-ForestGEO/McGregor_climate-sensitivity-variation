######################################################
# Purpose: Calculate heights of different species in ForestGEO from regression equations
# Developed by: Ian McGregor - mcgregori@si.edu
# R version 3.5.3 - First created April 2019
######################################################

library(data.table)
library(RCurl)
library(ggplot2)
library(devtools)

# create regression equations ####
heights <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_dimensions/tree_heights/SCBI_tree_heights.csv"), stringsAsFactors = FALSE)

dbh_2008 <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/scbi.stem1.csv"))
dbh_2008$dbh.cm <- dbh_2008$dbh/10 #cm

dbh_2013 <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/scbi.stem2.csv"))
dbh_2013$dbh.cm <- dbh_2013$dbh/10 #cm

dbh_2018 <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/scbi.stem3_TEMPORARY.csv"), stringsAsFactors = FALSE)

# #get stemIDs for each stem (leaving this here if need be)
# heights$stemID <- ifelse(is.na(heights$stemID), dbh_2013$stemID[match(paste(heights$tag, heights$stemtag), paste(dbh_2013$tag, dbh_2013$StemTag))], heights$stemID)
# 
# heights$tag <- ifelse(is.na(heights$tag), dbh_2013$tag[match(heights$stemID, dbh_2013$stemID)], heights$tag)



#separate stemID in 2018 data
dbh_2018$tag <- gsub("_.*$", "", dbh_2018$Tree_ID_Num)
dbh_2018$stemtag <- gsub("[[:digit:]]*_", "", dbh_2018$Tree_ID_Num)
dbh_2018$tag <- as.numeric(as.character(dbh_2018$tag))
dbh_2018$stemtag <- as.numeric(as.character(dbh_2018$stemtag))

#create subsets and match dbh by stemID
# heights_2013 <- heights[heights$height.year < 2018, ]
# heights_2018 <- heights[heights$height.year >= 2018, ]
# 
# heights_2013$dbh.cm <- dbh_2013$dbh.cm[match(heights_2013$stemID, dbh_2013$stemID)]
# heights_2013$dbh_year <- 2013
# 
# heights_2018$dbh.cm <- dbh_2018$DBHcm[match(heights_2018$stemID, dbh_2018$stemID)]
# heights_2018$dbh_year <- 2018
# 
# heights <- rbind(heights_2013, heights_2018)

heights$dbh_regr.cm <- paste0(heights$DBH.2008.cm, heights$DBH.2013.cm, heights$DBH.TLS.2015.cm, heights$DBH.2018.cm)
heights$dbh_regr.cm <- gsub("NA", "", heights$dbh_regr.cm)
heights$dbh_regr.cm <- as.numeric(heights$dbh_regr.cm)

heights$dbh_year <- ifelse(heights$dbh_regr.cm %in% heights$DBH.2008.cm, 2008,
                     ifelse(heights$dbh_regr.cm %in% heights$DBH.2013.cm, 2013,
                      ifelse(heights$dbh_regr.cm %in% heights$DBH.TLS.2015.cm, 2015,
                      ifelse(heights$dbh_regr.cm %in% heights$DBH.2018.cm, 2018,
                                 heights$dbh_year))))

heights_regr <- heights[c(1:4,6,8,15:17,24:25)]

#check which ones need dbh from previous census because they died
check <- heights_regr[is.na(heights_regr$dbh) | heights_regr$dbh ==0, ]

# heights_regr$dbh.cm <- ifelse(heights_regr$dbh.cm == 0 & heights_regr$dbh_year == 2013, 
#                       dbh_2008$dbh.cm[match(heights_regr$stemID, dbh_2008$stemID)], 
#                       ifelse(is.na(heights_regr$dbh.cm) & heights_regr$dbh_year == 2018, 
#                              dbh_2013$dbh[match(heights_regr$stemID, dbh_2013$stemID)], 
#                               heights_regr$dbh.cm))
#check again before moving on
check <- heights_regr[is.na(heights_regr$dbh) | heights_regr$dbh ==0, ]

#get quadrat and coordinates for field data ####
heights_regr$quadrat <- dbh_2013$quadrat[match(heights_regr$stemID, dbh_2013$stemID)]
heights_regr <- heights_regr[order(heights_regr$quadrat, heights_regr$tag), ]
dbh_2013$lx <- dbh_2013$gx - 20*((dbh_2013$quadrat %/% 100) - 1)
dbh_2013$ly <- dbh_2013$gy - 20*((dbh_2013$quadrat %% 100) - 1)
heights_regr$lx <- dbh_2013$lx[match(heights_regr$stemID, dbh_2013$stemID)]
heights_regr$ly <- dbh_2013$ly[match(heights_regr$stemID, dbh_2013$stemID)]

#round local coordinates to nearest tenth
heights_regr$lx <- round(heights_regr$lx, digits=1)
heights_regr$ly <- round(heights_regr$ly, digits=1)

#get current dbh and live/dead status from 2018
heights_regr <- heights_regr[c(1:3,10:12,4:9)]
heights_regr$dbh_2018.cm <- dbh_2018$DBHcm[match(heights_regr$stemID, dbh_2018$stemID)]
heights_regr$status <- dbh_2018$Tree_Status[match(heights_regr$stemID, dbh_2018$stemID)]

heights_regr_check <- heights_regr[c(1:8,10:14)]
write.csv(heights_regr_check, "data/heights_regr_check.csv", row.names=FALSE)

#make regression equations ####
#bring in list of cored species we're using
neil_list <- read.csv("data/core_list_for_neil.csv", stringsAsFactors = FALSE)
neil_sp <- unique(neil_list$sp)

paper_heights <- heights_regr[heights_regr$sp %in% neil_sp, ]
paper_heights <- paper_heights[order(paper_heights$sp), ]

unique(heights_regr$sp) #shows all sp that we have height data for
unique(paper_heights$sp) #shows the cored sp that we have data for
paper_heights <- paper_heights[!paper_heights$sp %in% c("fram", "juni", "quve"), ] #juni, fram, and quve have <5 measurements has only one measure

test <- paper_heights[paper_heights$method != "manual", ]

# library(dplyr)
# max_ht <- paper_heights %>% 
#   group_by(sp) %>% 
#   summarise(height.m = max(height.m))
# min_ht <- paper_heights %>% 
#   group_by(sp) %>% 
#   summarise(height.m = min(height.m))
# paper_heights$max_ht <- max_ht$height.m[match(paper_heights$sp, max_ht$sp)]
# paper_heights$min_ht <- min_ht$height.m[match(paper_heights$sp, min_ht$sp)]



#DBH IS IN CM, HEIGHT IS IN M

source_gist("524eade46135f6348140")
ggplot(data = paper_heights, aes(x = log(dbh_regr.cm), y = log(height.m), label = log(height.m))) +
  stat_smooth_func(geom="text",method="lm",hjust=0.16, vjust=-1.5,parse=TRUE) +
  geom_smooth(method="lm", se=FALSE, color="black") +
  geom_point(color = "#0c4c8a") +
  theme_minimal() +
  facet_wrap(vars(sp))


#equations for all species together
ggplot(data = paper_heights, aes(x = log(dbh_regr.cm), y = log(height.m), label = log(height.m))) +
  stat_smooth_func(geom="text",method="lm",hjust=0.16, vjust=-1.5,parse=TRUE) +
  geom_smooth(method="lm", se=FALSE, color="black") +
  geom_point(color = "#0c4c8a") +
  theme_minimal()

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
