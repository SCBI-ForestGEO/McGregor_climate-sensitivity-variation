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

dbh_2018$stemID <- ifelse(is.na(heights$stemID), dbh_2013$stemID[match(paste(dbh_2018$tag, dbh_2018$stemtag), paste(dbh_2013$tag, dbh_2013$StemTag))], heights$stemID)



#create subsets and match dbh by stemID
heights_2013 <- heights[heights$height.year < 2018, ]
heights_2018 <- heights[heights$height.year == 2018, ]

heights_2013$dbh.cm <- dbh_2013$dbh.cm[match(heights_2013$stemID, dbh_2013$stemID)]
heights_2013$dbh_year <- 2013

heights_2018$dbh.cm <- dbh_2018$DBHcm[match(heights_2018$stemID, dbh_2018$stemID)]
heights_2018$dbh_year <- 2018

heights <- rbind(heights_2013, heights_2018)

#check which ones need dbh from previous census because they died
check <- heights[is.na(heights$dbh) | heights$dbh ==0, ]

heights$dbh.cm <- ifelse(heights$dbh.cm == 0 & heights$dbh_year == 2013, 
                      dbh_2008$dbh.cm[match(heights$stemID, dbh_2008$stemID)], 
                      ifelse(is.na(heights$dbh.cm) & heights$dbh_year == 2018, 
                             dbh_2013$dbh[match(heights$stemID, dbh_2013$stemID)], 
                              heights$dbh.cm))
#check again before moving on
check <- heights[is.na(heights$dbh) | heights$dbh ==0, ]

#get quadrat and coordinates
heights$quadrat <- dbh_2013$quadrat[match(heights$stemID, dbh_2013$stemID)]
heights <- heights[order(heights$quadrat, heights$tag), ]
dbh_2013$lx <- dbh_2013$gx - 20*((dbh_2013$quadrat %/% 100) - 1)
dbh_2013$ly <- dbh_2013$gy - 20*((dbh_2013$quadrat %% 100) - 1)
heights$lx <- dbh_2013$lx[match(heights$stemID, dbh_2013$stemID)]
heights$ly <- dbh_2013$ly[match(heights$stemID, dbh_2013$stemID)]

#round local coordinates to nearest tenth
heights$lx <- round(heights$lx, digits=1)
heights$ly <- round(heights$ly, digits=1)

#get current dbh and live/dead status from 2018
heights <- heights[c(1:3,10:12,4:9)]
heights$dbh_2018.cm <- dbh_2018$DBHcm[match(heights$stemID, dbh_2018$stemID)]
heights$status <- dbh_2018$Tree_Status[match(heights$stemID, dbh_2018$stemID)]

heights_check <- heights[c(1:8,10:14)]
#write.csv(heights_check, "data/heights_check.csv", row.names=FALSE)

#bring in list of cored species we're using
neil_list <- read.csv("data/core_list_for_neil.csv", stringsAsFactors = FALSE)
neil_sp <- unique(neil_list$sp)

paper_heights <- heights[heights$sp %in% neil_sp, ]
paper_heights <- paper_heights[complete.cases(paper_heights), ]
paper_heights <- paper_heights[order(paper_heights$sp), ]

unique(paper_heights$sp) #shows the sp that we have data for
paper_heights <- paper_heights[!paper_heights$sp %in% c("fram", "juni", "qual", "quve") & !paper_heights$dbh.cm == 0, ] #juni, fram, and quve have <10 measurements has only one measure and 0 dbh meant dead



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
ggplot(data = paper_heights, aes(x = log(dbh.cm), y = log(height.m), label = log(height.m))) +
  stat_smooth_func(geom="text",method="lm",hjust=0.16, vjust=-1.5,parse=TRUE) +
  geom_smooth(method="lm", se=FALSE, color="black") +
  geom_point(color = "#0c4c8a") +
  theme_minimal() +
  facet_wrap(vars(sp))


#equations for all species together
ggplot(data = paper_heights, aes(x = log(dbh.cm), y = log(height.m), label = log(height.m))) +
  stat_smooth_func(geom="text",method="lm",hjust=0.16, vjust=-1.5,parse=TRUE) +
  geom_smooth(method="lm", se=FALSE, color="black") +
  geom_point(color = "#0c4c8a") +
  theme_minimal()

############################################################################################
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
