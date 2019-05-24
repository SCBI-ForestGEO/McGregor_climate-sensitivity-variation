######################################################
# Purpose: Calculate heights of different species in ForestGEO from regression equations
# Developed by: Ian McGregor - mcgregori@si.edu
# R version 3.5.3 - First created April 2019
######################################################

library(data.table)
library(RCurl)
library(ggplot2)
library(devtools)

heights <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_dimensions/tree_heights/SCBI_tree_heights.csv"), stringsAsFactors = FALSE)

heights <- heights[,c(1:3,5:6,13)]

heights_dup <- heights[duplicated(heights$tag), ]
dup <- heights_dup$tag

heights_dup <- heights[heights$tag %in% dup, ]
heights_dup <- heights_dup[order(heights_dup$tag), ]

setnames(heights, old="species.code", new="sp")

dbh_2008 <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/scbi.stem1.csv"))
dbh_2008$dbh.cm <- dbh_2008$dbh/10 #cm

dbh_2013 <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/scbi.stem2.csv"))
dbh_2013$dbh.cm <- dbh_2013$dbh/10 #cm

dbh_2018 <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/scbi.stem3_TEMPORARY.csv"), stringsAsFactors = FALSE)

#get stemIDs for each stem
heights$stemID <- dbh_2013$stemID[match(paste(heights$tag, heights$stemtag), paste(dbh_2013$tag, dbh_2013$StemTag))]

dbh_2018$tag <- gsub("_.*$", "", dbh_2018$Tree_ID_Num)
dbh_2018$stemtag <- gsub("[[:digit:]]*_", "", dbh_2018$Tree_ID_Num)
dbh_2018$tag <- as.numeric(as.character(dbh_2018$tag))
dbh_2018$stemtag <- as.numeric(as.character(dbh_2018$stemtag))

dbh_2018$stemID <- dbh_2013$stemID[match(paste(dbh_2018$tag, dbh_2018$stemtag), paste(dbh_2013$tag, dbh_2013$StemTag))]

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

