######################################################
# Purpose: Create conglomerate graphs for McGregor et al 2019
# Developed by: Ian McGregor - mcgregori@si.edu
# R version 3.5.3 - First created July 2019
######################################################
library(ggplot2) #2
library(RCurl) #2
library(tidyr) #2
library(grid) #2
library(gridExtra) #2

#1 NEON vertical height profiles
source('scripts/vertical_height_neon.R', echo=TRUE)
#########################################################################
#2 height by crown position in 2018 ####
trees_all <- read.csv("manuscript/tables_figures/trees_all.csv", stringsAsFactors = FALSE)

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
current_ht <- current_ht[order(current_ht$tree, current_ht$year), ]

#graphing height by crown position (for paper)
current_ht <- current_ht[!is.na(current_ht$position_all), ]
current_ht$position_all <- factor(current_ht$position_all, levels = c("dominant", "co-dominant", "intermediate", "suppressed"))

heights <-
  ggplot(data = current_ht) +
  aes(x = position_all, y = height.m, group = position_all) +
  # aes(x=position_all, y=height.m, fill=year) +
  geom_boxplot() +
  ggtitle("Current height vs crown position")+
  xlab("crown position") +
  ylab("height(m)") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), limits=c(0,60)) +
  theme_minimal()
  

