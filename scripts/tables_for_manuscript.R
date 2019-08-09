#tables for manuscript

#Table 1 was originally made as .xslx
#Table 3 was originally made as .xlsx from tested_traits_all.csv

#Table 2 ####
library(RCurl)
library(tidyr)
library(data.table)
library(dplyr)

species <- read.csv("data/core_list_for_neil.csv", stringsAsFactors = FALSE)

table <- species[, c(1:3,6,17)]

positions <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_dimensions/tree_crowns/cored_dendroband_crown_position_data/dendro_cored_full.csv"))

table$position_all <- positions$crown.position[match(table$tag, positions$tag)]
table$position_all <- gsub("D", "dominant", table$position_all)
table$position_all <- gsub("C", "co-dominant", table$position_all)
table$position_all <- gsub("I", "intermediate", table$position_all)
table$position_all <- gsub("S", "suppressed", table$position_all)

table1 <- table %>%
  group_by(sp) %>%
  summarize(n_cores=n())

table2 <- table %>%
  group_by(sp, position_all) %>%
  summarize(count=n())

table2 <- spread(table2, key=position_all, value=count)
setnames(table2, old="<NA>", new="prior dead")

table2$n_cores <- table1$n_cores[match(table2$sp, table1$sp)]
table2 <- table2[, c(1,7,3,2,4:6)]
table2 <- table2[!table2$sp %in% c("frni", "pist"), ]

#bring in trait values
trees_all <- read.csv("manuscript/tables_figures/trees_all.csv", stringsAsFactors = FALSE)

traits <- trees_all[,c(2,6:10)]
traits <- traits[!duplicated(traits$sp), ]

table2 <- merge(table2, traits, by="sp")
table2[,c(8:11)] <- round(table2[,c(8:11)], 2)

#bring in mean DBH and DBH range for each species

dbh_cored <- species[,c(1:9)]

detach("package:tidyr", unload = TRUE)

aggregate(dbh_cored[,c(2,9)], by = list(dbh_cored$sp), "mean")

dbh_cored2 <- dbh_cored %>%
  group_by(sp) %>%
  summarise(mean = mean(dbh2018)) %>%
  summarise(range = max(dbh2018) - min(dbh2018))



trees_all <- read.csv("tables_figures/trees_all.csv", stringsAsFactors = FALSE)

trees_all <- trees_all[!duplicated(trees_all$sp), ]


traits_table <- data.frame(
  "Trait" = c("Ring Porosity", "Percent Leaf Area", "Leaf Mass Area", "Wood density", "TLP"),
  "Unit" = c("ring, semi-ring, diffuse", "%", "g/m2", "g/cm3", "MPa"))
traits_table$abb <- c("RP", "PLA", "LMA", "WD", "TLP")

traits_table$mean <- NA
traits_table$min <- NA
traits_table$max <- NA

for (i in seq(along=traits_table$abb[2:5])){
  trait <- traits_table$abb[2:5][i]
  
  sub <- trees_all[grepl(trait, colnames(trees_all))]
  traits_table$mean[2:5][i] <- ifelse(grepl(trait, colnames(sub)), 
                                      mean(sub[, 1], na.rm=TRUE), 
                                      traits_table$mean)
  
  traits_table$min[2:5][i] <- ifelse(grepl(trait, colnames(sub)), 
                                     min(sub[, 1], na.rm=TRUE), 
                                     traits_table$min)
  
  traits_table$max[2:5][i] <- ifelse(grepl(trait, colnames(sub)), 
                                     max(sub[, 1], na.rm=TRUE), 
                                     traits_table$max)
}

traits_table[, c("mean", "min", "max")] <- 
  sapply(traits_table[, c("mean", "min", "max")], function(x)
    round(x, 2)
  )
