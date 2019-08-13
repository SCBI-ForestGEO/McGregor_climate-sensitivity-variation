#tables for manuscript

#Table 1 was originally made as .xslx ####
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
table2[,c(9:12)] <- round(table2[,c(9:12)], 2)

#bring in mean DBH and DBH range for each species

dbh_cored <- species[,c(1:9)]

dbh_cored <- dbh_cored %>%
  group_by(sp) %>%
  summarise(mean = mean(dbh2018),
            range = max(dbh2018) - min(dbh2018))
dbh_cored$mean <- round(dbh_cored$mean, 2)

table2 <- merge(dbh_cored, table2, by = "sp")

setnames(table2, old=c("mean", "range", "rp", "PLA_dry_percent", "LMA_g_per_m2", "mean_TLP_Mpa", "WD_g_per_cm3"), new=c("mean_DBH", "range_DBH", "RP", "PLA", "LMA", "TLP", "WD"))

table2_SI <- table2[,c(4:9)]
table2 <- table2[,-c(4:9)]

write.csv(table2, "manuscript/tables_figures/Table2.csv", row.names=FALSE)
write.csv(table2_SI, "manuscript/tables_figures/TableS2.csv", row.names=FALSE)

#Table 3 was originally made as .xlsx from tested_traits_all.csv ####

