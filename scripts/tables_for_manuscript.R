#tables for manuscript

#Table 3 ####
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
table1 <- table1[!table1$sp %in% c("frni", "pist"), ]

#bring in trait values
trees_all <- read.csv("manuscript/tables_figures/trees_all.csv", stringsAsFactors = FALSE)

traits <- trees_all[,c(4:9)]
traits <- traits[!duplicated(traits$sp), ]

table2 <- merge(table2, traits, by="sp")

#bring in mean DBH and DBH range for each species

dbh_cored <- species[,c(1:9)]

dbh_cored <- dbh_cored %>%
  group_by(sp) %>%
  summarise(mean = mean(dbh2018),
            min = min(dbh2018),
            max = max(dbh2018))
dbh_cored$mean <- round(dbh_cored$mean, 2)
dbh_cored$range <- paste0(dbh_cored$min, " - ", dbh_cored$max)
dbh_cored[,c("min", "max")] <- NULL

dbh_cored <- dbh_cored[dbh_cored$sp != c("frni", "pist"), ]

table2 <- merge(dbh_cored, table2, by = "sp")
table2_SI <- table2[,c(5:9)]
table2 <- table2[,-c(5:9)]

table2[,c(6:9)] <- round(table2[,c(6:9)], 2)

setnames(table2, 
         old=c("mean", "range", "n_cores", "rp", "PLA_dry_percent", "LMA_g_per_m2", "mean_TLP_Mpa", "WD_g_per_cm3"), 
         new=c("mean $DBH$ (cm)", "range $DBH$ (cm)", "n.cores", "$RP$", "$PLA$ (\\%)", "$LMA$ ($\\frac{g}{cm^2}$)", "$\\pi_{tlp}$ (Mpa)", "$WD$ ($\\frac{g}{cm^3}$)"))

#i think this originally came from one of valentine's tables
table2$percent.ANPP <- c(2, 3.7, 1.1,
                         2, 1.5, 3.8,
                         2.1, 47.1, 10.7,
                         4.8, 10.1, 7.8)

table2$sp <- c("Carya cordiformis (CACO)", "Carya glabra (CAGL)", "Carya ovalis (CAOVL)",
               "Carya tomentosa (CATO)", "Fagus grandifolia (FAGR)", "Fraxinus americana (FRAM)	",
               "Juglans nigra (JUNI)", "Liriodendron tulipifera (LITU)", "Quercus alba (QUAL)", 
               "Quercus montana (QUPR)", "Quercus rubra (QURU)", "Quercus velutina (QUVE)")

table2 <- table2[,c(1,10,4,2:3,5:9)]
table2 <- table2[base::order(table2$percent.ANPP, decreasing=TRUE), ]
table2[8,6] <- "semi-ring*" #change juni ring porosity


write.csv(table2, "manuscript/tables_figures/table3_species_table.csv", row.names=FALSE)
write.csv(table2_SI, "manuscript/tables_figures/TableS2.csv", row.names=FALSE)


