###############################################################
# Purpose: Other tables for McGregor et al 2020
# Developed by Ian McGregor, contact Anderson-Teixeira (teixeirak@si.edu)
# First created November 2019
################################################################
#Table 2 ####

# necessary packages
library(RCurl)
library(tidyr)
library(data.table)
library(dplyr)

species <- read.csv("data/core_files/core_list.csv", stringsAsFactors = FALSE)

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
  summarise(mean = signif(mean(dbh2018/10),3),
            min = signif(min(dbh2018/10),3),
            max = signif(max(dbh2018/10),3))
dbh_cored$mean <- round(dbh_cored$mean, 2)
dbh_cored$range <- paste0(dbh_cored$min, " - ", dbh_cored$max)
dbh_cored[,c("min", "max")] <- NULL

dbh_cored <- dbh_cored[dbh_cored$sp != c("frni", "pist"), ]

table2 <- merge(dbh_cored, table2, by = "sp")
table2_SI <- table2[,c(5:9)]
table2 <- table2[,-c(5:9)]

table2[,c(6:9)] <- round(table2[,c(6:9)], 2)

#bring in hydraulic traits to get std.error
traits_hydr <- fread("https://raw.githubusercontent.com/EcoClimLab/HydraulicTraits/master/data/SCBI/processed_trait_data/SCBI_all_traits_table_indvidual_level.csv?token=AJNRBEPQVCRRCIDQSBGNC2S7EC5A2")

library(plotrix)
traits_hydr <- traits_hydr[!(sp %in% c("frni", "pist")),]
newtraits <- traits_hydr[,.(tlp_se = std.error(mean_TLP_Mpa, na.rm=TRUE),
                            pla_se = std.error(PLA_dry_percent, na.rm=TRUE),
                            lma_se=std.error(LMA_g_per_m2, na.rm=TRUE),
                            wd_se=std.error(WD_g_per_cm3, na.rm=TRUE)),
                         by=.(sp)
                         ][order(sp), ]
traits_nosp <- newtraits[,sp := NULL]
traits_nosp <- traits_nosp[, names(traits_nosp) := 
                         lapply(.SD, signif, digits=3)]

table2 <- cbind(table2, traits_nosp)
#i think this originally came from one of valentine's tables
table2$percent.ANPP <- c(2, 3.7, 1.1,
                         2, 1.5, 3.8,
                         2.1, 47.1, 10.7,
                         4.8, 10.1, 7.8)
table2 <- table2[,c("sp", "percent.ANPP", "n_cores", "mean", "range",
                    "WD_g_per_cm3", "wd_se", "LMA_g_per_m2", "lma_se",
                    "rp", "mean_TLP_Mpa", "tlp_se", 
                    "PLA_dry_percent", "pla_se")]

roundcols <- c("WD_g_per_cm3", "wd_se", "LMA_g_per_m2", "lma_se",
               "mean_TLP_Mpa", "tlp_se", 
               "PLA_dry_percent", "pla_se")
table2[,roundcols] <- signif(table2[,roundcols], digits=3)

table2 <- as.data.table(table2)





# #get standard deviation
# org_traits <- fread("https://raw.githubusercontent.com/EcoClimLab/HydraulicTraits/master/data/SCBI/processed_trait_data/SCBI_all_traits_table_species_level.csv?token=AJNRBEKXLSB4H62QPT3RZQC7DWCPE", stringsAsFactors = FALSE)
# 
# table2$`$PLA$ (\\%)_sd` <- org_traits$PLA_dry_percent_sd[match(table2$sp, org_traits$sp)]
# table2$`$LMA$ ($\frac{g}{cm^2}$)_sd` <- org_traits$LMA_g_per_m2_sd[match(table2$sp, org_traits$sp)]
# table2$`$\\pi_{tlp}$ (Mpa)_sd` <- org_traits$mean_TLP_Mpa_sd[match(table2$sp, org_traits$sp)]
# table2$`$WD$ ($\frac{g}{cm^3}$)_sd` <- org_traits$WD_g_per_cm3_sd[match(table2$sp, org_traits$sp)]


table2$sp <- c("Carya cordiformis (CACO)", "Carya glabra (CAGL)", "Carya ovalis (CAOVL)",
               "Carya tomentosa (CATO)", "Fagus grandifolia (FAGR)", "Fraxinus americana (FRAM)	",
               "Juglans nigra (JUNI)", "Liriodendron tulipifera (LITU)", "Quercus alba (QUAL)", 
               "Quercus montana (QUPR)", "Quercus rubra (QURU)", "Quercus velutina (QUVE)")


table2 <- table2[base::order(table2$percent.ANPP, decreasing=TRUE), ]
table2[8,10] <- "semi-ring*" #change juni ring porosity





write.csv(table2, "manuscript/tables_figures/publication/table2_species_table.csv", row.names=FALSE)
# write.csv(table2_SI, "manuscript/tables_figures/publication/TableS2.csv", row.names=FALSE)


