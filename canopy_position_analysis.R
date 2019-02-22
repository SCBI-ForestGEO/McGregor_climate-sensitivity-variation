#canopy position analysis from tree cores

cru1901 <- read.csv("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/climate_sensitivity_cores/results/canopy_vs_subcanopy/1901_2009/tables/monthly_correlation/correlation_with_CRU_SCBI_1901_2016_climate_data.csv")

library(ggplot2)
library(ggpubr)

#subset out caco, cato, and frni because they don't have pair of canopy and subcanopy
cru1901 <- cru1901[!(cru1901$Species %in% c("CACO_subcanopy", "CATO_subcanopy", "FRNI_subcanopy")), ]

cru1901_loop <- cru1901

#create separate identifier
cru1901_loop$position <- ifelse(grepl("subcanopy", cru1901$Species), "subcanopy", "canopy")
cru1901_loop$Species <- gsub("_[[:alpha:]]+$", "", cru1901$Species)


setwd("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/tree-growth-and-traits")
cru1901_loop$variable <- as.character(cru1901_loop$variable)
clim <- unique(cru1901_loop$variable)
species <- unique(cru1901_loop$Species)
months <- c("curr.may", "curr.jun", "curr.jul", "curr.aug")

pdf("Canopy_subcanopy_correlation.pdf", width=10)

#creates a lattice graph showing box plot of variables grouped by species
ggplot(data = cru1901) +
  aes(x = Species, y = coef, fill = variable) +
  geom_boxplot() +
  labs(title = "Correlation by species and variable",
       y = "Correlation") +
  facet_wrap( ~ Species, scales="free", nrow=4) +
  theme_minimal()

cru1901_loop$Species <- as.factor(cru1901_loop$Species)

for (j in seq(along=clim)){
      cru1901_sub <- cru1901_loop[cru1901_loop$variable %in% clim[[j]] & cru1901_loop$month %in% months, ]
      cru1901_sub <- group_by(cru1901_sub, Species)
    
      q <- ggplot(data = cru1901_sub) +
        geom_boxplot(aes(x = position, y = coef, fill = position)) +
        labs(title = paste0("Canopy vs subcanopy: ", clim[[j]]),
             y = "Correlation") +
        stat_compare_means(aes(x=position, y=coef), method="t.test", label.x.npc = 0, label.y.npc = 0.97) +
        facet_wrap( ~ Species, scales="free", nrow=4) +
        theme_minimal()
      print(q)
}

dev.off()



       
