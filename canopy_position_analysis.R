cru1901 <- read.csv("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/climate_sensitivity_cores/results/canopy_vs_subcanopy/1901_2009/tables/monthly_correlation/correlation_with_CRU_SCBI_1901_2016_climate_data.csv")

library(ggplot2)

#subset out caco, cato, and frni because they don't have pair of canopy and subcanopy
cru1901 <- cru1901[!(cru1901$Species %in% c("CACO_subcanopy", "CATO_subcanopy", "FRNI_subcanopy")), ]

cru1901_loop <- cru1901

#create separate identifier
cru1901_loop$position <- ifelse(grepl("subcanopy", cru1901$Species), "subcanopy", "canopy")
cru1901_loop$Species <- gsub("_[[:alpha:]]+$", "", cru1901$Species)


setwd("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/tree-growth-and-traits")
cru1901_loop$variable <- as.character(cru1901_loop$variable)
clim <- unique(cru1901_loop$variable)


pdf("Canopy_subcanopy_correlation.pdf", width=10)

#creates a lattice graph showing box plot of variables grouped by species
ggplot(data = cru1901) +
  aes(x = Species, y = coef, fill = variable) +
  geom_boxplot() +
  labs(title = "Correlation by species and variable",
       y = "Correlation") +
  facet_wrap( ~ Species, scales="free", nrow=4) +
  theme_minimal()

for (j in seq(along=clim)){
    cru1901_sub <- cru1901_loop[cru1901_loop$variable %in% clim[[j]], ]
    
    q <- ggplot(data = cru1901_sub) +
      aes(x = Species, y = coef, fill = position) +
      geom_boxplot() +
      labs(title = paste0("Canopy vs subcanopy: ", clim[[j]]),
           y = "Correlation") +
      facet_wrap( ~ Species, scales="free", nrow=4) +
      theme_minimal()
    
    print(q)
}

dev.off()

#next step is to run a t-test in the loop, then make it appear on the graphs as a label.



plot(cru1901$Species, cru1901$coef)

library(car)
scatterplot(cru1901$Species, cru1901$coef, groups=cru1901$variable)

library(lattice)
splom(cru1901[2], groups=cru1901$Species, data=cru1901)            
View(mtcars)

attach(cru1901)
bwplot(coef~Species|variable)
