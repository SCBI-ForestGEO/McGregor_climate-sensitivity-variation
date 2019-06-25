######################################################
# Purpose: Plot figure of time series for manuscript
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.5.1 (2018-07-02)
######################################################

# Clean environment ####
rm(list = ls())

# Set working directory as Shenandoah main folder ####
setwd(".")

# Load libraries ####


# set parameters ####
save.plots <- TRUE

# time series for each species ####

SPECIES_IN_ORDER <- c("LITU", "QUAL", "QURU", "QUVE", "QUPR", "FRAM", "CAGL", "JUNI", 
             "CATO", "CACO", "FAGR", "CAOV", "FRNI")

all_sss <- read.csv("https://raw.githubusercontent.com/SCBI-ForestGEO/climate_sensitivity_cores/master/results/SSS_as_a_function_of_the_number_of_trees_in_sample.csv")

sss.threshold = 0.75


colors.species <- colorRampPalette(c("purple", "cadetblue", "darkorange", "red", "violetred4"))(length(SPECIES_IN_ORDER))

clim <- read.csv("https://raw.githubusercontent.com/SCBI-ForestGEO/climate_sensitivity_cores/master/data/climate/Formated_CRU_SCBI_1901_2016.csv")

clim_PDSI <- read.csv("data/pdsi/pdsi_timeseries_winchester_1949-2012.csv", stringsAsFactors = F)
clim_PDSI$year <- sapply(strsplit(clim_PDSI$Date, "/"), "[[", 3)
clim_PDSI$month <- sapply(strsplit(clim_PDSI$Date, "/"), "[[", 1)
names(clim_PDSI) <- c("Date", "pdsi", "year", "month")

clim <- merge(clim, clim_PDSI[, -1], by = c("year", "month"), all.x = T)

clim <- clim[clim$month %in% c(5:7), c("year", "pre", "pet_sum", "pdsi")]
clim <- clim[clim$year <= 2009 & clim$year >= 1901,]

clim <- data.frame(apply(clim, 2, function(x) tapply(x, clim$year, mean)))

clim <- clim[clim$year >= 1959, ]
# drought_years <- clim$year[which(c(clim$pet_sum - clim$pre) >= (sort(clim$pet_sum - clim$pre, decreasing = T)[10]))]

# cbind(drought_years, c(clim$pet_sum - clim$pre)[which(c(clim$pet_sum - clim$pre) >= (sort(clim$pet_sum - clim$pre, decreasing = T)[10]))])


drought_years <- c(1966, 1977, 1999)
pre_drought_years <- drought_years - 5



if(save.plots)  {
  tiff(paste0("manuscript/tables_figures/Time_series_for_each_species.tif"), res = 300, width = 150, height = 150, units = "mm", pointsize = 10)
}

par(mfrow = c(length(SPECIES_IN_ORDER) + 3, 1), mar = c(0,0,0,0), oma = c(4, 6, 0, 0))


# pet_sum ####
plot(NULL,
     axes = F,
     ann = F, 
     xlim = c(1959,2020), ylim = c(100, 160))


rect(xleft = pre_drought_years, ybottom = par("usr")[3], 
     xright = drought_years, ytop = par("usr")[4],  col = "grey90", border = "transparent")
# abline(v = pre_drought_years, col = "grey", lty = 2) # seq(1959, 2000, by = 20)
abline(v = drought_years, col = "grey50", lty = 2) # seq(1959, 2000, by = 20)

lines(pet_sum ~ year, data = clim, col  = "red", lwd = 2)
text(x = 2010, y = 130, "PET", pos = 4, col = "red")

axis(2, at = c(110, 130, 150), las = 1)
# mtext(side = 2, text = expression("(mm mo"^-1*")"), las = 1, cex = 0.7, line = 3)

# pre ####
plot(NULL,
     axes = F,
     ann = F, 
     xlim = c(1959,2020), ylim = c(40, 180))


rect(xleft = pre_drought_years, ybottom = par("usr")[3], 
     xright = drought_years, ytop = par("usr")[4],  col = "grey90", border = "transparent")
# abline(v = pre_drought_years, col = "grey", lty = 2) # seq(1959, 2000, by = 20)
abline(v = drought_years, col = "grey50", lty = 2) # seq(1959, 2000, by = 20)

lines(pre ~ year, data = clim, col  = "blue",lwd = 2)

text(x = 2010, y = 100, "PRE", pos = 4, col = "blue")

axis(2, at = c(60, 110, 160), las = 1)

mtext(side = 2, text = expression("(mm mo"^-1*")"), line = 3, adj = 0)
# axis(1, at = c(1700, 2020), labels = F, tck = 0, col = "grey60")

# pdsi ####
plot(NULL,
     axes = F,
     ann = F, 
     xlim = c(1959,2020), ylim = c(-7, 5))


rect(xleft = pre_drought_years, ybottom = par("usr")[3], 
     xright = drought_years, ytop = par("usr")[4],  col = "grey90", border = "transparent")
# abline(v = pre_drought_years, col = "grey", lty = 2) # seq(1959, 2000, by = 20)
abline(v = drought_years, col = "grey50", lty = 2) # seq(1959, 2000, by = 20)

lines(pdsi ~ year, data = clim, col  = "black", lwd = 2)

text(x = 2010, y = 0, "PDSI", pos = 4, col = "black")

axis(2, at = c(-4, 0, 4), las = 1)

# mtext(side = 2, text = expression("(mm mo"^-1*")"), line = 3, adj = -0.5)
axis(1, at = c(1700, 2020), labels = F, tck = 0, col = "grey60")

# chronologies ####
for(f in SPECIES_IN_ORDER) {
  
  
  if (f %in% "CAOV") f <- "CAOVL"
  
  # get the detrended data
  core <- read.table(paste0("https://raw.githubusercontent.com/SCBI-ForestGEO/climate_sensitivity_cores/master/data/cores/", f, "/ARSTANfiles/", tolower(f), "_drop.rwl_tabs.txt"), sep = "\t", h = T)
  
  years.with.enough.sss <- all_sss[all_sss$Species %in% f & all_sss$sss >= sss.threshold, ]$Year
  
  years.with.enough.sss <- years.with.enough.sss[years.with.enough.sss >= 1959 & years.with.enough.sss <= 2009]
  
  core <- core[core$year %in% years.with.enough.sss, ] # trim to use only years for which with have clim data 
  
  if (f %in% "CAOVL") f <- "CAOV"
  
  plot(NULL,
       axes = F,
       ann = F, 
       xlim = c(1959,2020), ylim = c(0.5, 1.5))
  
  rect(xleft = pre_drought_years, ybottom = par("usr")[3], 
       xright = drought_years, ytop = par("usr")[4],  col = "grey90", border = "transparent")
  # abline(v = pre_drought_years, col = "grey", lty = 2) # seq(1959, 2000, by = 20)
  abline(v = drought_years, col = "grey50", lty = 2) # seq(1959, 2000, by = 20)
  
  lines(res ~ year, data = core,
        col = colors.species[which(SPECIES_IN_ORDER %in% f)])
  axis(2, at = c(0.7, 1, 1.3), las = 1)
  text(x = 2010, y = 1, f, pos = 4, col = colors.species[which(SPECIES_IN_ORDER %in% f)])
  
}
axis(1)
mtext(side = 1, "Year", outer = T, line = 2.5)
mtext(side = 2, "Ring Width Index", outer = T, line = 4)

if(save.plots) dev.off()
