######################################################
# Purpose: Create conglomerate graphs for McGregor et al 2019
# Developed by: Ian McGregor - mcgregori@si.edu
# R version 3.5.3 - First created July 2019
######################################################
library(ggplot2)

#1. NEON vertical height profiles
source('scripts/vertical_height_neon.R', echo=TRUE)
NEON_list <- list(wind_plot, RH_plot, SAAT_plot) #,"biotemp_plot")
names(NEON_list) <- c("wind_plot", "RH_plot", "SAAT_plot") #,"biotemp_plot
#########################################################################
#2 height by crown position in 2018 ####
library(RCurl) #2
library(tidyr) #2
library(grid) #2
library(gridExtra) #2
library(data.table)

##2a. heights for all cored trees ####
trees_all <- read.csv("manuscript/tables_figures/trees_all.csv", stringsAsFactors = FALSE) #these graphs are meant to be for all cored trees, not just the ones being used in analysis

scbi.stem3 <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/scbi.stem3.csv"), stringsAsFactors = FALSE)

scbi.stem3$dbh <- as.numeric(scbi.stem3$dbh)

current_ht <- trees_all[!duplicated(trees_all$tree), ]
current_ht$year <- 2018
current_ht <- current_ht[,c(1:4,17:19)]

current_ht$dbh_old.mm <- scbi.stem3$dbh[match(current_ht$tree, scbi.stem3$tag)]
current_ht$dbh_old.cm <- current_ht$dbh_old.mm/10
current_ht$year_dbh <- ifelse(!is.na(current_ht$dbh_old.cm), 2018, NA)

#get original dbh if they died before 2018 (only need 2013)
scbi.stem2 <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/scbi.stem2.csv"), stringsAsFactors = FALSE)

current_ht$dbh_old.mm <- ifelse(is.na(current_ht$dbh_old.mm),
                                scbi.stem2$dbh[match(current_ht$tree, scbi.stem2$tag)],
                                current_ht$dbh_old.mm)
current_ht$dbh_old.cm <- ifelse(is.na(current_ht$dbh_old.cm),
                                current_ht$dbh_old.mm/10,
                                current_ht$dbh_old.cm)
current_ht$year_dbh <- ifelse(is.na(current_ht$year_dbh), 2013, current_ht$year_dbh)

#get the log dbh and get heights
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
current_ht$position_all_abb <- substring(current_ht$position_all, 0, 1)
current_ht$position_all_abb <- toupper(current_ht$position_all_abb)
current_ht$position_all_abb <- factor(current_ht$position_all_abb, levels = c("D", "C", "I", "S"))


heights <-
  ggplot(data = current_ht, aes(x = position_all_abb, y = height.m, group = position_all)) +
  # aes(x=position_all, y=height.m, fill=year) +
  geom_boxplot() +
  xlab("Crown position") +
  ylab("Height [m]") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), limits=c(0,60)) +
  theme_minimal() +
  theme(axis.text = element_text(size=12)) +
  theme(axis.title = element_text(size=14))

##2b. get height data for all trees >10cm dbh in census ####
scbi.stem3 <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/scbi.stem3.csv"), stringsAsFactors = FALSE)

scbi.stem3$dbh <- as.numeric(scbi.stem3$dbh)
scbi.stem3$dbh <- ifelse(is.na(scbi.stem3$dbh), 0, scbi.stem3$dbh)

scbi18_ht <- scbi.stem3
scbi18_ht <- scbi18_ht[scbi18_ht$dbh>=100, ]
scbi18_ht <- scbi18_ht[,c(2:5,11,14:15)]

scbi18_ht$dbh_old.cm <- scbi18_ht$dbh/10
scbi18_ht$dbh.ln.cm <- log(scbi18_ht$dbh_old.cm)

#linear log-log regression
scbi18_ht$height.ln.m <- 
   ifelse(scbi18_ht$sp == "caco", (0.348+0.808*scbi18_ht$dbh.ln.cm),
   ifelse(scbi18_ht$sp == "cagl", (0.681+0.704*scbi18_ht$dbh.ln.cm),
   ifelse(scbi18_ht$sp == "caovl", (0.621+0.722*scbi18_ht$dbh.ln.cm),
   ifelse(scbi18_ht$sp == "cato", (0.776+0.701*scbi18_ht$dbh.ln.cm),
   ifelse(scbi18_ht$sp == "fagr", (0.708+0.662*scbi18_ht$dbh.ln.cm),
    ifelse(scbi18_ht$sp == "fram", (0.715+0.619*scbi18_ht$dbh.ln.cm),
    ifelse(scbi18_ht$sp == "juni", (1.22+0.49*scbi18_ht$dbh.ln.cm),
    ifelse(scbi18_ht$sp == "litu", (1.32+0.524*scbi18_ht$dbh.ln.cm),
    ifelse(scbi18_ht$sp == "qual", (1.14+0.548*scbi18_ht$dbh.ln.cm),
    ifelse(scbi18_ht$sp == "qupr", (0.44+0.751*scbi18_ht$dbh.ln.cm),
    ifelse(scbi18_ht$sp == "quru", (1.17+0.533*scbi18_ht$dbh.ln.cm),
    ifelse(scbi18_ht$sp == "quve", (0.864+0.585*scbi18_ht$dbh.ln.cm),
                                  (0.791+0.645*scbi18_ht$dbh.ln.cm)))))))))))))

scbi18_ht$height.m <- exp(scbi18_ht$height.ln.m) #used below in #4

##2c. combine heights to make plot with all years ####

trees_all$position_all_abb <- ifelse(trees_all$position_all == "dominant", "D",
                                     ifelse(trees_all$position_all == "co-dominant", "C",
                                            ifelse(trees_all$position_all == "suppressed", "S", "I")))
trees_all_plot <- trees_all[,c("tree", "year", "position_all_abb", "height.m")]

current_ht$position_all_abb <- ifelse(current_ht$position_all == "dominant", "D",
                                     ifelse(current_ht$position_all == "co-dominant", "C",
                                            ifelse(current_ht$position_all == "suppressed", "S", "I")))
current_ht_sub <- current_ht[,c("tree", "year", "position_all_abb", "height.m")]

png("manuscript/tables_figures/height_plot_analysis.png")
#NOTE notice diff when using current_ht_sub versus...
##current_ht_sub = log-derived 2018 heights of only trees in trees_all
heights_allplot <- rbind(trees_all_plot, current_ht_sub)
heights_allplot$position_all_abb <- factor(heights_allplot$position_all_abb, levels=c("D","C","I","S"))
heights_allplot$year <- as.character(heights_allplot$year)

ggplot(na.omit(heights_allplot), aes(position_all_abb, height.m)) +
   geom_boxplot(aes(fill=year)) +
   xlab("Crown position") +
   ylab("Height [m]") +
   ggtitle("Height comparison with only trees in analysis")
dev.off()

#######################################################################################
#3 Get PLA and TLP as function of TWI and height ####
library(ggstance)
library(raster)
library(dplyr)
library(Hmisc)
library(magrittr)

##3a. get TWI values for all trees in census, add trait values ####
scbi.stem3$dbh <- ifelse(is.na(scbi.stem3$dbh), 0, scbi.stem3$dbh)
scbi.stem3 <- scbi.stem3[scbi.stem3$dbh >= 100, ] #>10cm dbh

#get utm coords
source('E:/Github_SCBI/SCBI-ForestGEO-Data/R_scripts/SIGEO_plot_grid_UTMcoord.R', echo=TRUE)

plot_to_UTM(scbi.stem3)
scbi18 <- sigeo_coords
scbi18 <- scbi18[, c(2:5,20:21)]
scbi18_x <- scbi18[, c(5:6)]
topo <- raster("data/plot_TWI.tif")
twi <- raster::extract(topo, scbi18_x, method="simple")
scbi18$TWI <- twi
scbi18$TWI.ln <- log(scbi18$TWI)

#add in leaf traits (no rp because it's categorical)
leaf_traits <- trees_all[, c(4,6:8)]

for (i in seq(along=2:ncol(leaf_traits))){
   trait <- colnames(leaf_traits[2:ncol(leaf_traits)])
   scbi18[, trait[[i]]] <- leaf_traits[, trait[[i]]][match(scbi18$sp, leaf_traits$sp)]
}

##3b. create height groupings and graphs (Figure S1) ####
scbi18$height.m <- scbi18_ht$height.m[match(scbi18$stemID, scbi18_ht$stemID)]


scbi18$bins <- cut2(scbi18$height.m, g=19)
scbi18$bins_TWI <- cut2(scbi18$TWI, g=20)

type = c("tlp_ht", "pla_ht", "tlp_twi", "pla_twi")
var = c("mean_TLP_Mpa", "PLA_dry_percent", "mean_TLP_Mpa", "PLA_dry_percent")

breaks_tlp = c(-2.8,-2.6,-2.4,-2.2,-2.0,-1.8)
breaks_pla = c(8,12,16,20,24)

limits_tlp = c(-2.8,-1.8)
limits_pla = c(8,26)

for (i in seq(along=1:4)){
   if(!grepl("twi", type[[i]])){
      df <- scbi18 %>%
         group_by(bins) %>%
         dplyr::summarize(avg = mean(eval(parse(text = var[i])), na.rm=TRUE),
                          dev = sd(eval(parse(text = var[i])), na.rm=TRUE),
                          no_data = (sum(is.na(eval(parse(text = var[i]))))/length(eval(parse(text = var[i])))))
      
      df$sdmin <- df$avg - df$dev
      df$sdmax <- df$avg + df$dev
      df$col <- ifelse(df$no_data > 0.25, "grey", "black")
      df$lab <- seq(5,95,by=5) #percentiles
      df$num <- gsub("^.*,", "", df$bins) #upper bound of bin
      df$num <- gsub("[[:punct:]]$", "", df$num)
      df$num <- as.numeric(df$num)
      
   } else if (grepl("twi", type[[i]])){
      df <- scbi18 %>%
         group_by(bins_TWI) %>%
         dplyr::summarize(avg = mean(eval(parse(text = var[i])), na.rm=TRUE),
                          dev = sd(eval(parse(text = var[i])), na.rm=TRUE),
                          no_data = (sum(is.na(eval(parse(text = var[i]))))/length(eval(parse(text = var[i])))))
      
      df$sdmin <- df$avg - df$dev
      df$sdmax <- df$avg + df$dev
      df$col <- ifelse(df$no_data > 0.25, "grey", "black")
      df$num <- gsub("^.*,", "", df$bins_TWI) #upper bound of bin
      df$num <- gsub("[[:punct:]]$", "", df$num)
      df$num <- as.numeric(df$num)
   }
   
   #create plots
   if(!grepl("twi", type[[i]])){
      q <- ggplot(df, aes(x = avg, y = num)) +
         geom_point(aes(color=col)) +
         scale_color_manual(values=c("black", "grey")) +
         ggplot2::geom_errorbarh(aes(y = num, xmin = sdmin, xmax = sdmax, height=0.5, color=col)) +
         scale_y_continuous(breaks=c(0,10,20,30,40,50,60), limits=c(0,60)) +
         geom_path(aes(x=avg, y=num, color=col, group=1)) +
         ylab("Height [m]") +
         theme_minimal() +
         theme(legend.position = "none",
               axis.text = element_text(size=12),
               axis.title = element_text(size=14))
      
      if(i==1){
         q <- q + 
            xlab("TLP [MPa]") +
            scale_x_continuous(breaks=breaks_tlp, limits=limits_tlp)
      } else if(i==2){
         q <- q + 
            xlab("PLA [%]") +
            scale_x_continuous(breaks=breaks_pla, limits=limits_pla)
      }
   } else if(grepl("twi", type[[i]])){
      q <- 
         ggplot(df[!is.na(df$bins_TWI), ], aes(y = avg, x = num)) +
         geom_point(aes(color=col)) +
         scale_color_manual(values=c("black", "grey")) +
         ggplot2::geom_errorbar(aes(x = num, ymin = sdmin, ymax = sdmax, width=0.25, color=col)) +
         geom_path(aes(x=num, y=avg, color=col, group=1)) +
         scale_x_continuous(breaks=c(2,6,10,14), limits=c(0,16)) +
         xlab("Topographic wetness index") +
         theme_minimal() +
         theme(legend.position = "none",
               axis.text = element_text(size=12),
               axis.title = element_text(size=14))
      
      if(i==3){
         q <- q + 
            ylab("Mean turgor loss point [MPa]") +
            scale_y_continuous(breaks=breaks_tlp, limits=limits_tlp)
      } else if(i==4){
         q <- q + 
            ylab("Percent leaf area [%]") +
            scale_y_continuous(breaks=breaks_pla, limits=limits_pla)
      }
   }
   assign(paste0("plot_", type[i]), q)
}


#######################################################################################
#4 Export the graphs ####
library(ggpubr)
library(extrafont)
library(rasterVis)

loadfonts(device="win") #to get TNR

quantile(current_ht$height.m, c(.95), na.rm=TRUE) #95% quantile = 35.002m
quant <- data.frame(yintercept = 35.0022, Lines = "95th percentile")

#add this part to each graph:
# geom_hline(aes(yintercept = yintercept), linetype = "dashed", quant)

##4a. Figure S1 (TLP and PLA with height and TWI) ####
plot_pla_twi
plot_tlp_twi

traits <- ggarrange(plot_tlp_twi, plot_pla_twi, nrow=1, ncol=2)
ggsave("manuscript/tables_figures/figureS3_traits_with_twi.png", width=5, height=7, units="in", traits)

##4b. Export map of plot using TWI and cored tree locations ####
species <- read.csv("data/core_list_for_neil.csv", stringsAsFactors = FALSE)
species <- species[!species$sp %in% c("frni", "pist"), ]
species$sp_fact <- as.factor(species$sp)

color_pallete_function <- colorRampPalette(
   colors = c("pink","lightskyblue", "turquoise", "springgreen", "white", "khaki", "gold", "brown"),
   space = "Lab" # Option used when colors do not represent a quantitative scale
)
species_colors <- color_pallete_function(nlevels(species$sp_fact))

cored_points <- SpatialPointsDataFrame(data.frame(species$NAD83_X, species$NAD83_Y), data=species)

plot.new()

png("manuscript/tables_figures/figureS2_location_cored_trees.png", width=5, height=7, units="in", res=300)
levelplot(topo, margin=FALSE, scales=list(draw=FALSE), 
          key=list(space="right",
                   text=list(lab=levels(cored_points@data$sp_fact)),
                   points=list(pch=20, fill=species_colors, col=species_colors)),
          colorkey=list(space="left", width=0.75, height=0.75)) +
   layer(sp.points(cored_points, pch=20, col = species_colors[species$sp_fact]))
dev.off()

##4c. height profiles ####
NEON_order <- c("(a)", "(b)", "(c)")
NEON_order_x <- c(0.5, 35, 7.5)
NEON_order_y <- c(57.5, 57.5, 57.5)
for (i in seq(along=1:3)){
  NEON_list[[i]] <-
     NEON_list[[i]] +
         theme_bw(base_size = 16) + 
         # theme_bw(base_family = "serif") + #for TNR font
         geom_hline(aes(yintercept = yintercept), linetype = "longdash", quant) +
         annotate(geom="text", x=NEON_order_x[[i]], y=NEON_order_y[[i]], 
                  label = NEON_order[[i]], fontface="bold", size=7)
  if(i==3){
     NEON_list[[i]] <-
        NEON_list[[i]] +
        theme(legend.title = element_blank(),
              legend.box = "vertical",
              legend.position = c(0.8, 0.85),
              legend.background = element_rect(fill=alpha("white", 0.01)),
              legend.text=element_text(size=12),
              legend.key.size=unit(4,"mm"),
              legend.spacing=unit(-0.5,"cm")
              )
  }
  
  if(!i==3){
     NEON_list[[i]] <-
        NEON_list[[i]] +
        theme(legend.position = "none")
  }
  
  if(!i==3){
     NEON_list[[i]] <- 
        NEON_list[[i]] +
        theme(axis.title.y = element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
  }
}

NEON <- ggarrange(NEON_list$wind_plot, NEON_list$RH_plot, NEON_list$SAAT_plot,  nrow=1, ncol=3, align="h")

###format the other height graphs
plots_bw <- list(heights, plot_pla_ht, plot_tlp_ht)
names(plots_bw) <- c("heights", "plot_pla_ht", "plot_tlp_ht")
plots_bw_order <- c("(d)", "(e)", "(f)")
plots_bw_order_x <- c(0.8, 9, -2.75)
plots_bw_order_y <- c(57.5, 57.5, 57.5)

for (i in seq(along=1:3)){
   plots_bw[[i]] <- #all graphs
      plots_bw[[i]] + 
      theme_bw(base_size = 16) + 
      # theme_bw(base_family = "serif") + #for TNR font
      geom_hline(aes(yintercept = yintercept), linetype = "longdash", quant) +
      annotate(geom="text", x=plots_bw_order_x[[i]], y=plots_bw_order_y[[i]], 
               label = plots_bw_order[[i]], fontface="bold", size=7)
   
   if(!i == 1){ #only trait graphs
      plots_bw[[i]] <- 
         plots_bw[[i]] + 
         theme(axis.text.y=element_blank(), 
               axis.title.y=element_blank(),
               axis.ticks = element_blank(),
               legend.position = "none")
   }
}


heights_other <- ggarrange(plots_bw$heights, plots_bw$plot_pla_ht, plots_bw$plot_tlp_ht, nrow=1, ncol=3)

###put plots together
png("manuscript/tables_figures/Figure2.png", width=11, height=11, units="in", res=300)
ggarrange(NEON, heights_other, nrow=2, ncol=1)
dev.off()

################################################################################
#5 Distribution of resistance values and time series (Figure 1) ####
library(ggplot2)
library(png)
library(grid)
library(gridExtra)
library(cowplot)

trees_all_sub <- read.csv("manuscript/tables_figures/trees_all_sub.csv")
trees_all_sub$year <- as.character(trees_all_sub$year)

#In each drought, roughly 30% of the cored trees
#Find n trees that have resistance <= 0.7 per year: #% in 1966, #2% in 1977, and #% in 1999. #Do same thing for those that have resistance >1 for each year
library(data.table)
test <- as.data.table(trees_all_sub)

test_reduct <- test[resist.value<=0.7,.(reduct = length(resist.value)), by=.(year)]
by_year <- test[, .(total=length(resist.value)), by=.(year)]
test_reduct$total_re <- by_year$total
test_reduct$perc_re <- test_reduct$reduct/test_reduct$total_re

test_growth <- test[resist.value>1,.(growth = length(resist.value)), by=.(year)]
test_growth$total_gr <- by_year$total
test_growth$perc_gr <- test_growth$growth/test_growth$total_gr

test_full <- merge(test_reduct, test_growth)

#save as png so that way you're arranging only png
png("manuscript/tables_figures/density_plot.png", res = 300, width = 150, height = 50, units = "mm", pointsize = 10)
ggplot(trees_all_sub) +
   aes(x = resist.value, fill = year) +
   geom_density(adjust = 1L, alpha=.5) +
   geom_vline(xintercept=1) +
   scale_fill_hue(labels=c("1966", "1977", "1999")) +
   labs(x="resistance value") +
   theme_minimal()
dev.off()

plot1 <- readPNG("manuscript/tables_figures/Time_series_for_each_species.png")
plot2 <- readPNG("manuscript/tables_figures/density_plot.png")

png("manuscript/tables_figures/Figure1.png", res=300, height=200, width=150, units="mm", pointsize=10)
plot_grid(rasterGrob(plot1), rasterGrob(plot2), align = "v", nrow = 2, rel_heights = c(3/4, 1/4), axis = "b")
dev.off()
