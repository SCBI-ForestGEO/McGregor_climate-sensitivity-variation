# calculating tree heights for different sp ForestGEO

heights <- read.csv("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/tree_dimensions/tree_heights/SCBI_tree_heights.csv", stringsAsFactors = FALSE)

heights <- heights[,c(1:3,5:6)]

library(data.table)
setnames(heights, old="species.code", new="sp")

dbh_2013 <- read.csv("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/tree_main_census/data/census-csv-files/scbi.stem2.csv")

dbh_2018 <- read.csv("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/tree_main_census/data/scbi.stem3_TEMPORARY.csv")

#get stemIDs for each stem
heights$stemID <- dbh_2013$stemID[match(paste(heights$tag, heights$stemtag), paste(dbh_2013$tag, dbh_2013$StemTag))]

heights_2013 <- heights[heights$height.year < 2018, ]
heights_2018 <- heights[heights$height.year == 2018, ]

heights_2013$dbh <- dbh_2013$dbh[match(heights_2013$stemID, dbh_2013$stemID)]
heights_2013$dbh_year <- 2013
check <- heights_2013[is.na(heights_2013$dbh), ]

heights_2018$dbh <- dbh_2018$DBHcm[match(heights_2018$stemID, dbh_2018$stemID)]
heights_2018$dbh_year <- 2018
check <- heights_2018[is.na(heights_2018$dbh), ] #there are a number of NAs for 2018, presumably because these all died

heights <- rbind(heights_2013, heights_2018)
heights$dbh <- ifelse(is.na)

#bring in list of cored species we're using
neil_list <- read.csv("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/McGregor_climate-sensitivity-variation/core_list_for_neil.csv", stringsAsFactors = FALSE)
neil_sp <- unique(neil_list$sp)

paper_heights <- heights[heights$sp %in% neil_sp, ]
paper_heights <- paper_heights[complete.cases(paper_heights), ]
paper_heights <- paper_heights[order(paper_heights$sp), ]

unique(paper_heights$sp) #shows the sp that we have data for
paper_heights <- paper_heights[!paper_heights$sp == "juni", ] #juni has only one measure



library(ggplot2)
library(ggpmisc)
my.formula <- y ~ x

#equations for individual sp
ggplot(data = paper_heights) +
  aes(x = dbh, y = height.m) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + 
  geom_point(color = "#0c4c8a") +
  theme_minimal() +
  facet_wrap(vars(sp))

#equations for all species together
ggplot(data = paper_heights) +
  aes(x = dbh, y = height.m) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + 
  geom_point(color = "#0c4c8a") +
  theme_minimal()
