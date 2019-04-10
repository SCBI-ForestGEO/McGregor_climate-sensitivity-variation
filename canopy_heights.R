# calculating tree heights for different sp ForestGEO

heights <- read.csv("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/tree_dimensions/tree_heights/SCBI_tree_heights.csv", stringsAsFactors = FALSE)

heights <- heights[,c(1:3,5:6)]

library(data.table)
setnames(heights, old="species.code", new="sp")

dbh_2008 <- read.csv("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/tree_main_census/data/census-csv-files/scbi.stem1.csv")
dbh_2008$dbh <- dbh_2008$dbh/10

dbh_2013 <- read.csv("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/tree_main_census/data/census-csv-files/scbi.stem2.csv")
dbh_2013$dbh <- dbh_2013$dbh/10

dbh_2018 <- read.csv("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/tree_main_census/data/scbi.stem3_TEMPORARY.csv", stringsAsFactors = FALSE)

#get stemIDs for each stem
heights$stemID <- dbh_2013$stemID[match(paste(heights$tag, heights$stemtag), paste(dbh_2013$tag, dbh_2013$StemTag))]

dbh_2018$tag <- gsub("_.*$", "", dbh_2018$Tree_ID_Num)
dbh_2018$stemtag <- gsub("[[:digit:]]*_", "", dbh_2018$Tree_ID_Num)
dbh_2018$tag <- as.numeric(as.character(dbh_2018$tag))
dbh_2018$stemtag <- as.numeric(as.character(dbh_2018$stemtag))

dbh_2018$stemID <- dbh_2013$stemID[match(paste(dbh_2018$tag, dbh_2018$stemtag), paste(dbh_2013$tag, dbh_2013$StemTag))]

#create subsets and match dbh by stemID
heights_2013 <- heights[heights$height.year < 2018, ]
heights_2018 <- heights[heights$height.year == 2018, ]

heights_2013$dbh <- dbh_2013$dbh[match(heights_2013$stemID, dbh_2013$stemID)]
heights_2013$dbh_year <- 2013

heights_2018$dbh <- dbh_2018$DBHcm[match(heights_2018$stemID, dbh_2018$stemID)]
heights_2018$dbh_year <- 2018

heights <- rbind(heights_2013, heights_2018)

#check which ones need dbh from previous census because they died
check <- heights[is.na(heights$dbh) | heights$dbh ==0, ]

heights$dbh <- ifelse(heights$dbh == 0 & heights$dbh_year == 2013, 
                      dbh_2008$dbh[match(heights$stemID, dbh_2008$stemID)], 
                      ifelse(is.na(heights$dbh) & heights$dbh_year == 2018, 
                             dbh_2013$dbh[match(heights$stemID, dbh_2013$stemID)], 
                              heights$dbh))
#check again before moving on
check <- heights[is.na(heights$dbh) | heights$dbh ==0, ]

#bring in list of cored species we're using
neil_list <- read.csv("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/McGregor_climate-sensitivity-variation/core_list_for_neil.csv", stringsAsFactors = FALSE)
neil_sp <- unique(neil_list$sp)

paper_heights <- heights[heights$sp %in% neil_sp, ]
paper_heights <- paper_heights[complete.cases(paper_heights), ]
paper_heights <- paper_heights[order(paper_heights$sp), ]

unique(paper_heights$sp) #shows the sp that we have data for
paper_heights <- paper_heights[!paper_heights$sp %in% c("fram", "juni", "qual", "quve") & !paper_heights$dbh == 0, ] #juni, fram, and quve have <10 measurements has only one measure and 0 dbh meant dead



# library(dplyr)
# max_ht <- paper_heights %>% 
#   group_by(sp) %>% 
#   summarise(height.m = max(height.m))
# min_ht <- paper_heights %>% 
#   group_by(sp) %>% 
#   summarise(height.m = min(height.m))
# paper_heights$max_ht <- max_ht$height.m[match(paper_heights$sp, max_ht$sp)]
# paper_heights$min_ht <- min_ht$height.m[match(paper_heights$sp, min_ht$sp)]

library(SciViews)
library(lme4)
mix <- lmer(height.m.ln ~ dbh + (1|sp), data=paper_heights)

library(AICcmodavg)
pol <- lmer(height.m ~ poly(ln(dbh),2) + (1|sp), data=paper_heights)
# mix <- lmer(height.m ~ dbh + (1|sp), data=paper_heights)
log <- lmer(ln(height.m) ~ ln(dbh) + (1|sp), data=paper_heights)

aictab(list(log=log), second.ord=TRUE, sort=TRUE)

library(ggplot2)
library(ggpmisc)
my.formula <- ln(y) ~ ln(x)

#equations for individual sp
ggplot(data = paper_heights) +
  aes(x = ln(dbh), y = ln(height.m)) +
  geom_point(color = "#0c4c8a") +
  geom_smooth(method="lm", se=FALSE, color="black") +
  stat_poly_eq( 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + 
  theme_minimal() +
  facet_wrap(vars(sp))


library(devtools)
source_gist("524eade46135f6348140")
ggplot(data = paper_heights) +
  aes(x = ln(dbh), y = ln(height.m), label = y) +
  geom_point(color = "#0c4c8a") +
  geom_smooth(method="lm", se=FALSE, color="black") +
  stat_smooth_func(geom="text",method="lm",hjust=0.16, vjust=-1.5,parse=TRUE) +
  theme_minimal() +
  facet_wrap(vars(sp))


#equations for all species together
ggplot(data = paper_heights) +
  aes(x = ln(dbh), y = height.m) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + 
  geom_point(color = "#0c4c8a") +
  theme_minimal()
library(plotly)
ggplotly(q)
