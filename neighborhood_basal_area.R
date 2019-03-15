# calculating neighborhood basal area (NBA) for ForestGEO plot trees

#this script is attempting to recreate the efforts done by Alan Tepley from Gonzalez et al 2016: https://esajournals.onlinelibrary.wiley.com/doi/epdf/10.1002/ecs2.1595, whereby neighborhood basal area was calculated by summing the basal area of all trees within a given distance of a focal tree. Specifically, basal area for this paper was calculated to 30m at a distance increment of 0.5m. Initial calculations were done in Excel.

#for trees that have >1 stem that is at least 10cm dbh (1 individual in 2013), Tepley said, "When I calculated neighborhood basal area for the tree with Tag Number 60108 and Stem Tag 1, I would exclude the basal area for that stem and include the basal area for the tree with Tag Number 60108 and Stem Tag 2, and the distance to that tree is 0 meters."

#Tepley also created a calculation such that if a focal tree was within 30m of the ForestGEO plot edge, then that distance away he would give the NBA to be 0. For example, if a tree was 15m from the ForestGEO edge, that tree would have 0 NBA until distance 15.5m. This has not been replicated in this script, but it should be acknowledged that this was the original intent.

#script
scbi.full2 <- read.csv("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/tree_main_census/data/census-csv-files/scbi.full2.csv", stringsAsFactors=FALSE)

scbi.full2[5340, 3] <- 40874 #duplicated tag (above 10cm dbh)
scbi.full2 <- scbi.full2[scbi.full2$dbh>=100, ]#& !grepl("S", scbi.full2$codes), ]

trees <- scbi.full2[c(3,7,8)]
rownames(trees) <- trees[,1]
trees <- trees[, -1]

library(vegan)
d <- vegdist(trees, method="euclidean") #calculate distance between one tree and all the other trees using Pythagorean theorem
m <- data.frame(t(combn(rownames(trees),2)), as.numeric(d)) #put in df format
names(m) <- c("tree1", "tree2", "distance")
simple <- m[m$distance<=30, ] #only include distances of 30m or less
simple <- simple[order(simple$tree1, simple$distance), ] #sort by tree and distance

simple$tree1 <- as.character(simple$tree1)
simple$tree2 <- as.character(simple$tree2)

simple$tree1 <- as.numeric(simple$tree1)
simple$tree2 <- as.numeric(simple$tree2)

scbi.sub <- scbi.full2[scbi.full2$tag %in% unique(simple$tree1), ] #this number won't match "trees" because some trees from scbi.sub are >30m apart from another tree

scbi.sub <- scbi.sub[c(3,7,8,11)]
scbi.sub$dbh <- scbi.sub$dbh/10 #dbh needs to be in cm for basal area equation
scbi.sub$basal <- (pi*(scbi.sub$dbh/2)^2)*0.0001

dist <- seq(0,30, by=0.5)
dist <- gsub("^", "x", dist)
scbi.sub[, dist] <- NA

scbi.sub$x0 <- scbi.sub$basal

scbi.sub_list <- lapply(scbi.sub$tag, function(x){
  scbi.sub[scbi.sub$tag == x, ]}) #separate each stem into separate dataframe
names(scbi.sub_list) <- scbi.sub$tag


dist_shift <- dist[-1] #get rid of x0 now that you've defined it

#in short, the loop below does the following:
#1. defines a focal tree, creates df "test" with all distances of that focal tree (line 52-54)
#2. Adds column to test with all the trees the focal is relating to, plus the basal area of each one (line 57-62)
#3. Split test into separate dataframes (test_list) to make it easier to work with (line 64-66)
#4. Filter scbi.sub_list by the focal tree to "z". (line 69)
#5. Make each element of test_list a separate df "w". Then, define column indices to use (inc, inc_num, inc_prev). For each increment (inc and inc_num), subset the df "w" so that it returns all trees with distances less than the number of inc_num. Then, fill in df "z" whereby if inc_num is less than the max distance in the subset "w", fill in that column in "z" with the value of inc_prev. Otherwise, if inc is greater than the max distance, add the basal area from x0 to the sum of the basal areas in the filtered "w" df. (lines 72-85)
#6. Add z to list "full", then rbind to one df "all_dist". (lines 87-90)

all_dist <- NULL
full <- list()
for (j in seq(along=unique(simple$tree1))){
  tree <- unique(simple$tree1)[[j]] #one tree at a time
  test <- simple[simple$tree1 == tree | simple$tree2 == tree, ] #filter by tree
  test <- test[order(test$distance), ] #order by distance
  
  sapply(1:nrow(test), function(x){
    test$diff <- ifelse(test[x,1] == tree, test[x,2], 
                        ifelse(test[x,2] == tree, test[x,1], test$diff))})
  
  test$basal_diff <- scbi.sub$basal[match(test$diff, scbi.sub$tag)] 
  #get basal area for tags that aren't the focal tree
  
  test_list <- lapply(unique(test$diff), function(x){
    test[test$diff == x, ] }) #split test into separate df
  names(test_list) <- unique(test$diff)

  z <- scbi.sub_list[[grep(paste0("^",tree,"$"), names(scbi.sub_list))]]
    
  for (q in seq(along=test_list)){
    w <- test_list[[q]]
    w$distance <- as.numeric(w$distance)
    
    for (i in seq(along=dist_shift)){
      inc <- dist_shift[[i]]
      inc_num <- gsub("x", "", inc)
      inc_num <- as.numeric(inc_num)
      inc_num_prev <- inc_num - 0.5
      inc_prev <- gsub("^", "x", inc_num_prev)
      
      w <- test[test$distance < inc_num, ]
      
      z[, inc] <- ifelse(inc_num < max(w$distance), z[, inc_prev], sum(z$x0, sum(w$basal_diff)))
      full[[j]] <- z
    }
  }
  all_dist <- rbind(all_dist, full[[j]])
}




write.csv(m, "test.csv", row.names=FALSE)
