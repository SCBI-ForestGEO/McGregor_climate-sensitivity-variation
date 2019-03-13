# calculating basal area increment for ForestGEO plot trees

scbi.full2 <- read.csv("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/tree_main_census/data/census-csv-files/scbi.full2.csv", stringsAsFactors=FALSE)

scbi.full2[5340, 3] <- 40874 #duplicated tag (above 10cm dbh)
scbi.full2 <- scbi.full2[scbi.full2$dbh>=100  & !grepl("S", scbi.full2$codes), ]

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


test <- simple[simple$tree1 == 12165 | simple$tree2 == 12165, ]

for (i in seq(along=colnames(scbi.sub))){
  for (j in seq(along=unique(simple$tree1))){
    #for (k in seq(along = colnames(scbi.sub[, 6:ncol(scbi.sub)]))){
      if(i==k){
        tree <- unique(simple$tree1)[[j]]
        test <- simple[simple$tree1 == tree | simple$tree2 == tree, ] #filter by tree
        test <- test[order(test$distance), ] #order by distance
        
        inc_prev <- colnames(scbi.sub[[i-1]])
        inc_num <- colnames(scbi.sub[, 6:ncol(scbi.sub)])[[k]]
        inc_num <- gsub("x", "", inc)
        scbi.sub$inc <- ifelse(test$distance >= inc_num, sum(#basal area of tree corresponding to test$distance, the value in the previous column))
      }
    }
  }
}

apply(6:ncol(scbi.sub), function(x){
  ifelse()
})




write.csv(m, "test.csv", row.names=FALSE)
