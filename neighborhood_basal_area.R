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

scbi.sub$x0 <- scbi.sub$basal

scbi.sub_list <- lapply(scbi.sub$tag, function(x){
  scbi.sub[scbi.sub$tag == x, ]}) #separate each stem into separate dataframe
names(scbi.sub_list) <- scbi.sub$tag


dist_shift <- dist[-1]

for (j in seq(along=unique(simple$tree1))){
  tree <- unique(simple$tree1)[[j]]
  test <- simple[simple$tree1 == tree | simple$tree2 == tree, ] #filter by tree
  test <- test[order(test$distance), ] #order by distance
  test$diff <- 
  
  sapply(1:nrow(test), function(x){
    test$diff <- ifelse(test[x,1] == tree, test[x,2], 
                        ifelse(test[x,2] == tree, test[x,1], test$diff))})
  
  test$basal_diff <- scbi.sub$basal[match(test$diff, scbi.sub$tag)] 
  #get basal area for tags that aren't the focal tree
  
  test_list <- lapply(unique(test$diff), function(x){
    test[test$diff == x, ] }) #split test into separate df
  names(test_list) <- unique(test$diff)

    
    z <- scbi.sub_list[[grep(tree, scbi.sub_list)]]
    
#the below two loops work. The next step is to do some rbinding and figure make sure this entire thing then works.
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
      }
  }
#        
       
        
        scbi.sub$inc <- ifelse(scbi.sub$tag == tree & test_list[[x]]$distance >= inc_num, 
                               sum(inc-1, )
        
        
        inc <- colnames(scbi.sub[, 6:ncol(scbi.sub)])[[i]]
        #inc_prev <- colnames(scbi.sub[[i-1]])
        
        scbi.sub$inc <- 
      }
    }
  }
}

apply(6:ncol(scbi.sub), function(x){
  ifelse()
})




write.csv(m, "test.csv", row.names=FALSE)
