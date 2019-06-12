#get NEON data for vertically-scaled air temperature

library(neonUtilities)
zipsByProduct(dpID="DP1.00002.001", site="SCBI", 
              package="basic", avg=30, check.size=T, savepath = "data/neon/")
stackByTable(filepath="data/neon/", 
             folder=T)

neon_air <- loadByProduct(dpID="DP1.00002.001", 
                          site=c("SCBI"),
                          package="basic", avg=30, 
                          check.size = TRUE,
                          startdate="2018-02",
                          enddate="2018-10")

neon_data <- neon_air[[1]]
unique(neon_data$verticalPosition)

neon_data_sub <- neon_data[ ,c("verticalPosition", "startDateTime", "tempSingleMean")]
neon_data_sub <- neon_data_sub[neon_data_sub$tempSingleMean > -10, ]

ggplot(data = neon_data_sub) +
  aes(x = startDateTime, y = tempSingleMean) +
  geom_point(color = "#0c4c8a") +
  theme_minimal()+
  facet_wrap(~verticalPosition)

ggplot(data = neon_data_sub) +
  aes(x = startDateTime) +
  geom_point(aes(y = tempSingleMean, shape=verticalPosition), position=position_dodge(0.3)) +
  theme_minimal()

neon_data_sub$verticalPosition <- as.character(neon_data_sub$verticalPosition)
ggplot(data = neon_data_sub) +
  aes(x = startDateTime) +
  geom_line(aes(y=tempSingleMean, group=verticalPosition, color=verticalPosition)) +
  theme_minimal()
