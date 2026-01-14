
library(plyr)
library(ggplot2)

#A supplier of beverages performs quality controls for the bottle process.
#During a specific control of the contents of 33cl bottles the following ten
#measures (in cl) have been obtained: 33, 35, 32, 33, 34, 32, 36, 35, 34 and 33.

qualityControlVec <- c(33, 35, 32, 33, 34, 32, 36, 35, 34, 33)

#Calculate the mean, median and mode of these ten measures.
meanQualityControlVec <- mean(qualityControlVec)
medianQualityControlVec <- median(qualityControlVec)

Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  res <- ux[tab == max(tab)]
}

modeQualityContorlVec <- Modes(qualityControlVec)

print(meanQualityControlVec)
print(medianQualityControlVec)
print(modeQualityControlVec)


