remove(list=ls())
cat("\f")

# install.packages("stargazer", dependencies = TRUE)

library(plyr)
library(ggplot2)
library(stargazer)

dir <- "~/Jcode/Stats/University/"
dirProg <- paste0(dir,"Programs/")
dirData <- paste0(dir, "Data/")
dirResults <- paste0(dir, "Results/")

dsDrugs <- read.csv(file = paste0(dirData, "Drugstores.csv"), stringsAsFactors = FALSE)

head(dsDrugs)

#A supplier of beverages performs quality controls for the bottle process.
#During a specific control of the contents of 33cl bottles the following ten
#measures (in cl) have been obtained: 33, 35, 32, 33, 34, 32, 36, 35, 34 and 33.

qualityControlVec <- c(33, 35, 32, 33, 34, 32, 36, 35, 34, 33)

#6.1 Calculate the mean, median and mode of these ten measures.
meanQualityControlVec <- mean(qualityControlVec)
medianQualityControlVec <- median(qualityControlVec)

Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  res <- ux[tab == max(tab)]
}

modeQualityControlVec <- Modes(qualityControlVec)

print(meanQualityControlVec) # 33.7
print(medianQualityControlVec) # 33.5
print(modeQualityControlVec) # 33

#6.2 Calculate the variance,standard deviation,range and inter quartile range of the ten measures.
varianceQualityControlVec <- var(qualityControlVec)
rangeQualityControlVec <- range(qualityControlVec)
iqrQualityControlVec <- IQR(qualityControlVec)
stDevQualityControlVec <- sd(qualityControlVec)

print(varianceQualityControlVec) # 1.79
print(rangeQualityControlVec) # 32, 36
print(iqrQualityControlVec) #1.75
print(stDevQualityControlVec) #1.337

# A financial analist studies the monthly sales of two firms, A and B.
# The monthly sales (in me) of firm A in January to December is equal to:
firmASales <- c(10, 9, 8.5, 7, 7.5, 9, 10.5, 12, 14, 17.5, 15, 13)
firmBSales <- c(25, 26, 30, 35, 45, 50, 60, 58, 50, 40, 30, 28)

# 7.1 Make a sketch of sales agains time by firm in a single graphic.
months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
dfCombined <- data.frame(
  month = rep(months, 2),
  value = c(firmASales, firmBSales),
  firm = rep(c("A", "B"), each = 12)
)

ggplot(dfCombined, aes(x=month, y=value, group=firm, color=firm)) +
  geom_line() +
  geom_point()

meanSalesFirmA <- mean(firmASales)
meanSalesFirmB <- mean(firmBSales)

# 7.2 Calculate the mean monthly sales(in me) of both firms
print(meanSalesFirmA) # 11.083
print(meanSalesFirmB) # 39.75

# 7.3 Calculate the mean(compounded) monthly sales growth of both firms for both the entire year and for the first and second halves of the years
CompoundedMonthly <- function(xStart, xEnd, n) {
  g <- (xEnd / xStart)^(1 / n) - 1
}

lastSixMonthsOfYear <- months[7: 12]
lastSixMonthSalesA <- firmASales[7: 12]
lastSixMonthSalesB <- firmBSales[7: 12]

compoundedMeanMonthlySalesGrowthA <- CompoundedMonthly(firmASales[1], firmASales[length(firmASales)], length(months) - 1)
compoundedMeanMonthlySalesGrowthB <- CompoundedMonthly(firmBSales[1], firmBSales[length(firmBSales)], length(months) - 1)

compoundedLastHalfOfYearMeanMonthlySalesGrowthA <- CompoundedMonthly(lastSixMonthSalesA[1], lastSixMonthSalesA[length(lastSixMonthSalesA)], length(lastSixMonthsOfYear) - 1)
compoundedLastHalfOfYearMeanMonthlySalesGrowthB <- CompoundedMonthly(lastSixMonthSalesB[1], lastSixMonthSalesB[length(lastSixMonthSalesB)], length(lastSixMonthsOfYear) - 1)

print(compoundedMeanMonthlySalesGrowthA) # 0.02414
print(compoundedMeanMonthlySalesGrowthB) # 0.01035

print(compoundedLastHalfOfYearMeanMonthlySalesGrowthA) # 0.04364
print(compoundedLastHalfOfYearMeanMonthlySalesGrowthB) # -0.14138

# 7.4 Calculate the variance and the standard deviation of the monthly sales of both firms.
# Also, calculate the variation coefficient of the monthly sales firms A and B.
varianceFirmA <- var(firmASales)
stDevFirmA <- sd(firmASales)
variationCoefficientFirmA <- stDevFirmA / meanSalesFirmA

varianceFirmB <- var(firmBSales)
stDevFirmB <- sd(firmBSales)
variationCoefficientFirmB <- stDevFirmB / meanSalesFirmB

print(varianceFirmA) # 10.5379
print(varianceFirmB) # 158.0227

print(stDevFirmA) # 3.2462
print(stDevFirmB) # 3.2462

print(variationCoefficientFirmA) # 0.2929
print(variationCoefficientFirmB) # 0.3162


## 8.1 Use function ggplot to make a histogram of sales growth in the preceding year(Growth).
# The histogram has red bars with black lining.

ggplot(data = dsDrugs, aes(x = Growth)) +
  geom_histogram(fill = "red", color="black")

# 8.2 Identify observations with a standardized outcome (𝑧-score) of sales
# growth larger than 2.5, and determine the consequences of removing these observations for the average sales growth
outlierObservations <- abs(scale(dsDrugs[["Growth"]])) > 2.5

dfOutliers <- dsDrugs[outlierObservations, ]
filteredObservations <- dsDrugs[!outlierObservations, ]

nrow(dfOutliers) # 14
nrow(filteredObservations) # 60

ggplot(data = filteredObservations, aes(x = Growth)) +
  geom_histogram(fill = "red", color="black")


































