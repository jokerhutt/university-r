remove(list = ls())
cat("\f")
# install.packages("psych", dependencies = TRUE)
# install.packages("ggplot2", dependencies = TRUE)
library(psych)
library(ggplot2)

dir <- "~/JCode/Stats/University/"

# Directory Variables
dirProg <- paste0(dir,"Programs/")
dirData <- paste0(dir, "Data/")
dirResult <- paste0(dir, "Results/")

#Data Frame
dsSchool <- read.csv2(file = paste0(dirData, "School.csv"), stringsAsFactors = FALSE)

head(dsSchool)

# Create GGplot
ggplot(dsSchool, aes(x = Debts)) +
  geom_histogram()

# Styled GGplot
ggplot(dsSchool, aes(x = Debts)) +
  geom_histogram(bins=15, fill="purple", col="black") +
    xlab("Debts at graduation")

ggsave(paste0(dirResult, "Tutorial02HistogramDebts.pdf"),
       width = 4, height=3)


#Bar Chart
ggplot(dsSchool, aes(x = Location)) +
  geom_bar()

ggplot(dsSchool, aes(x = Location)) +
  geom_bar(fill = "Red2", col = "black") +
  xlab("Location of university")

dsSchool[["f.Location"]] <- factor(dsSchool[["Location"]],
                              levels = c(1, 2, 3),
                              labels=c("City", "Suburbs", "Rural"))

dsSchool[["Location"]]
dsSchool[["f.Location"]]

head(dsSchool)

levels(dsSchool$f.Location)

#Bar Chart simple
ggplot(dsSchool, aes(x = f.Location)) + 
  geom_bar(fill = "violet", col = "black") +
  xlab("Location of university")

ggsave(paste0(dirResult, "Tutorial02BarcharLocationSimple.pdf"), width=4, height=3)

dsSchool[["f.School"]] <- factor(dsSchool[["School"]],
                                 levels = c(0, 1),
                                 labels=c("Public", "Private"))

ggplot(dsSchool, aes(x=f.School, fill=f.Location)) +
  geom_bar(position="dodge") +
  ylab("Frequency") + xlab("Type of university") +
  scale_fill_brewer("Location", palette="Set1")

ggsave(paste0(dirResult, "Tutorial02BarchartLocationGrouped.pdf"), width=4, height=3)


## Scatter Plots

ggplot(dsSchool, aes(x = CostTot, y = Debts)) +
  geom_point()

ggplot(dsSchool, aes(x = CostTot, y = Debts)) +
  geom_point(col = "blue") +
  xlab("Total cost of studying (k$/year)") +
  ylab("Debts at graduation (k$.year)")


## Aggregate Functions 
mean(dsSchool[["Debts"]], na.rm = TRUE)
median(dsSchool[["Debts"]], na.rm = TRUE)

sd(dsSchool[["Debts"]], na.rm = TRUE)
IQR(dsSchool[["Debts"]], na.rm = TRUE)

min(dsSchool[["Debts"]], na.rm = TRUE)
max(dsSchool[["Debts"]], na.rm = TRUE)

## Summaries and describe()

describe(dsSchool, skew=FALSE)

sub <- c("Debts", "Sat", "CostTot")

describe(dsSchool, skew = FALSE)

describe(dsSchool[sub], skew = FALSE)

tmp <- describe(dsSchool[sub])
tmp[c("mean", "sd")]



## Frequency Table

tbl <- table(dsSchool[["Location"]])

cbind(
  Freq = tbl,
  CumFreq = cumsum(tbl),
  RelFreq = 100*tbl/sum(tbl),
  CumRelFreq = 100*cumsum(tbl)/sum(tbl)
)

round(cbind(Freq = tbl,
            CumFreq = cumsum(tbl),
            RelFreq = 100*tbl/sum(tbl),
            CumRelFreq = 100*cumsum(tbl)/sum(tbl),
            3
            ))

ggplot(dsSchool, aes(x = Debts)) +
  geom_boxplot(fill="purple", col="black") +
  xlab("Debts at graduation")

ggsave(paste0(dirResult, "Tutorial02BoxplotDebts.pdf"), width = 4, height = 3)


ggplot(dsSchool, aes(x = Debts)) +
  geom_histogram(bins=20, fill="orange", col="black") +
  xlab("Debts at graduation")



## Calculate z scores with the scale function
zDebts <- scale(dsSchool[["Debts"]])
zSat <- scale(dsSchool[["Sat"]])


## Find anomalies
which(dsSchool[["Debts"]] > 25)

which(zDebts > 2.5)

which(abs(zDebts) > 2.5)

dsSchool[["Debts"]][which(zDebts > 2.1)]






















