# Analysis of case study data involving buildup of grime types within design pattern instances.

library(daewr)
library(lme4)

# Import the data
setwd(".")
myData <- read.csv(file = "data/generated/test_data_cs2.csv", header = T, sep = ",", quote = "\"")

head(myData)
# Visualize the data


# Setup the longitudinal data model.
lmer.1 <- lmer(GSZ~PS + PF + PT + (PF + PT + PS | ChainID), data=myData, REML=F)
summary(lmer.1)

# Validate Assumptions


# Conduct ANOVA/perMANOVA Tests