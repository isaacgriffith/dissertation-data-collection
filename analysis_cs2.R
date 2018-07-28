# Analysis of case study data involving buildup of grime types within design pattern instances.

library(daewr)
library(lme4)
library(ggplot2)
library(nlme)

# Import the data
setwd(".")
myData <- read.csv(file = "data/generated/test_data_cs2.csv", header = T, sep = ",", quote = "\"")

myData$PT <- as.factor(myData$PT)
myData$PF <- as.factor(myData$PF)
myData$InstID <- as.factor(myData$InstID)
myData$ChainID <- as.factor(myData$ChainID)

head(myData, n=20)

# Shift data from wide to long format

# Estimate Sample Size

# Visualize the data
qplot(PF, GSZ, data=myData, group=ChainID, geom=c("line", "point"))
qplot(PT, GSZ, data=myData, group=ChainID, geom=c("line", "point"))
qplot(PS, GSZ, data=myData, group=ChainID, geom=c("line", "point"))

# Setup the longitudinal data model.
lmer.1 <- lmer(GSZ~1 + PS + PF + PT + (1 | ChainID), data=myData, REML=T)
summary(lmer.1)

lme.2 <- lme(GSZ~PS+PT, random=~1|ChainID/InstID, data=myData)
lme.2
summary(lme.2)

## GSZ should be a binding of GSZ.mod, GSZ.cls, and GSZ.org. This needs to be reflected in the dataset
grime.glm1 = glmer(GSZ.cls~PS + PT + PF + (1 | ChainID), data=myData, family = poisson())

# Validate Assumptions


# Conduct ANOVA/perMANOVA Tests
summary(grime.glm1)
anova(grime.glm1, dispersion = 1, test = "Chisq")

# Estimate Power