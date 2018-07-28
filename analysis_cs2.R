# MIT License
# 
# Copyright (c) 2018 Isaac Griffith
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# Analysis of case study data involving buildup of grime types within design pattern instances.

library(daewr)
library(lme4)
library(ggplot2)
library(nlme)
source("power_func.R")

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