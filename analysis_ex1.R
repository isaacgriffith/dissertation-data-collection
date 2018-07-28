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

library(daewr)
library(vegan)
library(car)
library(psych)
library(easypower)
library(sjPlot)
library(sjmisc)
library(ggplot2)
library(lsmeans)
library(psych)
library(dplyr)
library(biotools)
library(corrplot)

# Import the data
setwd(".")
myData <- read.csv(file = "data/generated/test_data_ex1.csv", header = T, sep = ",", quote = "\"")
myData$GS <- as.factor(myData$GS)
myData$GT <- as.factor(myData$GT)
myData$PT <- as.factor(myData$PT)
attach(myData)

# Create Multivariate model
responses <- cbind(FunctionalSuitability, Maintainability, PerformanceEfficiency, Reliability, Security)
grime.mod <- lm(responses~PT*GT*GS)

# Create Univariate Models
uni_model_fs <- lm(FunctionalSuitability~GT*GS*PT)
uni_model_m <- lm(Maintainability~GT*GS*PT)
uni_model_pe <- lm(PerformanceEfficiency~GT*GS*PT)
uni_model_r <- lm(Reliability~GT*GS*PT)
uni_model_s <- lm(Security~GT*GS*PT)

uni_model_fs.aov <- aov(uni_model_fs)
uni_model_m.aov <- aov(uni_model_m)
uni_model_pe.aov <- aov(uni_model_pe)
uni_model_r.aov <- aov(uni_model_r)
uni_model_s.aov <- aov(uni_model_s)

# Visualize the data
pairs.panels(myData, ellipse=F)

# Conduct replication analysis
main.eff.1 <- list(name = "GT", levels = 26, eta.sq = 0.125)
main.eff.2 <- list(name = "GS", levels = 5, eta.sq = 0.125)
main.eff.3 <- list(name = "PT", levels = 16, eta.sq = 0.125)
n.multiway(iv1 = main.eff.1, iv2 = main.eff.2, iv3 = main.eff.3, interaction.eta2 = 0.075, power = 0.95)

# Check Assumputions
# Check for Outliers
# * Do this by first assessing Mahalanobis Distance between dependent variables
# * Sort these distances from greatest to least
# * Find the Critical chi square value at p = 0.001, with df = number of dependent variables, and any distances greater than this should be removed
responses.cov <- cov(responses)
responses.dist <- mahalanobis(responses, center=F, cov=responses.cov)
thresh <- qchisq(0.001, 5, lower.tail = F)
which(responses.dist >= thresh)

# Check the linearity of the dependent variables
# * This is done using a scatter plot matrix, each of the dependent variables should be linearly related. Check each group of the MANOVA seperately
scatterplotMatrix(responses)

# Check for the absence of multicollinearity
# * This is done by conducting correlations among the dependent variables. 
# * The dependent variables should be moderately related, but none should have a value > 0.80
round(cor(responses),2)

# Equality of covariance matrices
# * Checked by conducting a Box's M test at a significance level of 0.001, if the p-value is greater than 0.001, the test is passed
boxM(responses, GT)

# Visually check that the homogeneity of group dispersions is the same and normality using Q-Q plots is on the line
par(mfrow=c(2,2))
plot(uni_model_fs)
plot(uni_model_m)
plot(uni_model_pe)
plot(uni_model_r)
plot(uni_model_s)

# Normality of the independent and dependent variables
shapiro.test(FunctionalSuitability)
shapiro.test(Maintainability)
shapiro.test(PerformanceEfficiency)
shapiro.test(Reliability)
shapiro.test(Security)

# Equality of variance for each univariate model
leveneTest(uni_model_fs)
leveneTest(uni_model_m)
leveneTest(uni_model_pe)
leveneTest(uni_model_r)
leveneTest(uni_model_s)

# Univariate outliers

# Conduct MANOVA to evaluate the model
model <- Manova(grime.mod, type="III", test.statistic="Pillai") # all interactions
model
summary(model)
summary(Anova(grime.mod, type="III"), univariate=T, multivariate=F, p.adjust.method=T)
summary.aov(model)
model1 <- manova(responses~PT*GT+PT*GS+GT*GS, data=myData) # only 2-way interactions
summary(model1)
summary.aov(model1)
model2 <- manova(responses~PT+GT+GS, data=myData) # additive model
summary(model2)
summary.aov(model2)
#adonis(responses~GT+GS+PT, data=myData, permutations=99)

# Conduct contrast analysis for each univariate model
lsm_fs <- lsmeans(uni_model_fs, ~GT)
lsm_m  <- lsmeans(uni_model_m, ~GT)
lsm_pe <- lsmeans(uni_model_pe, ~GT)
lsm_r  <- lsmeans(uni_model_r, ~GT)
lsm_s  <- lsmeans(uni_model_s, ~GT)
# lsm <- lsmeans(fit, ~GT|GS) # fit across GT for each level of GS
# lsm <- lsmeans(fit, ~GT|PT) # fit across GT for each level of PT
# lsm <- lsmeans(fit, ~GT|GS + PT) # fit across GT for each level of GS for each level of PT

grime_types <- list(Org_vs_Cls = c(-1.5,-1.5,-1.5,-1.5,-1.5,-1.5,-1.5,-1.5,1,1,1,1,1,1,1,1,0,1,0,1,1,rep(0,5)),
                    Cls_vs_Mod = c(1,1,1,1,1,1,1,1,rep(0,8),-1,0,-1.333,0,0,-1.333,0,-1.333,-1.333,-1.333),
                    Mod_vs_Org = c(rep(0,8),-1,-1,-1,-1,-1,-1,-1,-1,2,-1,2,-1,-1,2,-1,2,2,2))

## difference between class grime types
class_grime_types <- list(Dir_vs_Ind = c(1,1,1,1,-1,-1,-1,-1,rep(0,18)),
                          Icg_vs_Xcg = c(-1,-1,1,1,-1,-1,1,1,rep(0,18)),
                          Sin_vs_Par = c(-1,1,-1,1,-1,1,-1,1,rep(0,18)))

## difference between modular grime types
mod_grime_types <- list(Pmg_vs_Tmg = c(rep(0,16),1,0,1,0,0,1,0,-1,-1,-1),
                        Img_vs_Xmg = c(rep(0,16),-1,0,-1,0,0,2,0,-1,-1,2),
                        Amg_vs_Emg = c(rep(0,16),1,0,-1,0,0,0,0,1,-1,0))

## difference between organizational grime types
org_grime_types <- list(Pog_vs_Mog = c(rep(0,8),rep(-0.5,8),0,1,0,1,1,0,1,rep(0,3)),
                        Iog_vs_Xog = c(rep(0,8),-1,-1,1,1,-1,-1,1,1,-1,0,-1,1,0,1,rep(0,3)),
                        Cls_vs_Res = c(rep(0,17),1,0,-1,1,0,-1,rep(0,3)),
                        Cyc_vs_Uns = c(rep(0,8),1,-1,1,-1,1,-1,1,-1,rep(0,10)))


# Execute Contrasts for Functional Suitability
summary(contrast(lsm_fs, grime_types))
summary(contrast(lsm_fs, class_grime_types))
summary(contrast(lsm_fs, mod_grime_types))
summary(contrast(lsm_fs, org_grime_types))

# Execute Contrasts for Maintainability
summary(contrast(lsm_m, grime_types))
summary(contrast(lsm_m, class_grime_types))
summary(contrast(lsm_m, mod_grime_types))
summary(contrast(lsm_m, org_grime_types))

# Execute Contrasts for Performance Efficiency
summary(contrast(lsm_pe, grime_types))
summary(contrast(lsm_pe, class_grime_types))
summary(contrast(lsm_pe, mod_grime_types))
summary(contrast(lsm_pe, org_grime_types))

# Execute Contrasts for Reliability
summary(contrast(lsm_r, grime_types))
summary(contrast(lsm_r, class_grime_types))
summary(contrast(lsm_r, mod_grime_types))
summary(contrast(lsm_r, org_grime_types))

# Execute Contrasts for Security
summary(contrast(lsm_s, grime_types))
summary(contrast(lsm_s, class_grime_types))
summary(contrast(lsm_s, mod_grime_types))
summary(contrast(lsm_s, org_grime_types))

# Conduct multiple comparisons for each univariate model
TukeyHSD(uni_model_fs.aov, ordered = T)
TukeyHSD(uni_model_m.aov, ordered = T)
TukeyHSD(uni_model_pe.aov, ordered = T)
TukeyHSD(uni_model_r.aov, ordered = T)
TukeyHSD(uni_model_s.aov, ordered = T)

# Conduct Power Analysis
rmin <- 2
rmax <- 52
alpha <- .05
sigma <- 0.1
Delta <- 0.2
nlev <- c(16, 5)
nreps <- c(rmin:rmax)
result <- Fpower2(alpha, nlev, nreps, Delta, sigma)
options(digits = 5)
result