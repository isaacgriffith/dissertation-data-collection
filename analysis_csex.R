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
library(kSamples) # for steel's test
source("randtest.R")

# Get Data
myData <- read.csv(file="data/generated/test_data_csex.csv", header=T, sep=",")
attach(myData)

# Visualize Data
pairs.panels(myData, ellipse=F)

# One-sample t-test
## Check Assumptions
### Dependent variable must be continuous
### Observations are independent of one another
### Dependent variable should be approximately normally distributed
shapiro.test(FunctionalSuitability)
shapiro.test(Maintainability)
shapiro.test(PerformanceEfficiency)
shapiro.test(Reliability)
shapiro.test(Security)
shapiro.test(TechDebt)

### Dependent variable should not contain any outliers

# Conduct Sample Size Analysis (t-test only)
sd = 0.29
sig.level = 0.05
type = "one.sample"
strict = T
power = 0.95
delta = 0.03
alt = "two.sided"

power.t.test(n = NULL, delta = delta, power = power, sd = sd, type = type, alternative = alt)

## Conduct Test H_0 : y = 0, H_A : y != 0
t.test(FunctionalSuitability, mu=0)
t.test(Maintainability, mu=0)
t.test(PerformanceEfficiency, mu=0)
t.test(Reliability, mu=0)
t.test(Security, mu=0)
t.test(TechDebt, mu=0)

# Conduct Power Analysis (t-test only)
sd = 0.3
sig.level = 0.05
type = "one.sample"
strict = T
n = 1500
delta = 0.03
alt = "two.sided"

power.t.test(n, delta = delta, power = NULL, sd = sd, type = type, alternative = alt)

# Steel's Test
## Check Assumptions

## Conduct Test
#myData$control <- rep(0, nrow(myData))
#Steel.test(list(myData[,7],myData[,1],myData[,2]), method = "simulated", alternative = "two-sided", Nsim=1000)

# Randomization Test on Paired Data (Differences)
## Check Assumptions
### The differences D_i are independent
### The distribution of each D_i is symmetric about 0 and has the same mean
### The measurement scale for the D_i(s) is at least interval

one_sample.rand.test(FunctionalSuitability)
one_sample.rand.test(Maintainability)
one_sample.rand.test(PerformanceEfficiency)
one_sample.rand.test(Reliability)
one_sample.rand.test(Security)
one_sample.rand.test(TechDebt)