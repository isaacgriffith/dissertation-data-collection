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

# Import the data
setwd(".")
myData <- read.csv(file = "data/generated/test_data_ex2.csv", header = T, sep = ",", quote = "\"")

myData$GS <- as.factor(myData$GS)
myData$GT <- as.factor(myData$GT)
myData$PT <- as.factor(myData$PT)
attach(myData)

model <- TechnicalDebt_P ~ GT * PT * GS
model.aov <- aov(model)
model.Anova <- Anova(lm(model), type="III")

# Visualize the data
pairs.panels(myData, ellipse=F)

# Conduct replication analysis
main.eff.1 <- list(name = "GT", levels = 26, eta.sq = 0.125)
main.eff.2 <- list(name = "GS", levels = 5, eta.sq = 0.125)
main.eff.3 <- list(name = "PT", levels = 16, eta.sq = 0.125)
n.multiway(iv1 = main.eff.1, iv2 = main.eff.2, iv3 = main.eff.3, interaction.eta2 = 0.075, power = 0.95)

# Setup the experimental model
# Check assumptions
## Check homogeneity of variance
leveneTest(model)

## Check for normality using the Shapiro Wilks test
shapiro.test(myData$TechnicalDebt_P)

# Visually check that the homogeneity of group dispersions is the same and normality using Q-Q plots is on the line
pairs.panels(myData)
par(mfrow=c(2,2))
plot(model)

# Conduct ANOVA to evaluate the model
summary(model)

# Conduct Kruskal-Wallis test to evaluate the model
model
summary(model)

# Evaluate Interactions
fit <- lm(TechnicalDebt_P ~ GS * PT * GT, data=myData)
theme_set(theme_sjplot())

plot_model(model, type="int")

# Conduct contrast analysis
lmodel <- lm(TechnicalDebt_P ~ GT * PT * GS, data=myData)
leastsquare = lsmeans(lmodel, "TechnicalDebt_P", data=myData)

## difference between grime types
fit <- model
lsm <- lsmeans(fit, ~GT)
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

# Execute the contrasts
summary(contrast(lsm, grime_types))
summary(contrast(lsm, grime_types))
summary(contrast(lsm, grime_types))
summary(contrast(lsm, grime_types))

# Conduct multiple comparisons
TukeyHSD(model, ordered = T)

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