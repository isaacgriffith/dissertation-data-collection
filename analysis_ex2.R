library(daewr)
library(vegan)

# Import the data
setwd(".")
myData <- read.csv(file = "data/generated/test_data_ex2.csv", header = T, sep = ",", quote = "\"")

cast_td <- 0

myData$GS <- as.factor(myData$GS)
myData$GT <- as.factor(myData$GT)
myData$PT <- as.factor(myData$PT)
model <- aov(TechnicalDebt_P ~ GT + PT + GS, data=myData)

# Visualize the data
require(psych)
pairs.panels(myData)
par(mfrow=c(2,2))
plot(model)

require(effects)
plot(allEffects(myData))

# Conduct replication analysis

# Setup the experimental model
# Check assumptions
leveneTest(model)

levels(myData$GT)

# Visually check that the homogeneity of group dispersions is the same

# Points for each of the 26 grime types

# Points for each of the 16 pattern types

# Conduct ANOVA to evaluate the model


# Conduct Kruskal-Wallis test to evaluate the model
model
summary(model)

# Conduct contrast analysis
require(lsmeans)
lmodel <- lm(TechnicalDebt_P ~ GT + PT + GS, data=myData)
leastsquare = lsmeans(lmodel, "TechnicalDebt_P", data=myData)

## difference between grime types
grime_types <- list(Org_vs_Cls = c(-1.5,-1.5,-1.5,-1.5,-1.5,-1.5,-1.5,-1.5,1,1,1,1,1,1,1,1,0,1,0,1,1,rep(0,5)),
                    Cls_vs_Mod = c(1,1,1,1,1,1,1,1,rep(0,8),-1,0,-1.333,0,0,-1.333,0,-1.333,-1.333,-1.333),
                    Mod_vs_Org = c(rep(0,8),-1,-1,-1,-1,-1,-1,-1,-1,2,-1,2,-1,-1,2,-1,2,2,2))

test_grime_types <- contrast(leastsquare, grime_types)
test(test_grime_types, joint=T)

## difference between class grime types
class_grime_types <- list(Dir_vs_Ind = c(1,1,1,1,-1,-1,-1,-1,rep(0,18)),
                          Icg_vs_Xcg = c(-1,-1,1,1,-1,-1,1,1,rep(0,18)),
                          Sin_vs_Par = c(-1,1,-1,1,-1,1,-1,1,rep(0,18)))

test_class_grime_types <- contrast(leastsquare, class_grime_types)
test(test_class_grime_types, joint=T)

## difference between modular grime types
mod_grime_types <- list(Pmg_vs_Tmg = c(rep(0,16),1,0,1,0,0,1,0,-1,-1,-1),
                        Img_vs_Xmg = c(rep(0,16),-1,0,-1,0,0,2,0,-1,-1,2),
                        Amg_vs_Emg = c(rep(0,16),1,0,-1,0,0,0,0,1,-1,0))

test_mod_grime_types <- contrast(leastsquare, mod_grime_types)
test(test_mod_grime_types, joint=T)

## difference between organizational grime types
org_grime_types <- list(Pog_vs_Mog = c(rep(0,8),rep(-0.5,8),0,1,0,1,1,0,1,rep(0,3)),
                        Iog_vs_Xog = c(rep(0,8),-1,-1,1,1,-1,-1,1,1,-1,0,-1,1,0,1,rep(0,3)),
                        Cls_vs_Res = c(rep(0,17),1,0,-1,1,0,-1,rep(0,3)),
                        Cyc_vs_Uns = c(rep(0,8),1,-1,1,-1,1,-1,1,-1,rep(0,10)))

test_org_grime_types <- contrast(leastsquare, org_grime_types)
test(test_org_grime_types, joint=T)

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