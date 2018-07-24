library(daewr)
library(vegan)

# Import the data
setwd(".")
data <- read.csv(file = "data/generated/test_data_ex2.csv", header = T, sep = ",", quote = "\"")

quality_aspect = grime_type*pattern_type*severity

functional_suitability <- 0
functional_completeness <- 0
functional_correctness <- 0
functional_appropriateness <- 0

performance_efficiency <- 0
time_behavior <- 0
resource_utilization <- 0
capacity <- 0

compatibility <- 0
co_existence <- 0
interoperability <- 0

usability <- 0
appropriateness_recognizability <- 0
learnability <- 0
operability <- 0
user_error_protection <- 0
user_interface_aesthetics <- 0
accessibility <- 0

reliability <- 0
maturity <- 0
availability <- 0
fault_tolerance <- 0
recoverability <- 0

security <- 0
confidentiality <- 0
integrity <- 0
non_repudiation <- 0
authenticity <- 0
accountability <- 0

maintainability <- 0
modularity <- 0
reusability <- 0
analysability <- 0
modifiability <- 0
testability <- 0

portability <- 0
adaptability <- 0
installability <- 0
replaceability <- 0

# Visualize the data

# Setup the experimental model
# Calculate distance functions
dist <- vegdist(rbind(functional_suitability, performance_efficiency, compatibility, usability, reliability, security, maintainability, portability))

dist_func <- vegdist(rbind(functional_completeness, functional_correctness, functional_appropriateness))
dist_perf <- vegdist(rbind(time_behavior, resource_utilization, capacity))
dist_comp <- vegdist(rbind(co_existence, interoperability))
dist_use <- vegdist(rbind(appropriateness_recognizability, learnability, operability, user_error_protection, user_interface_aesthetics, accessibility))
dist_rely <- vegdist(rbind(maturity, availability, fault_tolerance, recoverability))
dist_sec <- vegdist(rbind(confidentiality, integrity, non_repudiation, authenticity, accountability))
dist_main <- vegdist(rbind(modularity, reusability, analysability, modifiability, testability))
dist_port <- vegdist(rbind(adaptability, installability, replaceability))

# Check Assumputions
# Need to check for multivariate homogeneity of groups dispersions (variances) using the betadisper function
anova(betadisper(dist, grime_type))
anova(betadisper(dist, pattern_type))
anova(betadisper(dist, severity))

# Conduct perMANOVA to evaluate the model
adonis(rbind(data)~grime_type*pattern_type*severity)

# Visually check that the homogeneity of group dispersions is the same
par(mfrow=c(1, 2))
plot(metaGT<-meta(dist, zerodist=ignore), type="n",
     main="beta-disp and location\nfor Grime")
# Points for each of the 26 grime types
points(metaGT, select=which(grime_type = "", col=""))

par(mfrow=c(1, 2))
plot(metaPT<-meta(dist, zerodist=ignore), type="n",
     main="beta-disp and location\nfor Patterns")
# Points for each of the 23 pattern types
points(metaGT, select=which(patter_type = "", col=""))

# Conduct contrast analysis


# Conduct multiple comparisons


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

require(Sleuth3)
data(case1101)
alc1 = lm(Metabol~Gastric*Sex, data=case1101)