myData <- read.csv(file="data/generated/test_data_csex.csv", header=T, sep=",")

require(kSamples) # for steel's test

myData$control <- rep(0, nrow(myData))
Steel.test(list(myData[,7],myData[,1],myData[,2]), method = "simulated", alternative = "two-sided", Nsim=1000)