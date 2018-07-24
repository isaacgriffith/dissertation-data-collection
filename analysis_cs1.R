myData <- read.csv(file="data/generated/test_data_cs1.csv", header=T, sep=",")

require(rCBA)

data("iris")

output <- rCBA::buildFPGrowth(iris, "Species")
model <- output$model
predictions <- rCBA::classification(iris, model)
table(predictions)

rules = fpgrowth(myData, support=0.03, confidence=0.03, maxLength=2)

require(arules)
data("Adult")
rules <- apriori(Adult, parameter = list(supp = 0.5, conf = 0.9, target = "rules"))
summary(rules)

items <- apriori(Adult, parameter = list(supp = 0.5, conf = 0.9, maxlen = 2, target = "frequent itemsets"))
summary(items)

myData2 <- myData[,!(names(myData) %in% c("InstChainID"))]
for (i in 1:ncol(myData2)) {
  myData2[,i] <- as.logical(myData2[,i])
}

items <- apriori(myData2, parameter = list(supp = 0.15, conf = 0.51, minlen = 1, target = "rules"))
summary(items)
inspect(items, subset = rhs %pin% "PIG")

# Graph plot
require(arulesViz)
subrules2 <- head(items, n = 50, by = "support")
plot(subrules2, method = "graph")

# Grouped Matrix Plot
plot(items, method="grouped", control = list(k = 50))

# Scatterplot
plot(items)
plot(items, measure = c("support", "lift"), shading = "confidence")
