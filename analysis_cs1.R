require(rCBA)
require(arules)
require(arulesViz)

# import the data
myData <- read.csv(file="data/generated/test_data_cs1.csv", header=T, sep=",")

# Remove InstChainID from the dataset
myData2 <- myData[,!(names(myData) %in% c("InstChainID"))]
for (i in 1:ncol(myData2)) {
  myData2[,i] <- as.logical(myData2[,i])
}

# Set the parameters
min_sup <- 0.15
min_conf <- 0.51
min_itemset_len <- 1
max_itemset_len <- 2

rules <- apriori(myData2, parameter = list(supp = 0.15, conf = 0.51, minlen = 1, target = "rules"))
summary(rules)
items <- apriori(myData2, parameter = list(supp = 0.5, conf = 0.9, maxlen = 2, target = "frequent itemsets"))
summary(items)

## Modular Grime
inspect(rules, subset = rhs %pin% "PIG")
inspect(rules, subset = rhs %pin% "TIG")
inspect(rules, subset = rhs %pin% "PEAG")
inspect(rules, subset = rhs %pin% "PEEG")
inspect(rules, subset = rhs %pin% "TEAG")
inspect(rules, subset = rhs %pin% "TEEG")

## Class Grime
inspect(rules, subset = rhs %pin% "PEAG")
inspect(rules, subset = rhs %pin% "PEAG")
inspect(rules, subset = rhs %pin% "PEAG")
inspect(rules, subset = rhs %pin% "PEAG")
inspect(rules, subset = rhs %pin% "PEAG")
inspect(rules, subset = rhs %pin% "PEAG")
inspect(rules, subset = rhs %pin% "PEAG")
inspect(rules, subset = rhs %pin% "PEAG")

## Organizational Grime
inspect(rules, subset = rhs %pin% "PEAG")
inspect(rules, subset = rhs %pin% "PEAG")
inspect(rules, subset = rhs %pin% "PEAG")
inspect(rules, subset = rhs %pin% "PEAG")
inspect(rules, subset = rhs %pin% "PEAG")
inspect(rules, subset = rhs %pin% "PEAG")
inspect(rules, subset = rhs %pin% "PEAG")
inspect(rules, subset = rhs %pin% "PEAG")
inspect(rules, subset = rhs %pin% "PEAG")
inspect(rules, subset = rhs %pin% "PEAG")
inspect(rules, subset = rhs %pin% "PEAG")
inspect(rules, subset = rhs %pin% "PEAG")


# Visualize the rules
## Graph plot
subrules2 <- head(rules, n = 50, by = "support")
plot(subrules2, method = "graph")

## Grouped Matrix Plot
plot(rules, method="grouped", control = list(k = 50))

## Scatterplot
plot(rules)
plot(rules, measure = c("support", "lift"), shading = "confidence")
