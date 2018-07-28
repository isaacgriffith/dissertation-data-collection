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

# Generate the gules and frequent itemsets
rules <- apriori(myData2, parameter = list(supp = 0.15, conf = 0.51, minlen = 1, target = "rules"))
summary(rules)
items <- apriori(myData2, parameter = list(supp = 0.5, conf = 0.9, maxlen = 2, target = "frequent itemsets"))
summary(items)

# Extract the Rules for each grime types
## Modular Grime
inspect(rules, subset = rhs %pin% "PIG")
inspect(rules, subset = rhs %pin% "TIG")
inspect(rules, subset = rhs %pin% "PEAG")
inspect(rules, subset = rhs %pin% "PEEG")
inspect(rules, subset = rhs %pin% "TEAG")
inspect(rules, subset = rhs %pin% "TEEG")

## Class Grime
inspect(rules, subset = rhs %pin% "DIPG")
inspect(rules, subset = rhs %pin% "DISG")
inspect(rules, subset = rhs %pin% "DEPG")
inspect(rules, subset = rhs %pin% "DESG")
inspect(rules, subset = rhs %pin% "IIPG")
inspect(rules, subset = rhs %pin% "IISG")
inspect(rules, subset = rhs %pin% "IEPG")
inspect(rules, subset = rhs %pin% "IESG")

## Organizational Grime
inspect(rules, subset = rhs %pin% "PICG")
inspect(rules, subset = rhs %pin% "PIRG")
inspect(rules, subset = rhs %pin% "PECG")
inspect(rules, subset = rhs %pin% "PERG")
inspect(rules, subset = rhs %pin% "MPICG")
inspect(rules, subset = rhs %pin% "MPIUG")
inspect(rules, subset = rhs %pin% "MPECG")
inspect(rules, subset = rhs %pin% "MPEUG")
inspect(rules, subset = rhs %pin% "MTICG")
inspect(rules, subset = rhs %pin% "MTIUG")
inspect(rules, subset = rhs %pin% "MTECG")
inspect(rules, subset = rhs %pin% "MTEUG")

# Visualize the rules
## Graph plot
subrules2 <- head(rules, n = 50, by = "support")
plot(subrules2, method = "graph")

## Grouped Matrix Plot
plot(rules, method="grouped", control = list(k = 50))

## Scatterplot
plot(rules)
plot(rules, measure = c("support", "lift"), shading = "confidence")
