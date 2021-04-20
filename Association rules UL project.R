# reading the packages
# install.packages("arules")
# install.packages("arulesViz")
# install.packages("arulesCBA")
library(arules)
library(arulesViz)
library(arulesCBA)
# data
data <- read.transactions("testdata.csv", sep = ",")
summary(data)

inspect(data)

size(data)

length(data)

# simple statistics 
itemFrequency(data, type="relative")
itemFrequency(data, type="absolute")

# 2001 rows refer to the store transactions
# 164 columns are features for each of the 164 different items that might appear in someone's grocery basket
# each cell in the matrix is a 1 if the item was purchased for the corresponding transaction, or 0 otherwise

# density value of 0.02714192 (2.7 percent) refers to the proportion of non-zero matrix cells

# as there are 2001 * 164 = 328164 positions in the matrix, we can calculate 
# that a total of 328164 * 0.02714192 = 8907 items were purchased within a month in the store.


# the average transaction contained 8907 / 2001  = 4.4513 different grocery items

# the function lists the items that were most commonly found in the transactional data
# as 511 / 2001 = 0.2554, we can determine that whole milk appeared in 25.5 percent of transactions

# a total of 432 transactions contained only a single item, while one transaction had 25 items
# first quartile and median purchase size are 2 and 3 items respectively.

# look at the contents of a sparse matrix 
inspect(data[1:10])

# check the support level for the first ten items in the grocery data
itemFrequency(data[, 1:10])
# the items in the sparse matrix are sorted in columns by alphabetical order

# set the minimum support at 10%
itemFrequencyPlot(data, support = 0.1)

# limit the plot to a specific number of items (e.g. 15 ones)
itemFrequencyPlot(data, topN = 15)

# visualize the sparse matrix for the first 5 items
image(data[1:5])

# 5 transactions and 164 possible items we requested
# cells in the matrix are filled with black for transactions (rows) where the item (column) was purchased
# it may help with the identification of potential data issues
# columns that are filled all the way down could indicate items that are purchased in every transaction
# this visualization could be especially powerful if the items were also sorted into categories
# it will not be as useful for extremely large transaction databases because the cells will be too small to discern

# random selection of 100 transactions
image(sample(data, 100))

# find the association rules using the apriori algorithm
# support at 0.6% (very low) and confidence at 25%, minimum length of a rule is 2 elements
data.rules <- apriori(data, parameter = list(support = 0.006, confidence = 0.25, minlen = 2))
data.rules

summary(data.rules)

# look at specific rules
inspect(data.rules[1:10])

# reorder the rules so that we are able to inspect the most meaningful ones
inspect(sort(data.rules, by = "lift")[1:5])

inspect(sort(data.rules, by = "confidence")[1:5])

inspect(sort(data.rules, by = "support")[1:5])

inspect(sort(data.rules, by = "count")[1:5])

# what drives people to buy root vegetables?
rules.rootveg<-apriori(data=data, parameter=list(supp=0.01,conf = 0.005), 
                       appearance=list(default="lhs", rhs="root vegetables"), control=list(verbose=F)) 
rules.rootveg.byconf<-sort(rules.rootveg, by="confidence", decreasing=TRUE)
inspect(head(rules.rootveg.byconf))

# # on the opposite - what else will I buy if I added root vegetable to my basket?
rules.rootvegopp<-apriori(data=data, parameter=list(supp=0.01,conf = 0.005), 
                          appearance=list(default="rhs", lhs="root vegetables"), control=list(verbose=F)) 
rules.rootvegopp.byconf<-sort(rules.rootvegopp, by="confidence", decreasing=TRUE)
inspect(head(rules.rootvegopp.byconf))

# look at specific products
berryrules <- subset(data.rules, items %in% "ham")
inspect(berryrules)
# some plots
#install.packages("arulesViz")
library(arulesViz)
itemFrequencyPlot(data, topN=10, type="absolute", main="Item Frequency") 
itemFrequencyPlot(data, topN=10, type="relative", main="Item Frequency")

plot(data.rules)

plot(rules.rootveg)

plot(data.rules, method="matrix", measure="lift")

plot(data.rules, measure=c("support","lift"), shading="confidence")

plot(data.rules, shading="order", control=list(main="Two-key plot"))

plot(data.rules, method="grouped")

plot(rules.rootveg, method="grouped")

plot(data.rules, method="graph")

plot(rules.rootveg, method="graph")

plot(data.rules, method="paracoord", control=list(reorder=TRUE))

plot(rules.rootveg, method="paracoord", control=list(reorder=TRUE))

# using the ECLAT algorithm
freq.items<-eclat(data, parameter=list(supp=0.006, maxlen=15)) 
inspect(freq.items)


# basic statistics with reference to confidence
freq.rules<-ruleInduction(freq.items, data, confidence=0.5)
freq.rules

freq.items<-eclat(data, parameter=list(supp=0.05, maxlen=15)) 
inspect(freq.items)
freq.rules<-ruleInduction(freq.items, data, confidence=0.1)
freq.rules
inspect(freq.rules)

# saving the output 
write(data.rules, file = "datarules.csv", sep = ",", quote = TRUE, row.names = FALSE)

# what happens if we take the output as a data frame?
datarules_df <- as(data.rules, "data.frame")
