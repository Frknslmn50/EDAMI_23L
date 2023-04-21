# EDAMI23l Task 1 : Association Rules
# Author: Furkan Salman

# Problem: Given a dataset of mushrooms, derive association rules to classify edible mushrooms.

# Solution: Use Apriori and Eclat algorithms to discover association rules.

# Load necessary dataset and libraries.
library(arules)
library(arulesViz)
data(Mushroom)

# Start by inspecting the dataset.
dim(Mushroom)
summary(Mushroom)
str(Mushroom)

# Inspect an example of records.
inspect(head(Mushroom, 10))

#####################################
# Data preprocessing.

# The data is already in transactional form and all attributes are nominal.
# Therefore, there is no need for discretization and transformation.

###############################
# Data analysis.

# Get the relative support of all attributes.
freqTbl <- itemFrequency(Mushroom, 'relative')
# Since there are many attributes, sort to see the most frequent ones.
freqTbl <- sort(freqTbl, decreasing=TRUE)
# Print only attributes having support > 0.2.
print(freqTbl[freqTbl > 0.2])

# Chart.
itemFrequencyPlot(Mushroom, type = 'relative', support = 0.2)

########################################
# Frequent itemsets.

# Set the parameters of the Apriori algorithm.
aParam <- new('APparameter', confidence = 1.0, support = 0.3, minlen = 1)
aParam@target <- 'frequent itemsets'
print(aParam)
asets <- apriori(Mushroom, aParam)

# Analyze the discovered frequent itemsets.
length(asets)
summary(asets)
# Sort and show the 10 most frequent itemsets.
inspect(head(sort(asets, by = 'support'), 10)) 

inspect(head(sort(asets[size(asets) > 8], by = 'support'), 10))
# Charts.
plot(asets[size(asets) > 8], method = 'graph')
plot(asets[size(asets) > 8], method = 'paracoord', control = list(reorder = TRUE))

# Inspect frequent itemsets containing "edible".
setsEdible <- subset(asets, subset = items %in% 'Class=edible')
inspect(head(sort(setsEdible, by = 'support'), 10))

########################################
# Association rules discovery.

# Set the parameters.
aParam@target <- 'rules'
aParam@minlen <- 2L
# Since edible and poisonous mushroom classification is a kind of problem, we should
# be certain about the accuracy of derived association rules; therefore, minConf = 1.0.
aParam@confidence <- 1.0
# Since the support of edible will be around 0.5, we should select support < 0.5 to have an output.
aParam@support <- 0.25
print(aParam)
?apriori
# Discover association rules that have "edible" as the consequent.
rulesEdible <- apriori(Mushroom, aParam, appearance = list(rhs = 'Class=edible', default = 'lhs'))

length(rulesEdible)
str(rulesEdible)

# Show the 10 rules with the highest lift measure.
inspect(head(sort(rulesEdible, by = "lift"), 10))
# Here we can see there are lots of redundant rules

#removing reduntant rules (rules with the same consequent and confidence but with less items in the antecedent)
notRedun <- rulesEdible[is.redundant(rulesEdible) == FALSE]
summary(notRedun)
inspect(notRedun)
