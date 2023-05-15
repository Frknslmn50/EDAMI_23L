# EDAMI23l Task 1 : Association Rules
# Author: Furkan Salman

# Problem: Given a dataset of mushrooms, derive association rules to classify edible mushrooms.

# Solution: Use Apriori algorithm to discover association rules.

# Load necessary dataset and libraries.
library(arules)
library(arulesViz)
data(Mushroom)

# Start by inspecting the dataset.
dim(Mushroom)
summary(Mushroom)
str(Mushroom)

# Inspect an example of records.
inspect(head(Mushroom, 5))

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

# We can see "VeilType=partial" has 1.0 relative support so it's redundant to edible mushrooms

# Plotting items with relative support > 0.25
itemFrequencyPlot(Mushroom, type = 'relative', support = 0.25)

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

# Let's observe the longest frequent itemsets:
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

# Since edible and poisonous mushroom classification is a kind of problem that we should
# be certain about the accuracy of derived association rules; therefore, minConf = 1.0.
aParam@confidence <- 1.0
# Since the support of edible will be around 0.5, we should select support < 0.5 to have an output.
aParam@support <- 0.30
print(aParam)
?apriori
# Discover association rules that have "edible" as the consequent.
rulesEdible <- apriori(Mushroom, aParam, appearance = list(rhs = 'Class=edible', default = 'lhs'))

length(rulesEdible)
str(rulesEdible)

# Taking a look at the discovered rules to select a subset of them
inspect(sort(rulesEdible, by = "lift"))

# Every discovered rule has the same lift value so I will be selecting the 5 rules
# the most support because list of rules should be memorizable
inspect(head(sort(rulesEdible, by="support"),5))
# Derived Rules:
# lhs                                                           rhs                support   confidence lift     count
# [1] {Odor=none,GillSize=broad,RingNumber=one}                  => {Class=edible} 0.3308715 1          1.930608 2688 
# [2] {Odor=none,GillSize=broad,VeilType=partial,RingNumber=one} => {Class=edible} 0.3308715 1          1.930608 2688 
# [3] {Odor=none,StalkShape=tapering}                            => {Class=edible} 0.3072378 1          1.930608 2496 
# [4] {Odor=none,GillSize=broad,StalkShape=tapering}             => {Class=edible} 0.3072378 1          1.930608 2496 
# [5] {Odor=none,StalkShape=tapering,RingNumber=one}             => {Class=edible} 0.3072378 1          1.930608 2496 
