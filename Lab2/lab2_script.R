#author: Grzegorz Protaziuk, Robert Bembenik
# script EDAMI lab2
#1. Data preprocessing and analysis
#2. Frequent itemsets discovery
#3. Association rules discovery

#set working directory - adjust a path to your directory with datasets
#setwd("path")
#getwd()
#library loading
library(arules) # association rules
library(arulesViz) # visualization of reles


#Dataset AdultUCI is availabe in arules library
data("AdultUCI")
dim(AdultUCI)
?AdultUCI

#information about data - statistics
summary(AdultUCI)
#exemple of records
head(AdultUCI,10)

#discretization of numerical attributes
#using discretize function: n-equal-size ranges,  
?discretize
summary(AdultUCI$age)
discInterval = discretize(AdultUCI$age, method ='interval', breaks = 4) # it will be divided into 4 equal age ranges
summary(discInterval)

# n-equal-frequency ranges 
discFreq = discretize(AdultUCI$age, method= 'frequency', breaks = 4) # it will be divided into 4 different groups with similar population
summary(discFreq);
summary(AdultUCI$age)

summary(AdultUCI$age)
discFreq = discretize(AdultUCI$fnlwgt, method= 'frequency', breaks = 4)
summary(discFreq);

#removing dispensable attributes
colnames(AdultUCI)
AdultUCI["fnlwgt"] <- NULL
AdultUCI["education-num"] <-NULL

# discretization with defined ranges:
AdultUCI[[ "age"]] <- ordered(cut(AdultUCI[[ "age"]], c(15,25,45,65,100)), labels = c("Young", "Middle-aged", "Senior", "Old"))

AdultUCI[[ "hours-per-week"]] <- ordered(cut(AdultUCI[[ "hours-per-week"]],c(0,25,40,60,168)),
                                         labels = c("Part-time", "Full-time", "Over-time", "Workaholic"))

AdultUCI[[ "capital-gain"]] <- ordered(cut(AdultUCI[[ "capital-gain"]], c(-Inf,0,median(AdultUCI[[ "capital-gain"]][AdultUCI[[ "capital-gain"]]>0]),Inf)),
                                       labels = c("None", "Low", "High"))

AdultUCI[[ "capital-loss"]] <- ordered(cut(AdultUCI[[ "capital-loss"]],
                                           c(-Inf,0, median(AdultUCI[[ "capital-loss"]][AdultUCI[[ "capital-loss"]]>0]),Inf)),
                                       labels = c("none", "low", "high"))
summary(AdultUCI)

#convertion from "data.frame" datatype to "transactions" datatype because our algorithms work only on transactions
adultTR <- as(AdultUCI, "transactions")
#info about a new structure
str(AdultUCI)
str(adultTR)
View(AdultUCI)
#class(adultTR)
#statistics
summary(adultTR)
length(which(is.na(AdultUCI)==TRUE)) # how many missing values do we have?

###############################
#data analysis 
#frequency of items
?itemFrequency # calculates support for all single items.
freqTbl  = itemFrequency(adultTR, type = "relative")
str(freqTbl)
#class(freqTbl)
summary(freqTbl)

print(freqTbl)
#?sort
#sorting according  relative support values
freqTbl = sort(freqTbl, decreasing= TRUE)

#printing only items having support > 20%
print(freqTbl[freqTbl > 0.2])

#number of elements with support >= 5%
length(freqTbl[freqTbl>=0.05])

#chart
itemFrequencyPlot(adultTR, type ="relative", support= 0.2)

########################################
#2. Frequent itemsets 
#setting parameters of Apriori algorithm
?APparameter
aParam  = new("APparameter", "confidence" = 0.6, "support" =0.5, "minlen"= 1)
print(aParam)
str(aParam)
#adjusting values of parameters
aParam@support <- 0.3
aParam@confidence <-0.8
aParam@target ="frequent itemsets"
str(aParam)
?apriori
#frequent itemsets discovery - Apriori algorithm
asets <-apriori(adultTR,aParam)
#str(asets)
#analysis of discovered frequent itemsets
length(asets)
summary(asets)
inspect(head(sort(asets, by="support"),10)) # sort and show 10 most frequent itemsets

size(asets)
str(asets)

inspect(asets[size(asets)>5])
#charts
#plot(asets[1:10])
plot(asets[size(asets)>5], method = "graph")
plot(asets[size(asets)>5], method = "paracoord", control = list(reorder = TRUE))

#searching for items including a given text

?subset
setsRace <- subset(asets, subset = items %in% "White") 
setsRace <- subset(asets, subset = items %in% "race=White")# we should use the exact name of column to get it.
inspect(setsRace)

setsRace <- subset(asets, subset = items %pin% "White")
inspect(setsRace)

setsRace <- subset(asets, subset = items %ain% c("race=White","sex=Male"))
inspect(setsRace)
?is.closed
#closed itemsets
is.closed(asets)
#maximal itemsets
?is.maximal
maxSets <- asets[is.maximal(asets)==TRUE]
inspect(maxSets)
summary(maxSets)


#frequent itemsets discovery - Eclat algorithm
#parameters
ecParam  = new("ECparameter", "confidence" = 0.8, "support" = 0.3) 
print(ecParam)

#discovery of frequent itemsets
fsets <- eclat(adultTR,ecParam)
length(fsets)

#selection of frequent itemset found by means of  Eclat algorithm 
# and not found by means of Apriori algorithm

inspect(fsets[which(is.na(fsets %in% asets))])

inspect(fsets[which((fsets %in% asets)==FALSE)])
length(fsets[which((fsets %in% asets)==FALSE)])

#changing values of parameters
ecParam@support = 0.4
print(ecParam)

#frequent itemsets discovery - Eclat algorithm
fsets <- eclat(adultTR,ecParam)
length(fsets)
length(asets)
#selection of frequent itemset found by means of  Apriori algorithm 
# and not found by means of Eclat algorithm

inspect(asets[which(is.na(asets %in% fsets))])

#3. Association rules discovery

#setting of parameters
aParam@target ="rules"
aParam@minlen = 2L
aParam@confidence = 0.8

print(aParam)

#Disvoering of assocation rules by means of Apriori algorithm
aRules <-apriori(adultTR,aParam)
#statistics of rules
summary(aRules)


length(aRules)
str(aRules)
#example of rules
inspect(head(aRules))

# discovering rules with a given consequent
rulesWithRHS <- apriori(adultTR, parameter = list(support=0.2, confidence = 0.5, minlen = 2), 
                        appearance = list(rhs = c("capital-gain=None", "capital-loss=none"), default="lhs"))
inspect(rulesWithRHS[1:10])
?apriori
# discovering rules not having given items
rulesNotItems <- apriori(adultTR, parameter = list(support=0.2, confidence = 0.5, minlen = 2), 
                         appearance = list(none = c("capital-gain=None", "capital-loss=none"), default="both"))
inspect(rulesNotItems[1:10])

?subset
#filtering rules - selection of interesting rules
#rules with lift parameter >1.2
rulesLift1.2 <- subset(aRules, subset = lift > 1.2)
inspect(rulesLift1.2)

#charts presenting association rules
#size(rulesLift1.2)
plot(rulesLift1.2, shading="order", control=list(main = "Two-key plot" ))
plot(rulesLift1.2, method="matrix", measure="lift", interactive = TRUE)

#selection of rules with a given consequent
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "relationship=Husband" & lift >=2.3)
inspect(rulesInGivenConseq)
plot(rulesInGivenConseq, method = "graph", engine = "htmlwidget")

#rules based on maximal frequent itemsets
maxRul <- aRules[is.maximal(aRules) == TRUE]
summary(maxRul)
inspect(maxRul[1:10])

#removing reduntant rules (rules with the same consequent and confidence but with less items in the antecedent
notRedun <- aRules[is.redundant(aRules) == FALSE]
summary(notRedun)
inspect(notRedun[1:10])

#Selecting rules based on the selected indicator of rule interestingness 
?interestMeasure
#rules with improvement indicator greater than 0.01

resTbl <- interestMeasure(aRules,"improvement", asets) # 
intres <- which(sapply(resTbl, function(x) {x > 0.01  && x <= 1 })==TRUE)
intersRule <- aRules[intres] 
inspect(intersRule)

################################################3
#generation of association rules based on disvovered frequent itemsets
?ruleInduction

ecParam  = new("ECparameter", "confidence" = 0.8, "support" = 0.3) 
#discvoring frequent itemsets
fsets <- eclat(adultTR,ecParam)


iERules = ruleInduction(fsets, adultTR, confidence = 0.8, control=list(method ="ptree"))
summary(iERules)
length(iERules)
inspect(head(iERules))

##################################################################
#datasets for the laboratory task

#marketSet

download.file('http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/supermarket.csv','supermarket.csv')
marketSet = read.csv('supermarket.csv',sep=';')
marketSet= as.data.frame(sapply(marketSet, function(x) as.logical(x)))
summary(marketSet)
head(marketSet)

#Mushroom dataset
data(Mushroom)
?Mushroom
str(Mushroom)
summary(Mushroom)
inspect(head(Mushroom))

#datasets that can be found in the location below
#http://fimi.uantwerpen.be/data/

# Find not a large number of rules to cover edible muhsrooms. set conf=1 and minsup<0.5
# Find some items together 
