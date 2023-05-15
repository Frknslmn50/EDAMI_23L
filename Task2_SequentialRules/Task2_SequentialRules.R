# Task 2: Sequential Rules Discovery
# Author: Furkan Salman

# Problem: Provided a diabetes dataset find sequential rules
# that lead to Hypoglycemic symptoms.

# Solution: Use spade algorithm to derive sequential rules from transactional data

# Import necessary libraries
library(arules)
install.packages("arulesSequences")
library(arulesSequences)
install.packages("tidyr")
library(tidyr)


# Download and prepare data
download.file('http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/diab_trans.data','diab_trans.data')
#reading data - into dataframe
diab.df <- read.csv("diab_trans.data", header=TRUE, stringsAsFactors = FALSE)
View(diab.df)

# Print unique values in code column:
print(unique(diab.df$code))
#[1] "id_58" "id_33" "id_34" "id_62" "id_48" "id_65" "id_60" "id_35" "id_56"
#[10] "id_64" "id_61" "id_67" "id_63" "id_57" "id_72" "id_68" "id_69" "id_59"
#[19] "id_71" "id_66" "id_70" "id_36"

# The Code field is deciphered as follows:
# 33 = Regular insulin dose
# 34 = NPH insulin dose
# 35 = UltraLente insulin dose
# 48 = Unspecified blood glucose measurement
# 57 = Unspecified blood glucose measurement
# 58 = Pre-breakfast blood glucose measurement
# 59 = Post-breakfast blood glucose measurement
# 60 = Pre-lunch blood glucose measurement
# 61 = Post-lunch blood glucose measurement
# 62 = Pre-supper blood glucose measurement
# 63 = Post-supper blood glucose measurement
# 64 = Pre-snack blood glucose measurement
# 65 = Hypoglycemic symptoms
# 66 = Typical meal ingestion
# 67 = More-than-usual meal ingestion
# 68 = Less-than-usual meal ingestion
# 69 = Typical exercise activity
# 70 = More-than-usual exercise activity
# 71 = Less-than-usual exercise activity
# 72 = Unspecified special event


#Since id_36 and is_56 are not defined by the dataset description and id_72 is unspecified, we can remove them
diab.df <- diab.df[diab.df$code != "id_36",]
diab.df <- diab.df[diab.df$code != "id_56",]
diab.df <- diab.df[diab.df$code != "id_72",]

# Find and remove null values
diab.df <- na.omit(diab.df)


# Using the information about code field, we can create a dictionary for the codes
# and their corresponding meaning
diabCodes <- data.frame(code=c("id_33","id_34","id_35","id_48","id_57","id_58","id_59","id_60","id_61","id_62","id_63","id_64","id_65","id_66","id_67","id_68","id_69","id_70","id_71"),
                        meaning=c("Regular insulin dose","NPH insulin dose","UltraLente insulin dose",
                                  "Unspecified blood glucose measurement","Unspecified blood glucose measurement",
                                  "Pre-breakfast blood glucose measurement","Post-breakfast blood glucose measurement",
                                  "Pre-lunch blood glucose measurement","Post-lunch blood glucose measurement",
                                  "Pre-supper blood glucose measurement","Post-supper blood glucose measurement",
                                  "Pre-snack blood glucose measurement","Hypoglycemic symptoms","Typical meal ingestion",
                                  "More-than-usual meal ingestion","Less-than-usual meal ingestion",
                                  "Typical exercise activity","More-than-usual exercise activity",
                                  "Less-than-usual exercise activity"))
View(diabCodes)

# Create a new column called "measurement" based on the code field
diab.df$measurement <- ifelse(diab.df$code %in% c("id_65"),"Hypoglycemic symptoms",
                            ifelse(diab.df$code %in% c("id_48", "id_57", "id_58", "id_59", "id_60", "id_61", "id_62", "id_63", "id_64"), "blood glucose",
                                   ifelse(diab.df$code %in% c("id_33", "id_34", "id_35"), "insulin dose",
                                          ifelse(diab.df$code %in% c("id_66", "id_67", "id_68"), "meal ingestion","exercise activity"))))
View(diab.df)

# Plot distribution of values in the measurement = "blood glucose" column
hist(diab.df[diab.df$measurement == "blood glucose",]$value)

# Plot distribution of values in the measurement = "insulin dose" column with small bins
hist(diab.df[diab.df$measurement == "insulin dose",]$value, breaks = 100)
mean(diab.df[diab.df$measurement == "insulin dose",]$value)
median(diab.df[diab.df$measurement == "insulin dose",]$value)
sd(diab.df[diab.df$measurement == "insulin dose",]$value)
quantile(diab.df[diab.df$measurement == "insulin dose",]$value, probs = c(0.25, 0.5, 0.75))

# Based on the statistical measures above, we can discretize the insulin dose values as follows:
#Low: doses less than or equal to the 25th percentile (4 units in this case)
#Medium: doses greater than the 25th percentile and less than or equal to the 75th percentile (between 4 and 12 units in this case)
#High: doses greater than the 75th percentile (more than 12 units in this case)

# Exercice activity and meal ingestion columns are binary, so we can plot them as bar charts
barplot(table(diab.df[diab.df$measurement == "exercise activity",]$value))

barplot(table(diab.df[diab.df$measurement == "meal ingestion",]$value))

# Since the values in the meal ingestion and exercise activity, hypoglycemic symptomps columns are single valued
# it is considered as representing a "YES" value
diab.df$value <- ifelse(diab.df$measurement == "meal ingestion" | diab.df$measurement == "exercise activity" | diab.df$measurement == "Hypoglycemic symptoms","YES",diab.df$value)
View(diab.df)

# After observing the glucose levels define the discretization intervals for value column for only glucose measurements
# 0-70 = low
# 70-130 = normal
# 130-180 = high
# 180-... = very high
diab.df$value <- ifelse(diab.df$measurement == "blood glucose", ifelse(diab.df$value <= 70, "low",
                                                                        ifelse(diab.df$value <= 130, "normal",
                                                                               ifelse(diab.df$value <= 180, "high", "very high"))), diab.df$value)
View(diab.df)

# Discretize the values for insulin dose measurements as follows:
#Low: 0-4
#Medium: 4-12
#High: 12-...
diab.df$value <- ifelse(diab.df$measurement == "insulin dose", ifelse(diab.df$value <= 4, "low",
                                                                       ifelse(diab.df$value <= 12, "medium", "high")), diab.df$value)
View(diab.df)

# Change code column to corresponding meaning column
diab.df$code <- diabCodes[match(diab.df$code, diabCodes$code),]$meaning
View(diab.df)

# Change value column to corresponding code = value format
diab.df$value <- paste(diab.df$code, diab.df$value, sep = " = ")

# Remove the code, measurement columns as they are not needed anymore
diab.df$code <- NULL
diab.df$measurement <- NULL
View(diab.df)

# Save the data in transactional form removing the row names and column names
write.table(diab.df, "diab_trans2.data", sep = "," , row.names = FALSE, col.names = FALSE )

#reading data in transactional form
diabSeq <- read_baskets(con = "diab_trans2.data", sep =",", info = c("sequenceID","eventID"))
View(as(diabSeq,"data.frame"))

summary(diabSeq)

#calculation of frequency of items
freqItem = itemFrequency(diabSeq)
#str(freqItem)
freqItem = sort(freqItem, decreasing = TRUE )

head(freqItem,20)
# frequency of Hypoglycemic symptoms = 0.012554651




#discovery of sequential patterns
seqParam = new ("SPparameter",support = 0.005, maxsize = 4, mingap=600, maxgap =172800, maxlen = 3 )
patSeq= cspade(diabSeq,seqParam, control = list(verbose = TRUE, tidLists = FALSE, summary= TRUE))

# discovery of sequential rules
# confidence = 1, because we want to be sure that the rule is true
seqRules = ruleInduction(patSeq,confidence = 1)

length(seqRules)
#summary for the set of rules
summary(seqRules)
#view of of rules
inspect(head(seqRules,100))

# Select the rules with rhs Hypoglycemic symptoms = YES and lhs without Hypoglycemic symptoms = YES
hypoglycemicRules <- seqRules[rhs(seqRules) %pin% as.character("Hypoglycemic symptoms = YES") & !(lhs(seqRules) %pin% as.character("Hypoglycemic symptoms = YES"))]

# Sort the rules by lift
hypoglycemicRules <- sort(hypoglycemicRules, by = "support", decreasing = TRUE)
inspect(hypoglycemicRules)

# lhs                                                          rhs                                                          support confidence     lift 
# 1 <{"Post-breakfast blood glucose measurement = low"},
# {"NPH insulin dose = low"}>                              => <{"Hypoglycemic symptoms = YES"}>                         0.06060606          1 1.783784 
# 2 <{"Less-than-usual exercise activity = YES"},
# {"Post-breakfast blood glucose measurement = low"}>      => <{"Hypoglycemic symptoms = YES"}>                         0.04545455          1 1.783784 
# 3 <{"Post-breakfast blood glucose measurement = low"},
# {"Typical meal ingestion = YES"}>                        => <{"Hypoglycemic symptoms = YES"}>                         0.03030303          1 1.783784
# 4 <{"Post-lunch blood glucose measurement = very high"}>    => <{"Hypoglycemic symptoms = YES"}>                      0.01515152          1 1.783784 
# 5 <{"Post-breakfast blood glucose measurement = low"},
# {"Typical exercise activity = YES"}>                     => <{"Hypoglycemic symptoms = YES"}>                         0.01515152          1 1.783784 
# 6 <{"Post-supper blood glucose measurement = very high"},
# {"Typical exercise activity = YES"}>                     => <{"Hypoglycemic symptoms = YES"}>                         0.01515152          1 1.783784 
# 7 <{"Post-lunch blood glucose measurement = very high"},
# {"Pre-supper blood glucose measurement = low"}>          => <{"Hypoglycemic symptoms = YES"}>                         0.01515152          1 1.783784
# 8 <{"Post-lunch blood glucose measurement = very high"},
# {"Pre-snack blood glucose measurement = low"}>           => <{"Hypoglycemic symptoms = YES"}>                         0.01515152          1 1.783784
# 9 <{"Post-lunch blood glucose measurement = very high"},
# {"Pre-lunch blood glucose measurement = very high"}>     => <{"Hypoglycemic symptoms = YES"}>                         0.01515152          1 1.783784
# 10 <{"Post-lunch blood glucose measurement = very high"},
# {"Pre-lunch blood glucose measurement = low"}>           => <{"Hypoglycemic symptoms = YES"}>                         0.01515152          1 1.783784 
# 11 <{"Post-lunch blood glucose measurement = very high"},
# {"Pre-breakfast blood glucose measurement = very high"}> => <{"Hypoglycemic symptoms = YES"}>                         0.01515152          1 1.783784 
# 12 <{"Post-lunch blood glucose measurement = very high"},
# {"Pre-breakfast blood glucose measurement = low"}>       => <{"Hypoglycemic symptoms = YES"}>                         0.01515152          1 1.783784
# 13 <{"Less-than-usual meal ingestion = YES"},
# {"Post-lunch blood glucose measurement = very high"}>    => <{"Hypoglycemic symptoms = YES"}>                         0.01515152          1 1.783784
# 14 <{"Pre-breakfast blood glucose measurement = low"},
# {"Post-lunch blood glucose measurement = very high"}>    => <{"Hypoglycemic symptoms = YES"}>                         0.01515152          1 1.783784 
# 15 <{"Pre-lunch blood glucose measurement = low"},
# {"Post-lunch blood glucose measurement = very high"}>    => <{"Hypoglycemic symptoms = YES"}>                         0.01515152          1 1.783784 
# 16 <{"Pre-lunch blood glucose measurement = very high"},
# {"Post-lunch blood glucose measurement = very high"}>    => <{"Hypoglycemic symptoms = YES"}>                         0.01515152          1 1.783784
# 17 <{"Pre-supper blood glucose measurement = low"},
# {"Post-lunch blood glucose measurement = very high"}>    => <{"Hypoglycemic symptoms = YES"}>                         0.01515152          1 1.783784
# 18 <{"Unspecified blood glucose measurement = very high"},
# {"Post-breakfast blood glucose measurement = low"}>      => <{"Hypoglycemic symptoms = YES"}>                         0.01515152          1 1.783784 
# 19 <{"Post-lunch blood glucose measurement = very high"},
# {"More-than-usual meal ingestion = YES"}>                => <{"Hypoglycemic symptoms = YES"}>                         0.01515152          1 1.783784 