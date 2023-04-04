#EDAMI 2023l Lab1

library(arules) # to load the necessary library
#getwd() # to get working directory

#####################################
#data preprocessing
#####################################
#data imputation
#example vector with pseudorandom numbers  - "runif" function
?runif # this will display help of this function
wRand <- runif(100,1,10) # generating 100 random numbers between 1 to 10 and assigning to variable wRand
wRand
#pseudorandom selection of  10 natural numbers - "sample" function
#setting generator seed
set.seed(1234)
?sample
wek2 = sample(100,10) # generates ten values from 0 to 100
wek2
#assigning the spacial value NA to selected positions in a vector
wRand[wek2] <- NA # Entering missing values randomly
wRand
#a list of numbers indicating vector fields with the special value NA -"which" and "is.na" functions
?which
?is.na
which(is.na(wRand) == TRUE)  
?anyNA
#which(wRand == NA) # it doesn't give the expected result

#data copy
wRand2 <- wRand
mean(wRand) # mean will be NA because we have NA values in our dataset
?mean

#mean caluculation based on valid values 
average = mean(na.omit(wRand)) # here we dont include missing values to compute mean
average

#replacement of the special value NA by the mean
wRand2[is.na(wRand2)==TRUE] = mean(na.omit(wRand)) # replacing missing values by mean

is.na(wRand2)
is.na(wRand)
#checking
which(is.na(wRand2)) # checking if there is any missing value after replacement
wRand2
wRand2 == wRand # checking where two vectors are the same missing, value comparison results in NA

?complete.cases


################################################3
#preliminary  data analysis
?AdultUCI
data("AdultUCI") # loading the dataset from arules library 

#?summary
summary(AdultUCI) 

str(AdultUCI) # structure of dataset, Factor means nominal attribute, Ord. factor means ordinal attribute
#numeric attribute
summary(AdultUCI$workclass) # summary of a single attribute, access attr using $ sign
#standard deviation
sd(AdultUCI$age) # calculates standard deviation
#quantiles
quantile(AdultUCI$age) # calcualate the quantiles
#deciles
quantile(AdultUCI$age, prob = seq(0, 1, length = 11), type = 5)
#histogram
hist(AdultUCI$age) # to calculate and draw a histogram


#nominal attribute for example colors 
#number of occurences of values 
?table
table(AdultUCI$education, useNA = 'always') # summarizes occurences of nominal attr and includes missing value count 
#visualization of values distribution
pie(table(AdultUCI$"education")) # drawing a pie chart
barplot(table(AdultUCI$"education"), cex.names=0.7) # drawing a bar plot, cex_names = size of column names
plot(table(AdultUCI$"education"),ylab = "number of occurences") # ylab = label for y axis

#saving picture into file with the high resolution
#?png
#png("file_name.png", res=80, height=800, width=2400) 
#pie(table(AdultUCI$"education"))
#dev.off(); 


#####################################
#discretization  - "cut" function 
?cut

age = sample(130, 300, replace = TRUE) # generates 300 rand values from 1 to 130, sampling with replacement
age

#division into ranges based on the given borders
age_p = cut(age, c(0,15,25,67,200)) # ages are replaced by the ranges
age_p
#ladding labels
age_p = cut(age, c(0,15,25,67,200), labels = c("child", "pupil", "working", "pensioner")) # instead of ranges we can add labels
age_p
age_p[1] > age_p[3] # comparing nominal attr results in error we should add order
#adding the order relation
age_p = cut(age, c(0,15,25,67,200), labels = c("child", "pupil", "working", "pensioner"), ordered_result = TRUE)
age_p[1] > age_p[3]

#division into ranges based on the mean and the standard deviation values
?sd
age_p1 = cut(age, c(-Inf,mean(age)-sd(age), mean(age), mean(age)+sd(age), +Inf), labels=c('p1','p2','p3','p4'))
age_p1

#division into ranges based on the given borders
AdultUCI$age.d  <- ordered(cut(AdultUCI[["age"]], c(15,25,45,65,100)), labels = c("Young", "Middle-aged", "Senior", "Old"))


s <-AdultUCI[["age"]]
str(s)
z <-AdultUCI["age"]
str(z)
summary(AdultUCI["age.d"])
View(AdultUCI) # used to view the whole dataset


#division into ranges based on the median 
AdultUCI$"capital_gain.d" <- ordered(cut(AdultUCI[[ "capital-gain"]], c(-Inf, 0, median(AdultUCI[[ "capital-gain"]][AdultUCI["capital-gain"] > 0 ]),Inf)), labels = c("None", "Low", "High"))
#cross table
?xtabs
xtabs(formula = ~ age.d + capital_gain.d , data = AdultUCI) # it shows a cross table for two attributes using the specified formula
#####################################
#removing attributes with only one values
delOneValued <- function(inputData11)
{
  res <- c(which(sapply(inputData11, function(x) {length(unique(x))}) == 1));
  if(length(res) > 0)         
  {
    data11 <- inputData11[,-res];
  }   
  else
  {
    data11 <-inputData11;
  }
  data11 
}

#testing

which(sapply(AdultUCI, function(x) {length(unique(x))}) == 1); # sapply applies given function
dim(AdultUCI) # size of dataset
AdultUCI <- delOneValued(AdultUCI)

#adding two new attributes
AdultUCI$Att1 = 0 # adding single values attributes
AdultUCI$Att2.Cat = 'cat1'
View(AdultUCI)
colnames(AdultUCI)

which(sapply(AdultUCI, function(x) {length(unique(x))}) == 1); # finding single valued attributes
AdultUCI <- delOneValued(AdultUCI)
colnames(AdultUCI)

###########################################################
#removing attributes with unique values
delUniqueValueAtt<- function(inputData11)
{
  res <- c(which(sapply(inputData11, function(x) {length(unique(x))}) == nrow(inputData11)));
  if(length(res) >0)         
  {
    data11 <- inputData11[,-res];
  }   
  else
  {
    data11 <-inputData11;
  }
  
  data11 
}

#testing
which(sapply(AdultUCI, function(x) {length(unique(x))}) == nrow(AdultUCI))

AdultUCI <- delUniqueValueAtt(AdultUCI)
dim(AdultUCI)
#adding two new attributes
AdultUCI$Att1 = sample(nrow(AdultUCI),nrow(AdultUCI)); # sampling without replacement to have unique values
AdultUCI$Att2.Cat = sample.int(nrow(AdultUCI))

which(sapply(AdultUCI, function(x) {length(unique(x))}) == nrow(AdultUCI))

AdultUCI <- delUniqueValueAtt(AdultUCI)
colnames(AdultUCI)

###########################################################
#removing duplicates
#?duplicated
#?anyDuplicated

which(duplicated(AdultUCI) == TRUE) # checking whether we have duplicated values
length(which(duplicated(AdultUCI) == TRUE)) # count of duplicated rows
# depending on the situation we can remove them or not
AdultUCI.U <- unique(AdultUCI) # assigning the table with duplicates removed to a new variable
dim(AdultUCI)
dim(AdultUCI.U)


###########################################################

#replacement of nominal attribute by numeric attributes because the algorithms generally work with numeric values

#setting working directory

#download data
download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data', 'car.data')

#read data
cars = read.csv("car.data", header = FALSE,
                col.names = c('buying', 'maint', 'doors', 'persons', 'lug_boot','safety', "category") )


#variable internal structure
str(cars)
# data view
View(cars)

#creation of additional attribute for each value of a given nominal attribute in a given dataframe -> one hot encoding
genNumericAttrsForFactor = function(dataF, attName)
{
  #validation of input data  
  if(is.data.frame(dataF) == FALSE)
  {
    print("A given object is not a dataFrame");
    return(dataF);
  }
  
  if(is.character(attName) == FALSE)
  {
    print("The name of an attribute is not a text value");
    return(dataF);    
  }
  
  if(is.factor(dataF[[attName]]) == FALSE)
  {
    print("The indicated attribute is not a nominal one.");
    return(dataF);
  }
  
  #names for new attributes
  attsName = levels(dataF[[attName]]);
  
  #creation of new attributes
  for(name1 in attsName)
  {    
    dataF[paste0(attName,'_',name1)] = 0;
  } 
  #data updating 
  for( id in 1:nrow(dataF))
  {
    dataF[id,paste0(attName,'_',as.character(dataF[id,attName]))] = 1
  } 
  
  dataF
}

#function checking
cars2 = genNumericAttrsForFactor(cars,'maint')
View(cars)
View(cars2)
