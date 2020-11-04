#Loading the Dataset (from Kaggle)
original.data <- read.csv(file='C:/Users/MJ/Desktop/Dataset/BlackFriday.csv', header=TRUE, sep=",")

#Installing necessary packages
library("UsingR")
library(sampling)
library(stringr)  
library(tidyverse)  
library(stats)
library(prob)
library(dtplyr)
library(dbplyr)

#Data
print("Black Friday Sales Data")
head(original.data)
print("Columns we have in our data")
names(original.data)

attach(original.data)

#DATA PRE_PROCESSING (GATHERING + CLEANING)
# Creating data for analysis and processing (data pre-processing stage)
user.id = User_ID
product.id = Product_ID
gender = Gender
age.range = Age
occupation = Occupation
city = City_Category

# vectorization (data to numeric values for processing) 
years.in.current.city = as.numeric(Stay_In_Current_City_Years)
marital.status = Marital_Status
product.category.1 = Product_Category_1
# NAs were introduced in the above lines of code

print("Data Cleaning")
# convert NA to 0 to perform calculations 
product.category.2 = Product_Category_2
product.category.2[which(is.na(product.category.2))] = 0

# convert NA to 0 to perform calculations 
product.category.3 = Product_Category_3
product.category.3[which(is.na(product.category.3))] = 0

purchase = Purchase

#making dataframe of data
blackfridaysales = data.frame(user.id, product.id, gender, occupation, city, years.in.current.city, marital.status, product.category.1, product.category.2, product.category.3, purchase)

#DATA EXPLORATION
# table() will summarize data
head(table(user.id))
print("User Id: Not Unique, maps person to the particular purchase")

head(table(product.id))
print("Product Id: Not Unique, tells how many purchases are made for a product")

table(gender)
print("Gender: Have only two variables: F M")

table(age.range)
print("Age: It is divided into 7 ranges, Here Age is Categorical Variable")

table(occupation)
print("Occupation: There are 22 different occupation ranging from 0-21")

table(city)
print("CIty Category: Cities in which customers have lived is categorized into three categories: A B C")

table(years.in.current.city)
print("Years.. : People have lived in the current city for 0-5 years. Here 5 could mean atleast 5 years")

table(marital.status)
print("Marital Status: People have their marriage status marked as either 0 or 1")

table(product.category.1)
print("Product Category 1: Ranges form 1-18")

table(product.category.2)
print("Product Category 2: Ranges form 2-18")
print("0 = NA for the product category, substituted this to make data clean")


table(product.category.3)
print("Product Category 3: Ranges form 3-18")
print("0 = NA for the product category")

head(table(purchase),n=10)
print("It is the amount people spent in $ for purchases. Not unique.")

#1. Analysis of categorical and numerical variables

# 1.1 Analysis on "Gender"
# keeping copy of data in a temporary variable
temporary = gender 
gender = table(gender)
# making labels for piechart
gender.labels = c(" Female", " Male")
gender.percent = round(gender/sum(gender)*100)
gender.labels = paste(gender.labels, gender.percent)
gender.labels = paste(gender.labels, "%", sep="")
print("Ploting a pie chart to check which gender shop more")
pie(gender, labels = gender.labels, col=c("Pink","Blue"), main="BlackFriday | Gender wise shoppers distribution")
print("RESULT : We can conclude that Male(75%) shop more than Female(25%) by the pie chart")
# making the variable same as before
gender = temporary 

#1.2 Analysis on "Age"
barplot(table(age.range), col="Red", main="Black Friday Shoppers by Age Range", xlab="Age Ranges", ylab="Count")
print("RESULT : People withing range of 26-35 shopped most")
print("While people in age-range 0-17 or 55+ shopped least and almost none compared to 26-35")
print("Also, overall people within age range 18-45 are the group which makes maximum population of shooping")

#1.3 Analysis on "Purchase"
print("Analysing 'Purchase' by barplot, histogram, boxplot ")
# average purchase
cat("Average dollars shoppers spent = ", mean(purchase))
# purchase range
range.purchase = range(purchase)
cat("Range of amount shoppers spent = ", range.purchase)

# Barplot
print("We can see an overview how much amount is spent by people")
barplot(table(purchase), border = c("darkgreen"), main="Purchase made in $ by shoppers", xlab = "Amount", ylab="Frequency of people")
print("RESULT : Hardly a shopper spend above $19000")
print("Shoppers mostly spent an amount of approximately 6800 or 8700 as they got highest peak in barplot")

# HISTOGRAMS
print("We plot histogram with increasing breaks to analyse data")
# breaks = 10
hist(purchase,breaks=10, xlim=c(185,25000),col="darkorange", main="Purchase made in $ by shoppers(Breaks=10)", xlab="Amount", ylab="Frequency of people")
print("Break=10 We see max data lies between 5000-10000, increase break to 20")
# breaks = 20
hist(purchase,breaks=20, xlim=c(185,25000),col="darkorange", main="Purchase made in $ by shoppers(Breaks=20)", xlab="Amount", ylab="Frequency of people")
print("We can see there are some figures which are not at all spent and good amount is spent near 15000 and b/w 5000-10000, increasing break=40")
# breaks = 40
hist(purchase,breaks=40, xlim=c(185,25000),col="darkorange", main="Purchase made in $ by shoppers(Breaks=40)", xlab="Amount", ylab="Frequency of people")
print("We can now clearly see how much figures people spent")
print("RESULT : If a shopper is coming to black friday sale there are maximum chances, he would be spending on an average at least $5000")
print("Maximum shoppers populayion lie across $5000 mark")
print("Coincidence & Interesting to see a 0 frequency near 10,000, and mid of 15000-20000")
print("We may consider that people didn't spent in $9000 or $17000(avg of 15K & 20K) in sales")

# SUMMARY
print("Summary for figures people spent")
summary(purchase)
print("Min - Minimum Purchase Value")
print("1st Quantle - Middle number between the smallest number and the median of the data set")
print("It tells the mid value of min and median purcahse value")
print("Median - Middle value of all the values")
print("2nd Quantle - Middle number between the median and the largest number of the data set")
print("It tells the mid value of min and median purcahse value")
print("Max - Maximum Purchase Value")

# BOXPLOT
print("Plotting a boxplot")
print("In box plot we can clearly see distribution of the amounts spended in black friday sales")
f = fivenum(purchase)
oulier = c(f[2]-1.5*(f[4]-f[2]) , f[4]+1.5*(f[4]-f[2]))
boxplot(f,horizontal = TRUE, xaxt="n", xlab="Amount", col="yellow", main="Purchase made in $ by shoppers")
axis(side = 1, at = f, labels=TRUE)
text(f,srt=90, rep(1.2,5), adj=0,labels=c("Min", "Lower Hinge", "Mean","Upper Hinge", "Max"))
print("RESULT : We can consider an average shopper will spend $5866-$12073 in black friday sales")


#2. Analysis of variables and corresponding plots
print("Analysing Multivariate Data")
# CHECK WHICH GENDER TOOK WHICH PRODUCT CATEGORY
# THERE ARE 3 CATEGORIES
# m male
# f female
# pc.1 product category 1
# pc.2 product category 2
# pc.3 product category 3
# converting data to make data frame
g = as.vector(as.character(gender))
pc1 = as.vector(as.numeric(as.character(product.category.1)))
pc2 = as.vector(as.numeric(as.character(product.category.2)))
pc3 = as.vector(as.numeric(as.character(product.category.3)))

# Checking all have equal rows to convert to data frame and confirming after cleaning the data
# > NROW(x = pc2)
# [1] 537577
# > NROW(x = pc1)
# [1] 537577
# > NROW(x = pc3)
# [1] 537577
# > NROW(x = g)
# [1] 537577
# created a data frame for variables/data required (gender, product category 1,2,3)

temp.data = data.frame(gender = g, product.category.1 = pc1, product.category.2 = pc2, product.category.3 = pc3)
head(temp.data)
g = temp.data$gender
pc1 = temp.data$product.category.1
pc2 = temp.data$product.category.2
pc3 = temp.data$product.category.3

m.pc.1 = sum(pc1[which( g == 'M')])
m.pc.2 = sum(pc2[which( g == 'M')])
m.pc.3 = sum(pc3[which( g == 'M')])

f.pc.1 = sum(pc1[which( g == 'F')])
f.pc.2 = sum(pc2[which( g == 'F')])
f.pc.3 = sum(pc3[which( g == 'F')])

# CREATING SUMMARIZED DATA/TABLE
# table creation
bidata = rbind(c(m.pc.1,f.pc.1),c(m.pc.2,f.pc.2),c(m.pc.3,f.pc.3))

# naming columns and rows
gender.names = c("male","female")
product.category.names = c("product category 1","product category 2","product category 3")
colnames(bidata) = gender.names
rownames(bidata) =  product.category.names
dimnames(bidata) = list(ProductCategory = product.category.names,Gender = gender.names)
print("Our summarized data for analysis is")
bidata
mosaicplot(t(bidata),col=c("coral1","yellow","paleturquoise"),main="Black Friday Sales | Product Category vs Gender")
print("RESULT :")
print("Overall there are more male shoppers cleary from plot")
print("Product Category 2 being sold most")
print("Product category 3 sales are almost half of product category 2 in case of female shoppers")
print("Total sales of every product category")
margin.table(bidata,1)
print("Total sales by gender")
margin.table(bidata,2)

# RESCALING DATA
print("Rescaling data for better understanding")
rescale.bidata = round(bidata/100000)
print("Rescaled Data")
rescale.bidata
print("Original Data")
bidata
print("Every value now onwards for bivariate data represent 100,000s (Million[M]) value")

# MOSAICPLOT
mosaicplot(rescale.bidata,col=c("royalblue4","palevioletred1"),main="Mosaic Plot for Product Category vs Gender")
print("Total Sales (in Millions(M))")
addmargins(rescale.bidata)
print("RESULT : Approximately,")
print("Total there is sales of 85M products")
print("out of which product category 1,2,3 have 28M, 36M, 21M sales respectively")
print("and males brought 64M products while female 21M")
print("Percentage wise sales")
x = prop.table(rescale.bidata)*100
round(x)
print("Percentage wise sales for gender")
x = prop.table(rescale.bidata,1)*100
round(x)
print("Notable thing is values of Male are around 75% and Female 25% we analysed earlier too")
print("Percentage wise sales for category")
x = prop.table(rescale.bidata,2)*100
round(x)
print("RESULT : Each gender have almost same contribution in every category")

#3. Examining the distribution of data
#3.1 Analysis on "Year in current city"
temporary = years.in.current.city # keeping copy of data in a temporary variable
years.in.current.city = table(years.in.current.city)
# GEOMETRIC DITRIBUTION
df.years.in.current.city = data.frame(years.in.current.city)
df.years.in.current.city$probability = df.years.in.current.city$Freq/sum(df.years.in.current.city$Freq)
options(digits = 2)
print("Geometric Distribution")
print("Probability that the person I picked have stayed 5 years in current city")
print("First person")
dgeom(0, prob=df.years.in.current.city$probability[5])*100
print("Second person")
dgeom(1, prob=df.years.in.current.city$probability[5])*100

# checking for 10 persons
pmf.10 = dgeom(0:9, prob=df.years.in.current.city$probability[5])
print("till 10 persons")
person.stay.5.year = data.frame(
  person.count = seq(1:10),
  probability = pmf.10,
  percentage = pmf.10 * 100
)
person.stay.5.year
# plotting graph
plot(0:9,pmf.10,type="h",col="green",main="Choosed a person who spent 5 years in current city out of 10 trials",xlab="Person Count", ylab="PMF/ Probability",pch=16)
abline(h=0)

# unloadNamespace("package:stats")
# unloadNamespace("package:prob")
# detach("package:prob",unload = TRUE)
# BARPLOT
years.in.current.city = round(years.in.current.city/1000)
print("We can see an overview which years.in.current.city people spent most")
barplot(years.in.current.city,col="pink3",xlab="Number of Years spent",ylab="Frequency",main="Black friday sales | Years (Value in 1000s)")
print("RESULT : Most of the people have spent at least 2 years in the city where the survey took place")

# PIECHART
years.in.current.city.labels = c("1 Year","2 Years","3 Years","4 Years","5 Years")
years.in.current.city.percent = round(years.in.current.city/sum(years.in.current.city)*100)
years.in.current.city.labels = paste(years.in.current.city.labels, years.in.current.city.percent)
years.in.current.city.labels = paste(years.in.current.city.labels, "%", sep="")
print("Ploting a pie chart for years")
pie(years.in.current.city, labels = years.in.current.city.labels, col=rainbow(5), main="BlackFriday | Gender wise shoppers distribution")
print("RESULT : From pie chart we can say that 50% of popuation have spend atmost 2 years and 50% more than 2 years")

# copying original data in variable
years.in.current.city = temporary

#4. Draw various random samples of the data and show the applicability of the Central Limit Theorem for this variable.
print("Applying central limit theorem on Purchase Amount")

# total samples 
purchase.samples.count = nrow(table(purchase))
cat("Total Samples: ", purchase.samples.count)
# bringing data back to original state
temporary = purchase
print("For easy calculation rescaling purchases")
purchase = round(purchase/1000)
# calculating mean
purchase.mean = mean(purchase)
cat("Mean: ",purchase.mean)
# calculating std. deviation
purchase.sd = sd(purchase)
cat("Standard Deviation: ",purchase.sd)

# taking samples
# SAMPLE 1
print("Taking 0.5% of total sample")
sample.size = round(purchase.samples.count*0.005)
cat("Sample size= ", sample.size)
sample1 = sample(purchase, size = sample.size, replace = TRUE)
sample1.mean = mean(sample1)
sample1.sd = sd(sample1)
cat("Orginal Data Mean | Sample Mean = ", purchase.mean, " | ",sample1.mean)
cat("Orginal Data Std. Dev. | Sample Std. Dev. = ", purchase.sd, " | ",sample1.sd)
main = paste("0.5% of Shoppers | Sample size = ",sample.size)
hist(sample1, prob = TRUE, breaks=15, xlim=c(0,60),col="darkorange", main = main)

# SAMPLE 2
print("Taking 1% of total sample")
sample.size = round(purchase.samples.count*0.01)
cat("Sample size= ", sample.size)
sample2 = sample(purchase, size = sample.size, replace = TRUE)
sample2.mean = mean(sample2)
sample2.sd = sd(sample2)
cat("Orginal Data Mean | Sample Mean = ", purchase.mean, " | ",sample2.mean)
cat("Orginal Data Std. Dev. | Sample Std. Dev. = ", purchase.sd, " | ",sample2.sd)
main = paste("1% of Shoppers | Sample size = ",sample.size)
hist(sample2, prob = TRUE, breaks=15, xlim=c(0,60),col="darkorange", main = main)

# SAMPLE 3
print("Taking 5% of total sample")
sample.size = round(purchase.samples.count*0.05)
cat("Sample size= ", sample.size)
sample3 = sample(purchase, size = sample.size, replace = TRUE)
sample3.mean = mean(sample3)
sample3.sd = sd(sample3)
cat("Orginal Data Mean | Sample Mean = ", purchase.mean, " | ",sample3.mean)
cat("Orginal Data Std. Dev. | Sample Std. Dev. = ", purchase.sd, " | ",sample3.sd)
main = paste("5% of Shoppers | Sample size = ",sample.size)
hist(sample3, prob = TRUE, breaks=15, xlim=c(0,60),col="darkorange", main = main)

# SAMPLE 4
print("Taking 30% of total sample")
sample.size = round(purchase.samples.count*0.3)
cat("Sample size= ", sample.size)
sample4 = sample(purchase, size = sample.size, replace = TRUE)
sample4.mean = mean(sample4)
sample4.sd = sd(sample4)
cat("Orginal Data Mean | Sample Mean = ", purchase.mean, " | ",sample4.mean)
cat("Orginal Data Std. Dev. | Sample Std. Dev. = ", purchase.sd, " | ",sample4.sd)
main = paste("30% of Shoppers | Sample size = ",sample.size)
hist(sample4, prob = TRUE, breaks=15, xlim=c(0,60),col="darkorange", main = main)

# SAMPLE 5
print("Taking 75% of total sample")
sample.size = round(purchase.samples.count*0.75)
cat("Sample size= ", sample.size)
sample5 = sample(purchase, size = sample.size, replace = TRUE)
sample5.mean = mean(sample5)
sample5.sd = sd(sample5)
cat("Orginal Data Mean | Sample Mean = ", purchase.mean, " | ",sample5.mean)
cat("Orginal Data Std. Dev. | Sample Std. Dev. = ", purchase.sd, " | ",sample5.sd)
main = paste("75% of Shoppers | Sample size = ",sample.size)
hist(sample5, prob = TRUE, breaks=15, xlim=c(0,60),col="darkorange", main = main)
print("In all plots we saw the distribution is centered around original mean")
options(digits = 3)
print("Mean: Original, 0.5%, 1%, 5%, 30%, 75% of total no of purchases")
cat(purchase.mean, sample1.mean, sample2.mean, sample3.mean, sample4.mean, sample5.mean)
print("Std. Dev.: Original, 0.5%, 1%, 5%, 30%, 75% of total no of purchases")
cat(purchase.sd, sample1.sd, sample2.sd, sample3.sd, sample4.sd, sample5.sd)
print("Therefore we can see that")
print("If data is drawn from any distribution, then the distribution of the sample means has
the shape of a normal distribution for a large sample. The mean of the sample mean distribution is equal to the
      mean of the parent data. The higher the sample size, the narrower the spread of the sample means.")
# copy the original data into variable again
purchase = temporary

#5. Show how various sampling methods can be used on your data. What are your conclusions if these samples are used instead of the whole dataset.

#5.1 SAMPLING : Stay in Current City
# still using rescaled Purchase values
sample.size = round(0.07 * purchase.samples.count)#picking random sample size
cat("Sample size = ", sample.size)

# SIMPLE RANDOM SAMPLING
print("SIMPLE RANDOM SAMPLING")

#with replacement
print("With Replacement")
s = srswr(sample.size, nrow(blackfridaysales))
rows = (1:nrow(blackfridaysales))[s!=0]
rows = rep(rows,s[s!=0])
sample.with.replace = blackfridaysales[rows,]
print("For Example")
print("Years in Current City Original: ")
table(blackfridaysales$years.in.current.city)
print("Years in Current City Sampled Data")
table(sample.with.replace$years.in.current.city)

# Using Sample Data vs Original Data
print("Sample Data")
print("Gender Count")
table(sample.with.replace$gender)
print("Percentage wise Gender Distribution")
prop.table(table(sample.with.replace$gender))*100
print("Original Data")
print("Percentage wise Gender Distribution")
prop.table(table(blackfridaysales$gender))*100
print("RESULT : As we can see that distribution has changed in sample data but not much deflection wrt original")

# without replacement
print("Without Replacement")
s = srswor(sample.size, nrow(blackfridaysales))
rows = (1:nrow(blackfridaysales))[s!=0]
rows = rep(rows,s[s!=0])
sample.without.replace = blackfridaysales[rows,]
print("For Example")
print("Years in Current City Original: ")
table(blackfridaysales$years.in.current.city)
print("Years in Current City Sampled Data")
table(sample.without.replace$years.in.current.city)

# Using Sample Data vs Original Data
print("Sample Data")
print("Gender Count")
table(sample.without.replace$gender)
print("Percentage wise Gender Distribution")
prop.table(table(sample.without.replace$gender))*100
print("Original Data")
print("Percentage wise Gender Distribution")
prop.table(table(blackfridaysales$gender))*100
print("RESULT : As we can see that distribution has changed in sample data but not much deflection wrt to original")

# SYSTEMATIC SAMPLING
print("SYSTEMATIC SAMPLING")

# Equal Probability
print("Considering equal probabilities of every data")
N = nrow(blackfridaysales)
n = sample.size
k = ceiling(N/n)
r = sample(k,1)
s = seq(r, by=k, length=n)
sample.systematic = blackfridaysales[s,]
print("For Example")
print("Years in Current City Original: ")
table(blackfridaysales$years.in.current.city)
print("Years in Current City Sampled Data")
table(sample.systematic$years.in.current.city)

# Using Sample Data vs Original Data
print("Sample Data")
print("Gender Count")
table(sample.systematic$gender)
print("Percentage wise Gender Distribution")
prop.table(table(sample.systematic$gender))*100
print("Original Data")
print("Percentage wise Gender Distribution")
prop.table(table(blackfridaysales$gender))*100
print("RESULT : As we can see that distribution has changed in sample data with notable percentages")
print("Sample results vary a lot wrt to original data")

# UNEQUAL PROBABILITIES
print("Considering unequal probabilities for data")
pik = inclusionprobabilities(blackfridaysales$years.in.current.city, sample.size)
s= UPsystematic(pik)
sample.systematic.unequal.prob = blackfridaysales[s!=0,]
print("For Example")
print("Years in Current City Original: ")
table(blackfridaysales$years.in.current.city)
print("Years in Current City Sampled Data")
table(sample.systematic.unequal.prob$years.in.current.city)

# Using Sample Data vs Original Data
print("Sample Data")
print("Gender Count")
table(sample.systematic.unequal.prob$gender)
print("Percentage wise Gender Distribution")
prop.table(table(sample.systematic.unequal.prob$gender))*100
print("Original Data")
print("Percentage wise Gender Distribution")
prop.table(table(blackfridaysales$gender))*100
print("RESULT : As we can see that distribution has changed in sample data but not much deflection wrt original")

# STRATIFIED SAMPLING
temporary = blackfridaysales
temporary.size = sample.size
print("STRATIFIED SAMPLING")
sample.size = 10 #changin the sample size for this sampling to better understand
cat("Sample size for startified sampling = ",sample.size)

# ordering the data
blackfridaysales = blackfridaysales[order(blackfridaysales$years.in.current.city),]
freq <- table(blackfridaysales$years.in.current.city)
size <- sample.size * freq / sum(freq)
st1 = strata(blackfridaysales, size = size , stratanames = c("years.in.current.city"), method = "srswr", description = TRUE)
sample = getdata(blackfridaysales,st1)
sample

# Using Sample Data vs Original Data
print("Sample Data")
print("Gender Count")
table(sample$gender)
print("Percentage wise Gender Distribution")
prop.table(table(sample$gender))*100
print("Original Data")
print("Percentage wise Gender Distribution")
prop.table(table(blackfridaysales$gender))*100
print("RESULT : As we can see that distribution has changed by large percentages")
print("With this distribution we may have much different results")
print("Taking another example of Average Purchase")
print("Sample Data")
mean(sample$purchase)
print("Original Data")
print("Percentage wise Gender Distribution")
mean(blackfridaysales$purchase)
print("RESULT : Mean is differing in 1000s using this sample")
print("Thus much different interpretation if used this sample data")

# revert the data back to orginal
blackfridaysales = temporary
sample.size = temporary.size

# CLUSTER SAMPLING
temporary.size = sample.size
sample.size = 2 # should be less than no of unique value of the column which is basis of sampling
print("CLUSTER SAMPLING")
cl = cluster(blackfridaysales, c("years.in.current.city"), size=sample.size, method="srswor")
cl.sample = getdata(blackfridaysales, cl)
cat("Cluster Sampling for Years in Current City with sample size= ",sample.size)
table(cl.sample$years.in.current.city)
# head(cl.sample)
# tail(cl.sample)

# Using Sample Data vs Original Data
print("Sample Data")
print("Gender Count")
table(cl.sample$gender)
print("Percentage wise Gender Distribution")
prop.table(table(cl.sample$gender))*100
print("Original Data")
print("Percentage wise Gender Distribution")
prop.table(table(blackfridaysales$gender))*100
print("RESULT : As we can see that distribution has changed in sample data but not much deflection")

# revert the data back to original
sample.size = temporary.size
print("RESULT: Thus of all sampling in this data SYSTEMATIC SAMPLING WITH EQUAL PROBABILITY & STRATIFIED SAMPLING samples get deflected most as compared to original data")
print("Rest of the sampling data interpretations remains similar to the original data")

# Implementation of any feature(s) not mentioned in the specification
# Replacing city Names
print("City Category currently is A,B,C which is vague and does not give clear idea of its meaning")
print("So let's rename the category")
print("Originally the city categories are")
names(table(city))
print("Renaming them to A: urban, B: town, C: rural")
temp = city

# made a function to rename cities
renameCity = function (data,this,with){
  str_replace_all(data,this,with)
}
city = renameCity(city, 'A', "urban")
city = renameCity(city, 'B', "town")
city = renameCity(city, 'C', "rural")
print("Updated city is")
names(table(city))

# _____________________________________________________________________________________________________________________________________________________________________________
# TIBBLE
# _____________________________________________________________________________________________________________________________________________________________________________

print("Creating tibble of data")
bfs = as.tibble(blackfridaysales)
bfs

# Observation
print("Average year a shopper stayed in the current city")
print("For A")
dplyr::filter(bfs, city=="A") %>% summarise(mean(years.in.current.city))
print("For B")
dplyr::filter(bfs, city=="B") %>% summarise(mean(years.in.current.city))
print("For C")
dplyr::filter(bfs, city=="C") %>% summarise(mean(years.in.current.city))

# Observation
print("Average Purchase made in each City Category on the basis of Stay in the Current City")
print("For A")
dplyr::filter(bfs, city=="A") %>% group_by(years.in.current.city) %>% summarise(mean(purchase))
print("For B")
dplyr::filter(bfs, city=="B") %>% group_by(years.in.current.city) %>% summarise(mean(purchase))
print("For C")
dplyr::filter(bfs, city=="C") %>% group_by(years.in.current.city) %>% summarise(mean(purchase))


# PREDICTION
library(data.table)
library(caret)
library(randomForest)
library(rpart)
library(gbm)
library(dplyr)
library(ggplot2)
library(dummies)
library(h2o)
h2o.init()

# STEP 1 : Set the working directory and load the Train and Test Data
require(caTools)  # loading caTools library
set.seed(123)   #  set seed to ensure you always have same random numbers generated
# splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
sample = sample.split(original.data,SplitRatio = 0.75)
train =subset(original.data,sample ==TRUE) 
test=subset(original.data, sample==FALSE)

# Setting the Target Variable in Test Data
test$Purchase <- 1

# Creating an index so that test and train could be segregated later
train<- cbind(train,index =0)
test <- cbind(test, index=1)

#Merging test and train datasets
combi_data <- rbind(train,test)

#Exploratory Analysis
plot(x= train$Gender, y = train$Purchase, col="blue", xlab = "Gender", ylab = "Purchase")
ggplot(train, aes(x=Age, fill=Gender)) + geom_bar(position = "dodge")
ggplot(train, aes(x=Product_Category_1, fill=Gender)) + geom_bar(position = "dodge")
ggplot(train, aes(x=Product_Category_2, fill=Gender)) + geom_bar(position = "dodge")
ggplot(train, aes(x=Product_Category_3, fill=Gender)) + geom_bar(position = "dodge")


# Checking the Missing Values
# There are many missing values in Product Cat 2 and 3 which are actual 0 category
combi_data$Product_Category_2 <- as.character(combi_data$Product_Category_2)
combi_data$Product_Category_2[is.na(combi_data$Product_Category_2)]=0
combi_data$Product_Category_2 <- as.factor(combi_data$Product_Category_2)
combi_data$Product_Category_3 <- as.character(combi_data$Product_Category_3)
combi_data$Product_Category_3[is.na(combi_data$Product_Category_3)]=0
combi_data$Product_Category_3 <- as.factor(combi_data$Product_Category_3)

#Extracting Important Features
a<- aggregate(x= train$Purchase, by= list(train$Occupation,train$Age), FUN = median )
colnames(a) <- c("Occupation", "Age", "Purchase_Median")
a$Spending[a$Purchase_Median > 9500] <- "High"
a$Spending[a$Purchase_Median > 8000 & a$Purchase_Median < 9500] <- "Medium"
a$Spending[a$Purchase_Median < 8000] <- "Low"
a <- a[,c(1,2,4)]
a$Occupation <- as.numeric(a$Occupation)

b<- aggregate(x= train$Purchase, by= list(train$Product_ID), FUN = median )
b$Cost[b$x>= 20000] <- "VeryCostly"
b$Cost[b$x>=15000 & b$x< 20000] <- "Costly"
b$Cost[b$x>=10000 & b$x< 15000] <- "Average"
b$Cost[b$x < 10000] <- "Cheap"
b <- b[,c(1,3)]
colnames(b)[1] <- "Product_ID"
b$Product_ID <- as.character(b$Product_ID)

c<- aggregate(x= train$Purchase, by= list(train$Product_Category_1), FUN = median )
colnames(c)[1]<- "Product_Category_1"
c$Product_Cost[c$x> 15000] <- "CostlyProd"
c$Product_Cost[c$x> 8000 & c$x< 15000] <- "AverageCost"
c$Product_Cost[c$x< 8000] <- "CheapCost"
c <- c[,-2]

d <- aggregate(x=train$Purchase, by = list(train$City_Category, train$Stay_In_Current_City_Years), FUN = median)
colnames(d)[1]<- "City_Category"
colnames(d)[2]<- "Stay_In_Current_City_Years"
d$Feature[d$x> 8300] <- "A"
d$Feature[d$x<=8300] <- "B"
d <- d[,-3]
d$City_Category<- as.factor(d$City_Category)
d$Stay_In_Current_City_Years<- as.factor(d$Stay_In_Current_City_Years)

# Combining the extracted features to the main data set.
combi_data<- full_join(combi_data,a,by=c("Occupation","Age"))
combi_data<- full_join(combi_data,b,by=c("Product_ID"))
combi_data<- full_join(combi_data,c,by=c("Product_Category_1"))
combi_data<- full_join(combi_data,d,by=c("City_Category", "Stay_In_Current_City_Years"))

# Setting the right data types:
combi_data$Occupation<- as.factor(combi_data$Occupation)
combi_data$Stay_In_Current_City_Years = as.character(combi_data$Stay_In_Current_City_Years)
combi_data$Stay_In_Current_City_Years[combi_data$Stay_In_Current_City_Years=="4+"] <- "4"
combi_data$Stay_In_Current_City_Years = as.factor(combi_data$Stay_In_Current_City_Years)
combi_data$Marital_Status = as.factor(combi_data$Marital_Status )
combi_data$Product_Category_1 = as.factor(combi_data$Product_Category_1)
combi_data$Product_Category_2 = as.factor(combi_data$Product_Category_2)
combi_data$Product_Category_3 = as.factor(combi_data$Product_Category_3)
combi_data1 <- combi_data
combi_data1$Stay_In_Current_City_Years <- as.numeric(combi_data1$Stay_In_Current_City_Years)
combi_data1$Spending <- as.factor(combi_data1$Spending)
combi_data1$Cost <- as.factor(combi_data$Cost)
combi_data1$Product_Cost <- as.factor(combi_data1$Product_Cost)
combi_data1$Feature <- as.factor(combi_data1$Feature)
levels(combi_data1$Age)[levels(combi_data1$Age) == "0-17"] <- 0
levels(combi_data1$Age)[levels(combi_data1$Age) == "18-25"] <- 1
levels(combi_data1$Age)[levels(combi_data1$Age) == "26-35"] <- 2
levels(combi_data1$Age)[levels(combi_data1$Age) == "36-45"] <- 3
levels(combi_data1$Age)[levels(combi_data1$Age) == "46-50"] <- 4
levels(combi_data1$Age)[levels(combi_data1$Age) == "51-55"] <- 5
levels(combi_data1$Age)[levels(combi_data1$Age) == "55+"] <- 6
combi_data1$Age <- as.numeric(combi_data1$Age)
combi_data1$Gender <- as.numeric(combi_data1$Gender)
combi_data1$Product_Category_1 <- as.numeric(combi_data1$Product_Category_1)
combi_data1$Product_Category_2 <- as.numeric(combi_data1$Product_Category_2)
combi_data1$Product_Category_3 <- as.numeric(combi_data1$Product_Category_3) 
combi_data1$Occupation <- as.numeric(combi_data1$Occupation)
combi_data1$Marital_Status <- as.numeric(combi_data1$Marital_Status)

library(dummy)
#Dummy Variable
combi_data2 <- dummy.data.frame(combi_data1, names = c("City_Category", "Spending", "Cost", "Product_Cost", "Feature"), sep ="_")
combi_data2 <- combi_data2[,-23]

# Perparing Data for Modeling
modelDataTrain=combi_data2[combi_data2$index==0, ]
modelDataTest=combi_data2[combi_data2$index==1, ]

#Removing Index Variable created earlier
modelDataTrain = modelDataTrain[,-15]
modelDataTest = modelDataTest[,-15]
modelDataTrain=as.data.frame(modelDataTrain)
modelDataTest=as.data.frame(modelDataTest)

# Initiating H2o and setting the H2O data frames
localH2O <- h2o.init(nthreads=-1)
train.h2o <- as.h2o(modelDataTrain)
test.h2o <- as.h2o(modelDataTest)
colnames(train.h2o)

#Setting Dependent and Independent Variable
target <- 14
predictor <- c(3:13,15:26)

#Applying Basic Regression Model
regression.model <- h2o.glm( y = target, x = predictor, training_frame = train.h2o)
h2o.performance(regression.model)
predict.reg <- as.data.frame(h2o.predict(regression.model, test.h2o))

#Applying Random Forest Model with 100 trees
rforest.model <- h2o.randomForest(y=target, x=predictor, training_frame = train.h2o, ntrees = 100, mtries = 3, max_depth = 4, seed = 1122)
h2o.performance(rforest.model)
predict.rforest <- as.data.frame(h2o.predict(rforest.model,test.h2o))


# GBM Model
gbm.model <- h2o.gbm(y=target, x=predictor, training_frame = train.h2o, ntrees = 1000, max_depth = 4, learn_rate = 0.01, seed = 1122)
predict.gbm <- as.data.frame(h2o.predict(gbm.model, test.h2o))

# Deep Learning
dlearning.model <- h2o.deeplearning(y = target,x = predictor,training_frame = train.h2o,epoch = 60,
                                    hidden = c(100,100),activation = "Rectifier",seed = 1122)

predict.dl1 <- as.data.frame(h2o.predict(dlearning.model,test.h2o))

# Creating Submission File
predict <- 0.5*predict.gbm +0.5*predict.dl1
submission <- modelDataTest[,c("User_ID","Product_ID")]
submission<- cbind(submission, predict)
colnames(submission)[3] <- "Purchase"
write.csv(submission,file = "Submission.csv", row.names = F)

