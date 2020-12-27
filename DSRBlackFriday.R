#Loading the Dataset (from Kaggle)
original.data <- read.csv(file='C:/Users/MJ/Desktop/Dataset/BlackFriday.csv', header=TRUE, sep=",")

#Installing necessary packages
library("UsingR") # to use R and its packages
library(sampling) 
library(stringr) #for character manipulation and pattern matching
library(tidyverse)#set of packages that share API design 
library(stats) # R statistical functional 
library(prob) # probability calculations
library(dtplyr) #data frame manipulation
library(dbplyr) #data frame manipulation

#Data
print("Black Friday Sales Data")
head(original.data)

print("Columns we have in our data")
names(original.data)

attach(original.data) 


#DATA PRE_PROCESSING 
user.id = User_ID
product.id = Product_ID
gender = Gender
age.range = Age
occupation = Occupation
city = City_Category
years.in.current.city = as.numeric(Stay_In_Current_City_Years)
marital.status = Marital_Status
product.category.1 = Product_Category_1

print("Data Cleaning")
# convert NA to 0 to perform calculations 
product.category.2 = Product_Category_2
product.category.2[which(is.na(product.category.2))] = 0
product.category.3 = Product_Category_3
product.category.3[which(is.na(product.category.3))] = 0
purchase = Purchase

#making dataframe of data
blackfridaysales = data.frame(user.id, product.id, gender, occupation, city, years.in.current.city, marital.status, product.category.1, product.category.2, product.category.3, purchase)


#DATA EXPLORATION
head(table(user.id))

head(table(product.id))

table(gender)

table(age.range)

table(occupation)

table(city)

table(years.in.current.city)

table(marital.status)

table(product.category.1)

table(product.category.2)

table(product.category.3)

head(table(purchase),n=10)


# 1.1 Analysis on "Gender"
temporary = gender 
gender = table(gender)
gender.labels = c(" Female", " Male")
gender.percent = round(gender/sum(gender)*100)
gender.labels = paste(gender.labels, gender.percent)
gender.labels = paste(gender.labels, "%", sep="")
pie(gender, labels = gender.labels, col=c("Pink","Blue"), main="BlackFriday | Gender wise shoppers distribution")
gender = temporary 

#1.2 Analysis on "Age"
barplot(table(age.range), col="Red", main="Black Friday Shoppers by Age Range", xlab="Age Ranges", ylab="Count")

#1.3 Analysis on "Purchase"
# average purchase
cat("Average dollars shoppers spent = ", mean(purchase))
# purchase range
range.purchase = range(purchase)
cat("Range of amount shoppers spent = ", range.purchase)

# Barplot
barplot(table(purchase), border = c("darkgreen"), main="Purchase made in $ by shoppers", xlab = "Amount", ylab="Frequency of people")

# HISTOGRAMS 
# breaks = 20
hist(purchase,breaks=20, xlim=c(185,25000),col="darkorange", main="Purchase made in $ by shoppers(Breaks=20)", xlab="Amount", ylab="Frequency of people")
# breaks = 40
hist(purchase,breaks=40, xlim=c(185,25000),col="darkorange", main="Purchase made in $ by shoppers(Breaks=40)", xlab="Amount", ylab="Frequency of people")

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
f = fivenum(purchase) 
oulier = c(f[2]-1.5*(f[4]-f[2]) , f[4]+1.5*(f[4]-f[2]))
boxplot(f,horizontal = TRUE, xaxt="n", xlab="Amount", col="yellow", main="Purchase made in $ by shoppers")
axis(side = 1, at = f, labels=TRUE)
text(f,srt=90, rep(1.2,5), adj=0,labels=c("Min", "Lower Hinge", "Mean","Upper Hinge", "Max"))

#2. Analysis of variables and corresponding plots
print("Analysing Multivariate Data")
g = as.vector(as.character(gender))
pc1 = as.vector(as.numeric(as.character(product.category.1)))
pc2 = as.vector(as.numeric(as.character(product.category.2)))
pc3 = as.vector(as.numeric(as.character(product.category.3)))

temp.data = data.frame(gender = g, product.category.1 = pc1, product.category.2 = pc2, product.category.3 = pc3)
head(temp.data)
g = temp.data$gender
pc1 = temp.data$product.category.1
pc2 = temp.data$product.category.2
pc3 = temp.data$product.category.3

m.pc.1 = sum(pc1[which( g == 'M')]) #the sum of PC1 bought by all men
m.pc.2 = sum(pc2[which( g == 'M')])
m.pc.3 = sum(pc3[which( g == 'M')])

f.pc.1 = sum(pc1[which( g == 'F')])
f.pc.2 = sum(pc2[which( g == 'F')])
f.pc.3 = sum(pc3[which( g == 'F')])

bidata = rbind(c(m.pc.1,f.pc.1),c(m.pc.2,f.pc.2),c(m.pc.3,f.pc.3))
bidata

gender.names = c("male","female")
product.category.names = c("product category 1","product category 2","product category 3")
colnames(bidata) = gender.names
rownames(bidata) =  product.category.names
dimnames(bidata) = list(ProductCategory = product.category.names,Gender = gender.names)
bidata
mosaicplot(t(bidata),col=c("coral1","yellow","paleturquoise"),main="Black Friday Sales | Product Category vs Gender")

# RESCALING DATA
rescale.bidata = round(bidata/100000)
print("Rescaled Data")
rescale.bidata
print("Original Data")
bidata

#MOSAICPLOT
mosaicplot(rescale.bidata,col=c("royalblue4","palevioletred1"),main="Mosaic Plot for Product Category vs Gender")
print("Total Sales (in Millions(M))")
addmargins(rescale.bidata)

#3. Examining the distribution of data 
#3.1 Analysis on "Year in current city"
temporary = years.in.current.city # keeping copy of data in a temporary variable
years.in.current.city = table(years.in.current.city)
df.years.in.current.city = data.frame(years.in.current.city)
years.in.current.city = round(years.in.current.city/1000)

# PIECHART
years.in.current.city.labels = c("1 Year","2 Years","3 Years","4 Years","5 Years")
years.in.current.city.percent = round(years.in.current.city/sum(years.in.current.city)*100)
years.in.current.city.labels = paste(years.in.current.city.labels, years.in.current.city.percent)
years.in.current.city.labels = paste(years.in.current.city.labels, "%", sep="")
pie(years.in.current.city, labels = years.in.current.city.labels, col=rainbow(5), main="BlackFriday | Year wise shoppers distribution")

years.in.current.city = temporary

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

