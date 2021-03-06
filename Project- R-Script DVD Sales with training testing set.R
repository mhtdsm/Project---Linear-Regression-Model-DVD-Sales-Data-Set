#  Project to predict the sales of dvd based on the data provided in the sales_dataset
# Our sales Dataset has 4 variables we need to predict sales using the other 3 predictors available with us
# Independent Variable or predictors are : Advertising , plays & Attractiveness
#Dependent Variable or Response Variable is Sales
# importing the dataset using this command 

# sales_dataset<-read.csv(file.choose())   OR

sales_dataset<- read.csv("D:/Courses/acadgild/BA with R/Sessions/Session 23  - Project 2 3/Project 2/DVD sales data set/sales_dataset.csv")
dim(sales_dataset)
View(sales_dataset)
str(sales_dataset)
colnames(sales_dataset)
names(sales_dataset)


install.packages("caTools")
library(caTools)
set.seed(2)
split<-sample.split(sales_dataset,SplitRatio = 0.75)
split
training_sales_dataset<-subset(sales_dataset,split=="TRUE")
training_sales_dataset
dim(training_sales_dataset)
testing_sales_dataset<-subset(sales_dataset,split=="FALSE")
testing_sales_dataset
dim(testing_sales_dataset)


# plotting the box plot for Descriptive Analysis of Predictors in training dataset
boxplot(training_sales_dataset$advertise,training_sales_dataset$plays,training_sales_dataset$attractiveness,main= "Boxplot of Predictors (training dataset)",col=heat.colors(3))
#1 = Advertise   2=plays   3=attractiveness

# Describing the predictors one by one , plotting their Histogram  & boxplot in training dataset
library(psych)
describe(training_sales_dataset$advertise)
hist(training_sales_dataset$advertise,xlab = "Advertising Budget in thousands",ylab="Frequency",main="Histrogram of Advertise (training dataset)",col="red")
boxplot(training_sales_dataset$advertise,col="red",main="Boxplot of Advertising Budget ",xlab="advertise")  # we can see 2-3 Outliers in the dats

describe(training_sales_dataset$plays)
hist(training_sales_dataset$plays,xlab = " no of times Played on Radio Mirchi",ylab="Frequency",main="Histrogram of Plays (training dataset)",col="green")
boxplot(training_sales_dataset$plays,col="green",main="Boxplot of No of times played (training dataset)",xlab="plays")


describe(sales_dataset$attractiveness)
hist(training_sales_dataset$attractiveness,xlab = " Attractiveness of the brand",ylab="Frequency",main="Histrogram of Attractiveness (training dataset)",col="yellow")
boxplot(training_sales_dataset$attractiveness,col="yellow",main="Boxplot of Attractiveness (training dataset)",xlab="attractiveness") # we can see 2 outliers in our data


#Scatter Plot of predictor Variable Vs Response Variable to check what type of relationship is their between them
plot(training_sales_dataset$advertise,training_sales_dataset$sales,main= "Advertising Budget Vs Sales (training dataset)" , xlab = "Advertising Budget", ylab = "Sales",col="red",abline(lm(sales~advertise,data=training_sales_dataset)))
plot(training_sales_dataset$plays,training_sales_dataset$sales,main= "No of song plays Vs Sales (training dataset)" , xlab = "No of song plays", ylab = "Sales",col="brown",abline(lm(sales~plays,data=training_sales_dataset)))
plot(training_sales_dataset$attractiveness,training_sales_dataset$sales,main="Attractivenss Vs Sales (training dataset) ",xlab= " Atractiveness",ylab="Sales",col="red",abline(lm(sales~attractiveness,data=training_sales_dataset)))


#Checking correalation of each of predictors with Response Variable in training dataset
cor(training_sales_dataset$advertise,training_sales_dataset$sales)
cor(training_sales_dataset$plays,training_sales_dataset$sales)
cor(training_sales_dataset$attractiveness,training_sales_dataset$sales)

# We can see that there is not much of correlation between the predictor variables


#storing all the correlation between the variables in the cr table
cr<-cor(training_sales_dataset)
cr

#plotting a scatter matrix to understand the pattern or the relationship between the variables and the response variable
library(lattice)
splom(~training_sales_dataset[c(1:150),],groups=NULL,data = training_sales_dataset,axis.line.tck=0,axis.text.alpha=0)


install.packages("corrplot")
library(corrplot)
cor.plot(cr,type="lower",main= " Correlation plot or the training_Sales_dataset ")

# finding multicollinearity by removing sales from the training_sales_dataset & checking correlation between predictor variables only
#we created a new dataset sales_dataset_a to find multicollinearity among the variable
install.packages("caret")
library(caret)
training_sales_dataset_a=subset(training_sales_dataset,select = -c(sales))
numericdata<-training_sales_dataset_a[sapply(training_sales_dataset_a,is.numeric)]
descrCor<-cor(numericdata)
descrCor
cor.plot(descrCor,type="lower")
# we can see that predictor variables i.e. advertise,plays,attractiveness are not highly correlated  
# meaning multi collinarity doesnot exist among the variables


#building the Linear regression model
install.packages("CARS")
library(CARS)
fit<-lm(sales~advertise+plays+attractiveness,data=training_sales_dataset)
summary(fit)

#Verifying the Variation Inflation Factor for our model "fit"
install.packages("car")
library(car)
vif(fit)
# all the variables in the dataset has VIF less then 5 so all the  variable are not correlated highly
# Also the model which we built has all the three predictors as significant p-value hence we can continue with  our model "fit" for predicting


#Trying  to optimize the model  "fit" by getting Multiple R-squared value as high along with other paramets satisfied like p-value significance,F-value significance etc
model1<-lm(sales~advertise,data=training_sales_dataset)
summary(model1)

model2<-lm(sales~plays,data=training_sales_dataset)
summary(model2)

model3<-lm(sales~attractiveness,data=training_sales_dataset)
summary(model3)

model4<-lm(sales~advertise+plays,data=training_sales_dataset)
summary(model4)

model5<-lm(sales~plays+attractiveness,data=training_sales_dataset)
summary(model5)

model6<-lm(sales~advertise+attractiveness,data=training_sales_dataset)
summary(model6)

#Now we have build all the models possible with the 3 predictors available . All have R-Squared value less than "fit" 
summary(fit)
# valuee of  Multiple R sqaure for "fit" 0.6657 meaning that the model is able to explain 66.57% of the variance in the sales in training dataset


# final equation of our model fit is :  
#  sales = -36.634675 + (0.089398 * advertise) + (3.244344 * plays) + (12.461907 * attractiveness)


#  Checking Durban Watson statistics 
dwt(fit)  # durban Watson Statistics for our model is 2.060746
# The DWS statistics is the number that tests the autocorrelation in the errors/residuals from a regression analysis.
#It's value will always lie between "0 & 4"" .DWS value of '2' means there is no autocorrelation in the sample,
#whereas value towards "zero"  from  '2' means there is +ve autocorrelation & "towards 4"" from '2' means -ve  autocorrelation.
#Autocorrelation is a characteristic of data in which the correlation between the values of the same variables is based on related objects. 


#Finding the predicted Value of sales through the model made  "fit"
training_sales_dataset$predictedsales <- predict(fit)
training_sales_dataset$predictedsales

# error values of sales through the model is below. These values are giving us the residuals/errors 
#i.e difference between predicted and Actual sales

training_sales_dataset$error<- residuals(fit)
training_sales_dataset$error

# Adding observation No. Column n our dataset
training_sales_dataset$obsno<-c(1:150)
training_sales_dataset$obsno


View(training_sales_dataset)

#comparing model predicted values with actual values of sales using the graph for training_sales_dataset
#we can see the line curve as below
plot(training_sales_dataset$sales,type="l",lty=1.8,col="green") # Actual sales in green color in training dataset
lines(training_sales_dataset$predictedsales,type="l",col="blue") # predicted sales by 'fit' model in blue color

#most of the lines are overlapping meaning our model "fit" is a good model based on the predictors given to us in sales_dataset for training_sales_dataset

### Assumptions Test for Model "fit" for training dataset

####1.  Normality test for model
hist(training_sales_dataset$error,main = "Normality check for our model (training dataset)", xlab="Residuals",col="orange")

####2.  Independence of observations 
plot(training_sales_dataset$obsno,training_sales_dataset$error,main="Independence of  error for model (training dataset)",xlab= " obsv no", ylab="residuals",col="red")

####3 Check of linear relationship 

plot(training_sales_dataset$advertise,training_sales_dataset$sales,main="Linear Rltnship for model (trainingDataset)",xlab="advertise (in thousands)",ylab="Sales",col="red")
plot(training_sales_dataset$plays,training_sales_dataset$sales,main="Linear Rltnship for model (training dataset)",xlab="plays ",ylab="Sales(thousands) ",col="red")
plot(training_sales_dataset$attractiveness,training_sales_dataset$sales,main="Linear Rltnship for model (training dataset)",xlab="attractiveness ",ylab="Sales(thousands) ",col="red")


####4 Check of Constant Error Variance : Homoscedacity

plot(training_sales_dataset$predictedsales,training_sales_dataset$error,main="Constant error variance for model (training dataset)",xlab="Predited sales",ylab="errors",abline(h=0),col="red")


# for finding the confidence intervals & the predited values for model for training dataset

confint(fit)
fitted(fit)
predicted_training<- predict(fit, interval="confidence")
predicted_training  #with lower & upper limit of prediction by model "fit"
dim(predicted_training)
predicted_training<-as.data.frame(predicted_training)

# comparing the predicted values from "fit & the actual values of sales in training_sales_dataset in the dataframe
predicted_training$actualtrainingsales<-training_sales_dataset$sales
predicted_training # now with actual sales values from training_sales_dataset



# Checking basics details about testing_sales_dataset
testing_sales_dataset
dim(testing_sales_dataset)
View(testing_sales_dataset)
summary(testing_sales_dataset)

#Verifying our model on testing data set so that we get to know the accuracy and predicted values of our testing dataset and compare them with the actual sales values in testing dataset
#Finding the predicted Value of sales through the model made  "fit"
predict_testing<-predict(fit,testing_sales_dataset)
predict_testing
testing_sales_dataset$predicted_testing_sales <- predict_testing
testing_sales_dataset
dim(testing_sales_dataset)
View(testing_sales_dataset)

# error values of sales through the model is below. These values are giving us the residuals/errors 
#i.e difference between predicted and Actual sales

testing_sales_dataset$error_sales<-testing_sales_dataset$sales - testing_sales_dataset$predicted_testing_sales
testing_sales_dataset

# Adding observation No. Column n our dataset
testing_sales_dataset$obsno<-c(1:50)
testing_sales_dataset$obsno

#comparing model predicted values with actual values of sales using the graph for training_sales_dataset
#we can see the line curve as below
plot(testing_sales_dataset$sales,type="l",lty=1.8,col="brown") # actual sales value in testing dataset
lines(testing_sales_dataset$predicted_testing_sales,type="l",col="blue") #predicted sales value for testing dataset by "fit' model

#most of the lines are overlapping while comparing sales of testing dataset
#meaning our model "fit" is a good model based on the predictors given to us in dataset

