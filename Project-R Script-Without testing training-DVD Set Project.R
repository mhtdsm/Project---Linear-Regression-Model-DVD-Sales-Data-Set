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

# plotting the box plot for Descriptive Analysis of Predictors
boxplot(sales_dataset$advertise,sales_dataset$plays,sales_dataset$attractiveness,main= "Boxplot of Predictors",col=heat.colors(3))
#1 = Advertise   2=plays   3=attractiveness


# Describing the predictors one by one , plotting their Histogram  & boxplot
library(psych)
describe(sales_dataset$advertise)
hist(sales_dataset$advertise,xlab = "Advertising Budget in thousands",ylab="Frequency",main="Histrogram of Advertise",col="red")
boxplot(sales_dataset$advertise,col="red",main="Boxplot of Advertising Budget",xlab="advertise")

describe(sales_dataset$plays)
hist(sales_dataset$plays,xlab = " no of times Played on Radio Mirchi",ylab="Frequency",main="Histrogram of Plays",col="green")
boxplot(sales_dataset$plays,col="green",main="Boxplot of No of times played",xlab="plays")


describe(sales_dataset$attractiveness)
hist(sales_dataset$attractiveness,xlab = " Attractiveness of the brand",ylab="Frequency",main="Histrogram of Attractiveness",col="yellow")
boxplot(sales_dataset$attractiveness,col="yellow",main="Boxplot of Attractiveness",xlab="attractiveness")

#Scatter Plot of predictor Variable Vs Response Variable to check what type of relationship is their between them
plot(sales_dataset$advertise,sales_dataset$sales,main= "Advertising Budget Vs Sales" , xlab = "Advertising Budget", ylab = "Sales",col="red",abline(lm(sales~advertise,data=sales_dataset)))
plot(sales_dataset$plays,sales_dataset$sales,main= "No of song plays Vs Sales" , xlab = "No of song plays", ylab = "Sales",col="brown",abline(lm(sales~plays,data=sales_dataset)))
plot(sales_dataset$attractiveness,sales_dataset$sales,main="Attractivenss Vs Sales",xlab= " Atractiveness",ylab="Sales",col="red",abline(lm(sales~attractiveness,data=sales_dataset)))


#Checking correalation of each of predictors with Response Variable

cor(sales_dataset$advertise,sales_dataset$sales)
cor(sales_dataset$plays,sales_dataset$sales)
cor(sales_dataset$attractiveness,sales_dataset$sales)

# We can see that there is not much of correlation between the predictor variables


#plotting to see autocorrealtion among the sales values 
plot(sales_dataset,sales_dataset$sales)

#storing all the correlation between the variables in the cr table
cr<-cor(sales_dataset)
cr

#plotting a scatter matrix to understand the pattern or the relationship between the variables and the response variable
library(lattice)
splom(~sales_dataset[c(1:200),],groups=NULL,data = sales_dataset,axis.line.tck=0,axis.text.alpha=0)


install.packages("corrplot")
library(corrplot)
cor.plot(cr,type="lower",main= " Correlation plot or the Sales_dataset ")

# finding multicollinearity by removing sales from the sales_dataset & checking correlation between predictor variables only
#we created a new dataset sales_dataset_a to find multicollinearity among the variable
install.packages("caret")
library(caret)
sales_dataset_a=subset(sales_dataset,select = -c(sales))
numericdata<-sales_dataset_a[sapply(sales_dataset_a,is.numeric)]
descrCor<-cor(numericdata)
descrCor
cor.plot(descrCor,type="lower")
# we can see that predictor variables i.e. advertise,plays,attractiveness are not highly correlated  
# meaning multi collinarity doesnot exist among the variables


#building the model
install.packages("CARS")
library(CARS)
fit<-lm(sales~advertise+plays+attractiveness,data=sales_dataset)
summary(fit)

#Verifying the Variation Inflation Factor for our model "fit"
install.packages("car")
library(car)
vif(fit)
# all the variables in the dataset has VIF less then 5 so all the  variable are not correlated highly
# Also the model which we built has all the three predictors as significant p-value hence we can continue with  our model "fit" for predicting


#Trying  to optimize the model  "fit" by getting Multiple R-squared value as high along with other paramets satisfied like p-value significance,F-value significance etc
model1<-lm(sales~advertise,data=sales_dataset)
summary(model1)

model2<-lm(sales~plays,data=sales_dataset)
summary(model2)

model3<-lm(sales~attractiveness,data=sales_dataset)
summary(model3)

model4<-lm(sales~advertise+plays,data=sales_dataset)
summary(model4)

model5<-lm(sales~plays+attractiveness,data=sales_dataset)
summary(model5)

model6<-lm(sales~advertise+attractiveness,data=sales_dataset)
summary(model6)

#Now we have build all the models possible with the 3 predictors available . All have R-Squared value less than "fit" 
summary(fit)
# valuee of  Multiple R sqaure for "fit" is .6645 meaning that the model is able to explain 66.45% of the variance


# final equation of our model fit is :  
#  sales = -28.140377 + (0.084642 * advertise) + (3.385493 * plays) + (11.333342 * attractiveness)


#  Checking Durban Watson statistics 
dwt(fit)  # durban Watson Statistics for our model is 1.947396
# The DWS statistics is the number that tests the autocorrelation in the errors/residuals from a regression analysis.
#It's value will always lie between "0 & 4"" .DWS value of '2' means there is no autocorrelation in the sample,
#whereas value towards "zero"  from  '2' means there is +ve autocorrelation & "towards 4"" from '2' means -ve  autocorrelation.
#Autocorrelation is a characteristic of data in which the correlation between the values of the same variables is based on related objects. 


#Finding the predicted Value of sales through the model made  "fit"
sales_dataset$predictedsales <- predict(fit)
sales_dataset$predictedsales

# error values of sales through the model is below. These values are giving us the residuals/errors 
#i.e difference between predicted and Actual sales

sales_dataset$error<- residuals(fit)
sales_dataset$error

# Adding observation No. Column n our dataset
sales_dataset$obsno<-c(1:200)
sales_dataset$obsno

#comparing model predicted values with actual values of sales using the graph 
#we can see the line curve as below
plot(sales_dataset$sales,type="l",lty=1.8,col="green") #actual sales values in sales_dataset
lines(sales_dataset$predictedsales,type="l",col="blue") #predicted sales values in sales_dataset
#most of the lines are overlapping meaning our model "fit" is a good model based on the predicors given to us in dataset


###Test for Assumptions  for Model "fit"

####1.  Normality test for model
hist(sales_dataset$error,main = "Normality check for our model", xlab="Residuals",col="orange")

####2.  Independence of observations 
plot(sales_dataset$obsno,sales_dataset$error,main="Independence of  error for model",xlab= " obsv no", ylab="residuals",col="red")

####3 Check of linear relationship 

plot(sales_dataset$advertise,sales_dataset$sales,main="Linear Rltnship for model",xlab="advertise (in thousands)",ylab="Sales",col="red")
plot(sales_dataset$plays,sales_dataset$sales,main="Linear Rltnship for model",xlab="plays ",ylab="Sales(thousands) ",col="red")
plot(sales_dataset$attractiveness,sales_dataset$sales,main="Linear Rltnship for model",xlab="attractiveness ",ylab="Sales(thousands) ",col="red")


####4 Check of Constant Error Variance : Homoscedacity

plot(sales_dataset$predictedsales,sales_dataset$error,main="Constant error variance for model",xlab="Predited sales",ylab="errors",abline(h=0),col="red")


# for finding the confidence intervals & the predited values for model

confint(fit)
fitted(fit)
predict(fit, interval="confidence")

#most of the lines are overlapping meaning our model "fit" is a good model based on the predictors given to us in dataset

# Thanks
