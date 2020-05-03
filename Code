#Analysis of COVID cases in India

#Importing data from COVID 19 india.org
library(jsonlite)
dataframe <- fromJSON("https://api.covid19india.org/states_daily.json", flatten= TRUE)
dataframe1 <- as.data.frame(dataframe)
str(dataframe1)

#Rearranging data using dplyr
library(dplyr)
library(tibble)
dataframe2 <- as_tibble(dataframe1)
dataframe3 <- select(dataframe2,states_daily.date,everything())
str(dataframe3)
tail(dataframe3)

#Changing the data types of the variables
dataframe3$states_daily.tt <- as.numeric(dataframe3$states_daily.tt)
#Selecting the variables for further filtering
dataframe4 <- select(dataframe3, -states_daily.date, -states_daily.tt, -states_daily.status)
str(dataframe4)
tail(dataframe4)
data <- lapply(dataframe4, as.numeric)
dataframe5 <- as.data.frame(data)
str(dataframe5)
tail(dataframe5)
#Filtering only those variables (states) which constitute atleast 5% of the total number of cases
retain_columns <- sapply(dataframe5, function(col) sum(col, na.rm = TRUE))
retain_columns <- names(retain_columns[retain_columns >= 0.5*sum(dataframe3$states_daily.tt, na.rm= TRUE)])
retain_columns
dataframe6 <- select(dataframe5, retain_columns)
# Changing the data types of other variables like date and status
dataframe3$states_daily.date <- as.Date(dataframe3$states_daily.date, "%d-%b-%y")
date <- dataframe3$states_daily.date
date
dataframe3$states_daily.status
dataframe3$states_daily.factored <- factor(dataframe3$states_daily.status, levels =c("Confirmed","Decreased","Recovered"))
dataframe3$states_daily.factored
dataframe3$states_daily.numeric <- as.numeric(dataframe3$states_daily.factored)
dataframe3$states_daily.numeric
#Arriving at the final data frame
dataframe.final <- cbind(dataframe6,date, dataframe3$states_daily.numeric, dataframe3$states_daily.tt)
head(dataframe.final)
tail(dataframe.final)

#Renaming certain variables/columns for reference
names(dataframe.final)[names(dataframe.final) == "dataframe3$states_daily.numeric"] <- "Status"
names(dataframe.final)[names(dataframe.final) == "dataframe3$states_daily.tt"] <- "Total"
view(dataframe.final)
head(dataframe.final)

#Filtering only confirmed cases and retaining only date and total variables f or the time series analysis

dataframe.ts <- ts(dataframe.final %>%filter (Status ==1) %>% select(Total))
plot (dataframe.ts, ylab="Total number of cases", xlab "No of days since 1st case")

#Reducing the variance
ts.final <- (log(dataframe.ts))
plot(log(dataframe.ts), ylab="Total number of cases", xlab "No of days since 1st case", main ="with logarithmic scale")

#Making the series stationary
ts.final.1 <- diff(log(dataframe.ts))
plot(diff(log(dataframe.ts)), ylab="Total number of cases", xlab "No of days since 1st case", main ="with logarithmic scale and differecing")

#Every 6th day there is overshoot of the data
ts.final.2 <- diff(diff(log(dataframe.ts)),6)
par(mfrow =c(2,1))
plot(diff(log(dataframe.ts)),main="Single Differencing", ylab ="Total number of Cases", xlab ="No. of days since 1st case")
plot(ts.final.2, main="Double Differencing", ylab ="Total number of Cases", xlab ="No. of days since 1st case")
#Though the double difference didn't help much, but we will one model with seasonal difference as 6 

acf(ts.final, lag.max =31)
pacf(ts.final, lag.max=31)

#Furthering upon Time-Series Modelling using ARIMA
library(astsa)
libray(forecast)
select.model <- arima(dataframe.ts, order= c(0,1,0), seasonal =list(order = c(0,0,0), period =6)
plot(forecast(Selected.model),ylab="Total number of Cases", xlab="No of days since first case")
(forecast(Selected.model), h=20)

#Second part
#Establishing the model between the number of infections as dependent variable in a region/city
#Number of Flights (Indicative of international exposure/travelling) and temprature as independent variable

library(readxl) 
#Source: Data_international_Exposure_Weather_final.xlsx)

#We have 3 variables; two variables (Cases and no. of flights) as integers and one variable (Temperature) as continuous
#Applying different modelling techniques

#Regression Tree model
options(Scipen=10) #No scientific notation
library(tree)
set.seed(90)
regression.tree.data <- read.xlsx("File location and name", col_names =TRUE)
str(regression.tree.data)
n.train <- round(nrow(regression.tree.data)*0.7,0)
n.train
regression.tree.training.sample <- sample(nrow(regression.tree.data),n.train)
regression.tree.training.sample
Reression.tree.training.data <- Regression.tree.data[Regression.tree.training.sample,]
str(Reression.tree.training.data)
regression.tree.validation.data <- regression.tree.data [-Regression.tree.training.sample,]
str(regression.tree.validation.data)

#Performing tree() on training data
mean(Regression.tree.training.data$Cases)

#Performing prediction
library(rpart)
library(rpart.plot)
library(forecast)
Cases.tree <- rpart(Cases ~., data=Regression.tree.training.data, control= rpart.control(minbucket=5))
Cases.tree$frame
mean(Regression.tree.training.data$Cases)

#Plotting tree
rpart.plot(Cases.tree)

#Performing prediction and checking accuracy
Regression.tree. Validation.prediction <- predict(Cases.tree, Regression.tree.validation.data)
Regression.tree. Validation.prediction

#Performing Linear regression
library(ggplot2)
library(corrplot)
library(isdals)
library("PerformanceAnalytics")

#Correlation between variables and their normality

correls <- cor(Regression.tree.data)
(cor(Regression.tree.data))
corrplot(correls,tl.srt=50, type ="lower")
chart.Correlation(Regression.tree.data, histogram=TRUE, pch=25)

#Though cases and no. of flights don't appear to be normal, still we would perform linear regression
#For checking normality, we could perform QQ plot as well

linear.regression.model <- lm(Cases~., data=Regression.tree.training.data)
summary(linear.regression.model)

#predicting and checking accuracy with the validation data
predict.linear.Validation <- predict (linear.regression.model, Regression.tree.validation.data)
predict.linear.Validation
cat("Root Mean Square of Error, rpart model=", Regression.tree.Validation.prediction.accuracy[2])
cat("Root Mean Square of Error, rpart model=", predict.linear.Validation.accuracy[2])

#1st part of the study - The time series trend of the cases are shown and predictiions have been done accordingly

#2nd part of the study - The accuracy of the linear regression model is better than that of Regression tree model

