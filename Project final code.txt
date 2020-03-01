#importing the libraries

library(boot) 
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)

#Setting the directory

setwd("C:\\Users\\SONY\\Documents\\IVY\\R\\Project")

#importing the dataset

df<-read.csv("Fn-UseC_-Marketing-Customer-Value-Analysis.csv")
data<-df
head(data)

#checking the datatypes and details of the variables
str(data)

#Converting the datatype
data$Effective.To.Date<-as.Date(data$Effective.To.Date, format = "%m/%d/%y")
str(data)

#Checking for missing values

sapply(data, function(x) sum(is.na(x)))
summary(data)

#Detecting outliers
boxplot(data$Customer.Lifetime.Value)
quantile(data$Customer.Lifetime.Value, c(0,0.5,0.75,0.8,0.85,0.89,0.9,0.925,0.95,0.97,0.975,0.98,0.985,0.99,0.995,1))

#Removing outliers
data1<-data[data$Customer.Lifetime.Value<18062.002,]

boxplot(data1$Customer.Lifetime.Value)

#Renaming the dependent variable column
names(data1)[names(data1)=="Customer.Lifetime.Value"]<-"CLV"
summary(data1)

boxplot(data1$Income)

boxplot(data1$Monthly.Premium.Auto)
quantile(data1$Monthly.Premium.Auto, c(0,0.5,0.75,0.85,0.9,0.95,0.96,0.965,0.97,0.975,0.98,0.985,0.99,1))
data2<-data1[data1$Monthly.Premium.Auto<182.00,]

boxplot(data2$Monthly.Premium.Auto)
quantile(data2$Monthly.Premium.Auto, c(0,0.5,0.75,0.8,0.85,0.9,0.95,0.975,0.98,0.99,0.995,0.997,1))
data3<-data2[data2$Monthly.Premium.Auto<173,]

boxplot(data3$Monthly.Premium.Auto)
summary(data3)

boxplot(data3$Months.Since.Last.Claim)
boxplot(data3$Months.Since.Policy.Inception)
summary(data3)

boxplot(data3$Number.of.Open.Complaints)
data3$Number.of.Open.Complaints<-as.factor(data3$Number.of.Open.Complaints)
data3$Number.of.Policies<-as.factor(data3$Number.of.Policies)
str(data3)
summary(data3)

boxplot(data3$Total.Claim.Amount)
quantile(data3$Total.Claim.Amount, c(0,0.5,0.75,0.8,0.85,0.9,0.95,0.975,0.98,0.985,0.99,0.995,1))
data4<-data3[data3$Total.Claim.Amount<1142.038860,]

boxplot(data4$Total.Claim.Amount)
quantile(data4$Total.Claim.Amount, c(0,0.5,0.75,0.8,0.95,0.96,0.965,0.97,0.975,0.98,0.985,0.99,0.995,1))
data5<-data4[data4$Total.Claim.Amount<982.639809,]

boxplot(data5$Total.Claim.Amount)
summary(data5)
data6<-data5[,-1]
head(data6)

((nrow(data1)-nrow(data5))/nrow(data1))*100
names(data5)

#Setting up the regression model

fit<-lm(CLV~State+Response+Coverage+Education+Effective.To.Date+EmploymentStatus+Gender+Income+Location.Code+Marital.Status+Monthly.Premium.Auto+Months.Since.Last.Claim+Months.Since.Policy.Inception+Number.of.Open.Complaints+Number.of.Policies+Policy.Type+Policy+Renew.Offer.Type+Sales.Channel+Total.Claim.Amount+Vehicle.Class+Vehicle.Size,data=data5)
summary(fit)

fit<-lm(CLV~State+Response+Coverage+Education+Effective.To.Date+EmploymentStatus+Gender+Income+Location.Code+Marital.Status+Monthly.Premium.Auto+Months.Since.Last.Claim+Months.Since.Policy.Inception+Number.of.Open.Complaints+Number.of.Policies+Policy.Type+I(Policy=="Corporate L2")+I(Policy=="Corporate L3")+I(Policy=="Personal L1")+I(Policy=="Personal L2")+I(Policy=="Special L1")+I(Policy=="Special L2")+Renew.Offer.Type+Sales.Channel+Total.Claim.Amount+Vehicle.Class+Vehicle.Size,data=data5)
summary(fit)

fit<-lm(CLV~State+Response+Coverage+Education+Effective.To.Date+EmploymentStatus+Gender+Income+Location.Code+Marital.Status+Monthly.Premium.Auto+Months.Since.Last.Claim+Months.Since.Policy.Inception+Number.of.Open.Complaints+Number.of.Policies+Policy.Type+I(Policy=="Corporate L2")+I(Policy=="Corporate L3")+I(Policy=="Personal L1")+I(Policy=="Personal L2")+I(Policy=="Special L1")+I(Policy=="Special L2")+Renew.Offer.Type+Sales.Channel+Total.Claim.Amount+Vehicle.Class+Vehicle.Size,data=data5)
summary(fit)

fit<-lm(CLV~State+Response+Coverage+Education+Effective.To.Date+EmploymentStatus+Gender+Income+Location.Code+Marital.Status+Monthly.Premium.Auto+Months.Since.Last.Claim+Months.Since.Policy.Inception+Number.of.Open.Complaints+Number.of.Policies+Policy.Type+I(Policy=="Corporate L2")+I(Policy=="Corporate L3")+Sales.Channel+Total.Claim.Amount+Vehicle.Class,data=data5)
summary(fit)

fit<-lm(CLV~State+Response+Coverage+Education+Effective.To.Date+EmploymentStatus+Gender+Income+Location.Code+Marital.Status+Monthly.Premium.Auto+Months.Since.Last.Claim+Months.Since.Policy.Inception+Number.of.Open.Complaints+Number.of.Policies+Policy.Type+I(Policy=="Corporate L2")+I(Policy=="Corporate L3")+Total.Claim.Amount+Vehicle.Class,data=data5)
summary(fit)

fit<-lm(CLV~State+Response+Coverage+Education+Effective.To.Date+EmploymentStatus+Gender+Income+Location.Code+Marital.Status+Monthly.Premium.Auto+Months.Since.Last.Claim+Months.Since.Policy.Inception+Number.of.Open.Complaints+Number.of.Policies+I(Policy.Type=="Special Auto")+I(Policy=="Corporate L2")+I(Policy=="Corporate L3")+Total.Claim.Amount+Vehicle.Class,data=data5)
summary(fit)

fit<-lm(CLV~State+Response+Coverage+Education+Effective.To.Date+EmploymentStatus+Gender+Income+Location.Code+Marital.Status+Monthly.Premium.Auto+Months.Since.Last.Claim+Number.of.Open.Complaints+Number.of.Policies+I(Policy.Type=="Special Auto")+I(Policy=="Corporate L2")+I(Policy=="Corporate L3")+Total.Claim.Amount+Vehicle.Class,data=data5)
summary(fit)

fit<-lm(CLV~State+Response+Coverage+Education+Effective.To.Date+EmploymentStatus+Gender+Income+Marital.Status+Monthly.Premium.Auto+Months.Since.Last.Claim+Number.of.Open.Complaints+Number.of.Policies+I(Policy.Type=="Special Auto")+I(Policy=="Corporate L2")+I(Policy=="Corporate L3")+Total.Claim.Amount+Vehicle.Class,data=data5)
summary(fit)

fit<-lm(CLV~State+Response+Education+Effective.To.Date+EmploymentStatus+Gender+Income+Marital.Status+Monthly.Premium.Auto+Months.Since.Last.Claim+Number.of.Open.Complaints+Number.of.Policies+I(Policy.Type=="Special Auto")+I(Policy=="Corporate L2")+I(Policy=="Corporate L3")+Total.Claim.Amount+Vehicle.Class,data=data5)
summary(fit)

fit<-lm(CLV~State+Response+Education+Effective.To.Date+EmploymentStatus+Gender+Income+Marital.Status+Monthly.Premium.Auto+Number.of.Open.Complaints+Number.of.Policies+I(Policy.Type=="Special Auto")+I(Policy=="Corporate L2")+I(Policy=="Corporate L3")+Total.Claim.Amount+Vehicle.Class,data=data5)
summary(fit)

fit<-lm(CLV~State+Response+Education+Effective.To.Date+EmploymentStatus+Gender+Income+Marital.Status+Monthly.Premium.Auto+Number.of.Open.Complaints+Number.of.Policies+I(Policy.Type=="Special Auto")+Total.Claim.Amount+Vehicle.Class,data=data5)
summary(fit)

fit<-lm(CLV~State+Response+Education+Effective.To.Date+EmploymentStatus+Gender+Income+I(Marital.Status=="Single")+Monthly.Premium.Auto+Number.of.Open.Complaints+Number.of.Policies+I(Policy.Type=="Special Auto")+Total.Claim.Amount+Vehicle.Class,data=data5)
summary(fit)

fit<-lm(CLV~State+Response+Education+Effective.To.Date+EmploymentStatus+Gender+Income+I(Marital.Status=="Single")+Monthly.Premium.Auto+Number.of.Open.Complaints+Number.of.Policies+I(Policy.Type=="Special Auto")+Vehicle.Class,data=data5)
summary(fit)

fit<-lm(CLV~State+Response+Education+Effective.To.Date+EmploymentStatus+Gender+Income+I(Marital.Status=="Single")+Monthly.Premium.Auto+Number.of.Open.Complaints+Number.of.Policies+I(Policy.Type=="Special Auto")+I(Vehicle.Class=="SUV")+I(Vehicle.Class=="Two-Door Car"),data=data5)
summary(fit)

fit<-lm(CLV~State+Response+Education+Effective.To.Date+I(EmploymentStatus=="Employed")+I(EmploymentStatus=="Medical Leave")+Gender+Income+I(Marital.Status=="Single")+Monthly.Premium.Auto+Number.of.Open.Complaints+Number.of.Policies+I(Policy.Type=="Special Auto")+I(Vehicle.Class=="SUV")+I(Vehicle.Class=="Two-Door Car"),data=data5)
summary(fit)

fit<-lm(CLV~State+Response+Education+Effective.To.Date+I(EmploymentStatus=="Employed")+I(EmploymentStatus=="Medical Leave")+Gender+Income+I(Marital.Status=="Single")+Monthly.Premium.Auto+I(Number.of.Open.Complaints=="3")+I(Number.of.Open.Complaints=="4")+I(Number.of.Open.Complaints=="5")+Number.of.Policies+I(Policy.Type=="Special Auto")+I(Vehicle.Class=="SUV")+I(Vehicle.Class=="Two-Door Car"),data=data5)
summary(fit)

fit<-lm(CLV~I(State=="California")+I(State=="Oregon")+Response+Education+Effective.To.Date+I(EmploymentStatus=="Employed")+I(EmploymentStatus=="Medical Leave")+Gender+Income+I(Marital.Status=="Single")+Monthly.Premium.Auto+I(Number.of.Open.Complaints=="3")+I(Number.of.Open.Complaints=="4")+I(Number.of.Open.Complaints=="5")+Number.of.Policies+I(Policy.Type=="Special Auto")+I(Vehicle.Class=="SUV")+I(Vehicle.Class=="Two-Door Car"),data=data5)
summary(fit)

fit<-lm(CLV~I(State=="California")+Response+Education+Effective.To.Date+I(EmploymentStatus=="Employed")+I(EmploymentStatus=="Medical Leave")+Gender+Income+I(Marital.Status=="Single")+Monthly.Premium.Auto+I(Number.of.Open.Complaints=="3")+I(Number.of.Open.Complaints=="4")+I(Number.of.Open.Complaints=="5")+Number.of.Policies+I(Policy.Type=="Special Auto")+I(Vehicle.Class=="SUV")+I(Vehicle.Class=="Two-Door Car"),data=data5)
summary(fit)

fit<-lm(CLV~+Response+Education+Effective.To.Date+I(EmploymentStatus=="Employed")+I(EmploymentStatus=="Medical Leave")+Gender+Income+I(Marital.Status=="Single")+Monthly.Premium.Auto+I(Number.of.Open.Complaints=="3")+I(Number.of.Open.Complaints=="4")+I(Number.of.Open.Complaints=="5")+Number.of.Policies+I(Policy.Type=="Special Auto")+I(Vehicle.Class=="SUV")+I(Vehicle.Class=="Two-Door Car"),data=data5)
summary(fit)
fit<-lm(CLV~+Response+I(Education=="Doctor")+I(Education=="High School or Below")+I(Education=="Master")+Effective.To.Date+I(EmploymentStatus=="Employed")+I(EmploymentStatus=="Medical Leave")+Gender+Income+I(Marital.Status=="Single")+Monthly.Premium.Auto+I(Number.of.Open.Complaints=="3")+I(Number.of.Open.Complaints=="4")+I(Number.of.Open.Complaints=="5")+Number.of.Policies+I(Policy.Type=="Special Auto")+I(Vehicle.Class=="SUV")+I(Vehicle.Class=="Two-Door Car"),data=data5)
summary(fit)

fit<-lm(CLV~+Response+I(Education=="Doctor")+I(Education=="Master")+Effective.To.Date+I(EmploymentStatus=="Employed")+I(EmploymentStatus=="Medical Leave")+Gender+Income+I(Marital.Status=="Single")+Monthly.Premium.Auto+I(Number.of.Open.Complaints=="3")+I(Number.of.Open.Complaints=="4")+I(Number.of.Open.Complaints=="5")+Number.of.Policies+I(Policy.Type=="Special Auto")+I(Vehicle.Class=="SUV")+I(Vehicle.Class=="Two-Door Car"),data=data5)
summary(fit)

#Checking for multicollinearity
vif(fit)

#Getting the predicted or fitted values
fitted(fit)

#MAPE

data5$pred<-fitted(fit)
names(data5)

#Calculating MAPE
attach(data5)
(sum((abs(CLV-pred))/CLV))/nrow(data5)

#Checking for Autocorrelation
dwt(fit)

#Checking for Heteroscastacity
bptest(fit)

resids<-fit$residuals

#Checking for normality
ad.test(resids)