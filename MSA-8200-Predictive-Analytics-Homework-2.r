#MSA 8200 Predictive Analytics Homework 2
#James Wang

#Problem 3: Fit an ARIMA(p,d,q) model to the global temperature data globtemp performing all of the necessary diagnostics. After deciding on an appropriate model, forecast (with limits) the next 10 years. Comment.
require(astsa)

plot(globtemp)
acf(globtemp)

#Based off the plot, globtemps appears nonstationary. When examining the ACF of the globtemps,
#We see that the ACF decreases slowly and is significant within all visible lags. We attempt
#to remedy the situation by differencing globtemps

plot(diff(globtemp))
acf2(diff(globtemp))

#Our data now appears stationary because our ACF shows a degree of exponential decay.
#We see significant lags from 1, 2, 4, and 18 for the ACF. In the pacf we see significant lags 
#at 1, 2, and 3. Afterwards, the lags appear to taper off for both the ACF and the PACF. We
#suspect an ARIMA(1,1,1) model

#ARIMA(1,1,1) Model
output<-sarima(globtemp, 1, 1, 1, no.constant = TRUE)
print(output)
print(Box.test(output$fit$residuals, lag = 20, type="Ljung-Box"))

#The standardized residuals appear to have no trend, thus supporting the usage of the model.
#The normality of the residuals through the quantile-quantile plot appear to follow a straight
#line relatively closely. We see a few values that do not follow the line at the extremes,
#however, they are not statistically significant as outliers. When examining the independence,
#We look towards the ACF of the residuals as well as the Ljung-Box test. For the Residuals'
#ACF, we see no evidence of autocorrelation with the exception of lag 18, but the Ljung-Box
#test informs us with an X-squared of 24.76 and a p-value of 0.2108 that we fail to reject
#the null hypothesis that the error terms are uncorrelated.

#We proceed with an ARIMA(1,1,1) Model to forecast the next 10 years
sarima.for(globtemp, n.ahead = 10,p=1,d=1,q=1, no.constant = TRUE, plot=TRUE)

print(sarima.for(globtemp, n.ahead = 10,p=1,d=1,q=1, no.constant = TRUE, plot=TRUE))

#We predict that the globtemps will be the following for the next 10 years: 0.7942681, 
#0.7705020, 0.7630437, 0.7607031, 0.7599686, 0.7597381, 0.7596658, 0.7596431, 0.7596359,
#0.7596337

#The standard errors are as follows: 0.1010002, 0.1186411, 0.1287798, 0.1368393, 0.1440708,
#0.1508444, 0.1572932, 0.1634777, 0.1694336, 0.1751862

#Problem 4: Use the attached data (Sales.xls) and perform time series analysis on MONTHLY 
#sales. Make sure your analysis cover the following steps: 1) Data Exploration to check 
#whether the data is stationary and perform any transformation needed. 2) Model Fitting
#3) Model diagnosis. 4) Prediction.

#Load the data
sales <- read_excel("C:/Users/James Wang/Downloads/sales.xls")

#We are only interested in performing a time series analysis on monthly sales, so we extract
#two core columns from the sales data set: Order Date and Sales
newsales <-data.frame(Order=sales$`Order Date`,Sales=sales$Sales)

#The current format includes days and time, so we create a new column "monthyear" to simply
#have the month and years.
newsales$monthyear <- format(as.Date(newsales$Order), "%Y-%m")

#Now that we have months and years, we are looking at total monthly sales-- we need to sum up
#all sales for a particular month and year combination
require(dplyr)
MonthlySales <- newsales %>%
  group_by(monthyear) %>%
  summarize(sum(Sales))

#Now we can begin our process of data exploration
plot.ts(MonthlySales$`sum(Sales)`)
acf2(MonthlySales$`sum(Sales)`, max.lag = 47)

#Based off the plot, our dataset appears to be stationary-- it appears roughly horizontal
#and showcases some cyclical behavior. Likewise, based off of our ACF, we see that there is
#a degree of exponential decay, helping indicate that the process is stationary. Also while
#examining the data and plot, we see the potential for seasonality. For example, we see sales
#are relatively larger during the months of September, November, and December. We likewise 
#see that at lags of 12 and 24 we see significant autocorrelation. However, at lag 36, we see
#an nonsignificant spike. We do not move forward with incorporating seasonality into our
#model. What we do see for ACF and PACF is that we have an ARMA model of some form. 

#We fit an ARMA(1,1) model to account for the seasonality and run diagnostics.
output1<-sarima(MonthlySales$`sum(Sales)`, 1, 0, 1)
print(output1)
print(Box.test(output1$fit$residuals, lag = 20, type="Ljung-Box"))

#From our ARMA(1,1) model, although our standardized residuals appear to have no trend and
#the normality of the residuals appear close to straight line in the quantile-quantile plot,
#our Ljung-Box test indicates, with an X-squared = 41.47 and p-value of 0.00324, that we
#reject the null hypothesis that the error terms are uncorrelated. So we do not proceed with
#the ARMA(1,1) model. 

#We attempt to fit ARMA(1,2) and ARMA(1,3)
output2<-sarima(MonthlySales$`sum(Sales)`, 1, 0, 2)
print(output2)
print(Box.test(output2$fit$residuals, lag = 20, type="Ljung-Box"))

output3<-sarima(MonthlySales$`sum(Sales)`, 1, 0, 3)
print(output3)
print(Box.test(output3$fit$residuals, lag = 20, type="Ljung-Box"))

#Similar to our ARMA(1,1) model, the standardized residuals appear to have no trend and
#the normality of the residuals appear close to straight line in the quantile-quantile plot,
#but our Ljung-Box test each indicate [ARMA(1,2)'s X-squared = 41.467 and p-value of 0.003243]
#and ARMA(1,3)'s X-Squared = 36.955 and p-value of 0.01185] that we reject the null
#hypothesis that the error terms are uncorrelated. So we do not proceed with either the
#ARMA(1,2) or ARMA(1,3) model.

#We attempt to fit ARMA(1,4)
output4<-sarima(MonthlySales$`sum(Sales)`, 1, 0, 4)
print(output4)
print(Box.test(output4$fit$residuals, lag = 20, type="Ljung-Box"))

#The standardized residuals appear to have no obvious trend,supporting the the model.
#The residuals through the quantile-quantile plot appear to follow a straight line, and 
#We see a few values that do not follow the line at the extremes, but they do not appear
#as outliers. When examining the independence, We look towards the residual's ACF and
#Ljung-Box Test. For the ACF, we see no evidence of autocorrelation with the exception
#of lag 12, but the Ljung-Box test informs us with an X-squared of 28.315 and a p-value of 
#0.1021 that we fail to reject the null hypothesis that the error terms are uncorrelated.

#We move forward with the ARMA(1,4) model to begin forecasting for the next 12 months

sarima.for(MonthlySales$`sum(Sales)`, n.ahead = 12,p=1,d=0,q=4, plot=TRUE)

print(sarima.for(MonthlySales$`sum(Sales)`, n.ahead = 12,p=1,d=0,q=4, plot=TRUE))

#We predict that the sales over the next 12 months will be as follows: 63121.35, 63379.87, 
#53347.83, 35628.31, 56044.39, 40278.27, 52453.50, 43051.29, 50312.06, 44705.00, 49035.00,
#45691.20

#The standard errors are as follows: 20454.74, 23984.97, 24704.49, 26385.64, 26801.80,
#27046.94, 27192.07, 27278.26, 27329.53, 27360.06, 27378.24, 27389.09

