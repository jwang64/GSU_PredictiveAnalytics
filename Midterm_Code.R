# Question 1
wt <- rnorm(10,0,1)
xt <- wt[1]
xt[2] <- wt[2]
xt[3] <- wt[3]

for (i in 3:length(wt))
{
  xt<-c(xt, (.8 * xt[i-1]) - (.15 * xt[i-2]) + wt[i] - (.3 * wt[i-1]) )
}
xt2 <- as.ts(xt)
plot(xt2)

acf(xt2)

pacf(xt2)

# Question 2
ut <- rnorm(10,0,1)
xt_q2 <- ut[1]
for(i in 2:length(ut))
{
  xt_q2<-c(xt_q2, (.8 * xt_q2[i-1] + ut[i]))
}
xt2_q2 <- as.ts(xt_q2)
plot(xt2_q2)
acf(xt2_q2)
pacf(xt2_q2)

# Question 4
require(astsa)
oil

plot(oil)
acf(oil)
#Based off the plot, oil appears nonstationary. When examining the ACF of the oil,
#We see that the ACF decreases slowly and is significant within all visible lags. We attempt
#to remedy the situation by differencing oil

plot(diff(oil))
acf2(diff(oil))

#Our data now appears stationary because our ACF shows a degree of exponential decay.
#We see significant lags for the ACF. In the pacf we see significant lags 
#at . Afterwards, the lags appear to taper off for both the ACF and the PACF. We
#suspect an ARIMA(1,1,3) model

#ARIMA(1,1,3) Model
output<-sarima(oil, 1, 1, 3, no.constant = TRUE)
print(output)
print(Box.test(output$fit$residuals, lag = 20, type="Ljung-Box"))

#The standardized residuals appear to have no trend, thus supporting the usage of the model.
#The normality of the residuals through the quantile-quantile plot appear to follow a straight
#line relatively closely. There are a few values that do not follow the line at the extremes,
#I look towards the ACF of the residuals as well as the Ljung-Box test. For the Residuals'
#ACF, we see no evidence of autocorrelation except for a significant lag between .1 and .2, and .45 and .6, but the Ljung-Box
#test informs us with an X-squared of 30.288 and a p-value of .0652 that at a 5% significance level, I fail to reject
#the null hypothesis that the error terms are uncorrelated.

# Question 5
plot(so2)
# The Data appears unstationary because of the variation doesn't appear constant. it does appear to hover around a mean
# however there is still a slight downward trend.

acf(so2)
# Although there does appear to be some degree of exponential decay for the ACF, all of the lags still appear to be
# significant, so I attempt to remedy the situation through taking the difference.


plot(diff(so2))

# After taking the difference for so2, the data appears to be stationary since the variance appears to be more
# constant. The mean seems to be fixed around a certain point as well too.
acf(diff(so2))
# There seems to be exponential decay for the ACF, although there are some significant lags, at certain time points,
# I think that a ARIMA(1,1,1) model can be used.

output<-sarima(so2, 1, 1, 1, no.constant = TRUE)
print(output)
print(Box.test(output$fit$residuals, lag = 20, type="Ljung-Box"))
#The standardized residuals appear to have no trend, thus supporting the usage of the model.
#The normality of the residuals through the quantile-quantile plot appear to follow a straight
#line relatively closely. There are a few values that do not follow the line at the extremes,
#I look towards the ACF of the residuals as well as the Ljung-Box test. For the Residuals'
#ACF, we see no evidence of autocorrelation, but the Ljung-Box
#test informs us with an X-squared of 28.831 and a p-value of .09115 that at a 5% significance level, I fail to reject
#the null hypothesis that the error terms are uncorrelated.


#We move forward with the ARIMA(1,1,1) model to begin forecasting for the next 4 months

sarima.for(so2, n.ahead = 4,p=1,d=0,q=4, plot=TRUE)

print(sarima.for(so2, n.ahead = 4,p=1,d=0,q=4, plot=TRUE))

# Question 6
data(lead)

plot(sales)
# Based off of the plot sales data, the sales data doesn't seem to be stationary.

acf(sales)
# The ACF decreases slowly and is significant within all visible lags. I attempt
#to remedy the situation by differencing sales

plot(diff(sales))

#The plot of the difference for sales now appears stationary.

acf2(diff(sales))
# The ACF is now showing signs of exponential decay, but there are still some significant lags at 1,2,3,4
#In the PACF there are significant lags at 1 and 2. I suspect that it's an ARIMA(1,1,1) model.

plot(lead)
# The plot shows the the lead data doesn't appear stationary.
acf(lead)
# The ACF slowly decreases and is significant, I attempt to remedy the situation
# by differencing lead.

plot(diff(lead))
# The plot for the differncing of lead appears stationary.

acf(diff(lead))
# The ACF is now showing signs of exponential decay, but there are still some significant lags at 1,2,3,4
#In the PACF there are significant lags at 1 and 2. I suspect that it's an ARIMA(1,1,1) model.


library(tseries)

adf.test(sales)


adf.test(lead)
# For the ADF tests, the p value is greater than .05, so the null hypothesis is accepted that the series is
# nonstationary for both sales and lead.

#ARIMA(1,1,1) Model
output<-sarima(sales, 1, 1, 1, no.constant = TRUE)
print(output)
print(Box.test(output$fit$residuals, lag = 20, type="Ljung-Box"))

#The standardized residuals appear to have no trend, thus supporting the usage of the model.
#The normality of the residuals through the quantile-quantile plot appear to follow a straight
#line relatively closely. We see a few values that do not follow the line at the extremes,
#however, they are not statistically significant as outliers. When examining the independence,
#We look towards the ACF of the residuals as well as the Ljung-Box test. For the Residuals'
#ACF, we see no evidence of autocorrelation, but the Ljung-Box
#test informs us with an X-squared of 15.59 and a p-value of .7418 that we fail to reject
#the null hypothesis that the error terms are uncorrelated.

output<-sarima(lead, 1, 1, 1, no.constant = TRUE)
print(output)
print(Box.test(output$fit$residuals, lag = 20, type="Ljung-Box"))
#The standardized residuals appear to have no trend, thus supporting the usage of the model.
#The normality of the residuals through the quantile-quantile plot appear to follow a straight
#line relatively closely. We see a few values that do not follow the line at the extremes,
#however, they are not statistically significant as outliers. When examining the independence,
#We look towards the ACF of the residuals as well as the Ljung-Box test. For the Residuals'
#ACF, we see no evidence of autocorrelation, but the Ljung-Box
#test informs us with an X-squared of 15.69 and a p-value of .7357 that we fail to reject
#the null hypothesis that the error terms are uncorrelated.

