require(astsa)
require(prophet)

google <- read.csv("C:/Users/54208/Google Drive/School/Graduate School/Masters/Spring_2021/MSA8200/GoogleAds&Trends_Data.csv")

trends <-data.frame(date=google$FIXED.TIME,value=google$value..Google.Trend.)
trends$date <- as.Date(trends$date,"%m/%d/%Y")

plot.ts(trends$value)
acf2(trends$value, max.lag = 47)

set.seed(42) 
sample <- sample.int(n = nrow(trends), size = floor(.8*nrow(trends)), replace = F)
train <- trends[sample, ]
test  <- trends[-sample, ]

names(train) = c("ds","y")
names(test) = c("ds","y")

train$ds

m = prophet(train)
future = make_future_dataframe(m, periods = 213)
forecast = predict(m, future)

# Visualize forecast
plot(m, forecast)

prophet_plot_components(m, forecast)

forecast_test = predict(m, test)
forecast_test

MSPE = mean(((test$y - forecast_test$yhat) / test$y)^2) * 100
MSPE
