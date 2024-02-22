library(forecast)
library(zoo)
getwd()
setwd("C:/Users/vijay/OneDrive/Desktop/MSBA/2nd semester/Time series")

sales <- read.csv("Sales_case1.csv")
head(sales)
sales

# creating time series data sales.ts
 sales.ts <- ts(sales$Sales, 
               start = c(2015, 1), end = c(2021, 12), freq = 12)
sales.ts 

# creating a plot
plot(sales.ts, 
     xlab = "Time", ylab = "Sales (in millions)", 
     ylim = c(100, 600), xaxt = 'n',
     main = "Monthly sales")
axis(1, at = seq(2015, 2021, 1), labels = format(seq(2015, 2021, 1)))

#using stl and acf
sales.stl <- stl(sales.ts, s.window = "periodic")
autoplot(sales.stl, main = "Sales Data Time Series Components")

autocor <- Acf(sales.ts, lag.max = 12, 
               main = "Autocorrelation for Sales Data")

# Display autocorrelation coefficients for various lags.
Lag <- round(autocor$lag, 0)
ACF <- round(autocor$acf, 3)
data.frame(Lag, ACF)


#partitioning the data 
nValid <- 24
nTrain <- length(sales.ts) - nValid 
train.ts <- window(sales.ts, start = c(2015, 1), end = c(2015, nTrain))
valid.ts <- window(sales.ts, start = c(2015, nTrain + 1), 
                   end = c(2015, nTrain + nValid))

plot(train.ts, 
     xlab = "Time", ylab = "Sales (in millions)", ylim = c(100, 500), 
     bty = "l", xlim = c(2015, 2023.25), xaxt = 'n', main = "", lwd = 2) 
axis(1, at = seq(2015, 2022, 1), labels = format(seq(2015, 2022, 1)))
lines(valid.ts, col = "blue", lty = 1, lwd = 2)

lines(c(2020, 2020), c(0, 500))
lines(c(2022, 2022), c(0, 500))
text(2017, 510, "Training")
text(2020.9, 510, "Validation")
text(2022.8, 510, "Future")
arrows(2015, 500, 2019.9, 500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 500, 2021.9, 500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2022.1, 500, 2023.3, 500, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#trailing MAs

ma.trailing_2 <- rollmean(train.ts, k = 2, align = "right")
ma.trailing_6 <- rollmean(train.ts, k = 6, align = "right")
ma.trailing_12 <- rollmean(train.ts, k = 12, align = "right")

#forecast for above window widths
ma.trail_2.pred <- forecast(ma.trailing_2, h = nValid, level = 0)
ma.trail_2.pred
ma.trail_6.pred <- forecast(ma.trailing_6, h = nValid, level = 0)
ma.trail_12.pred <- forecast(ma.trailing_12, h = nValid, level = 0)
ma.trail_12.pred$mean



#accuracy rates for 3 trailing MAs
round(accuracy(ma.trail_2.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_6.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_12.pred$mean, valid.ts), 3)


#regression model with linear trend and seasonality 

trend.seas <- tslm(train.ts ~ trend + season)
summary(trend.seas)

#forecast for validation data
trend.seas.pred <- forecast(trend.seas, h = nValid, level = 0)
trend.seas.pred$mean


#finding regression residuals in the training data
trend.seas.res <- trend.seas$residuals
trend.seas.res

ma.trail.res <- rollmean(trend.seas.res, k = 2, align = "right")
ma.trail.res

#residual forecast in the validation data
ma.trail.res.pred <- forecast(ma.trail.res, h = nValid, level = 0)
ma.trail.res.pred$mean



# Regression residuals in validation period.
trend.seas.res.valid <- valid.ts - trend.seas.pred$mean
trend.seas.res.valid

#two level forecast by combining regression forecast and trailing MA forecast for residuals
fst.2level <- trend.seas.pred$mean + ma.trail.res.pred$mean
fst.2level

#combined table
valid.df <- round(data.frame(valid.ts, trend.seas.pred$mean, 
                             ma.trail.res.pred$mean, 
                             fst.2level), 3)
names(valid.df) <- c("Ridership", "Regression.Fst", 
                     "MA.Residuals.Fst", "Combined.Fst")
valid.df

#accuracy metrics comparing linear trend and seasonality model and two-level forecasting
round(accuracy(trend.seas.pred$mean, valid.ts), 3)
round(accuracy(fst.2level, valid.ts), 3)



#regression model with entire data
tot.trend.seas <- tslm(sales.ts ~ trend  + season)
summary(tot.trend.seas)

#regression residuals
tot.trend.seas.res <- tot.trend.seas$residuals
tot.trend.seas.res

#trailing MA of regression residuals
tot.ma.trail.res <- rollmean(tot.trend.seas.res, k = 2, align = "right")
tot.ma.trail.res

#regression forecast for 12 periods 
tot.ma.trail.res.pred <- forecast(tot.ma.trail.res, h = 12, level = 0)
tot.ma.trail.res.pred

#2 level forecast
tot.fst.2level <- tot.trend.seas.pred$mean + tot.ma.trail.res.pred$mean
tot.fst.2level

#combined table 
future12.df <- round(data.frame(tot.trend.seas.pred$mean, tot.ma.trail.res.pred$mean, 
                                tot.fst.2level), 3)
names(future12.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
future12.df

#seasonal naive
sales.snaive.pred <- snaive(sales.ts, h = nValid)

#accuracy metrics 
round(accuracy(tot.trend.seas.pred$fitted, sales.ts), 3)
round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res, sales.ts), 3)
round(accuracy((snaive(sales.ts))$fitted, sales.ts), 3)

#Holt-Winter’s model - automated selection of error, trend, and seasonality options, 
#and automated selection of smoothing parameters for the training partition
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ

#forecast for validation period
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred$mean


#HW model with entire data
HW.ZZZ <- ets(sales.ts, model = "ZZZ")
HW.ZZZ

#forecast for future 12 periods
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 12 , level = 0)
HW.ZZZ.pred$mean



#comparing accuracy between seasonal naive model and HW model with entire data

round(accuracy((snaive(sales.ts))$fitted, sales.ts), 3)
round(accuracy(HW.ZZZ.pred$fitted, sales.ts), 3)


#comparing best models from ques. 3e and 4c

round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res, sales.ts), 3)
round(accuracy(HW.ZZZ.pred$fitted, sales.ts), 3)
