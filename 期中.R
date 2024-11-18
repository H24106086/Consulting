
#install.packages("TSA")
library(TSA)
library(tseries)
data(google)
#help(google)
adf.test(google)



plot(google,ylab="daily returns")
plot(abs(google))
plot(google^2)
plot(abs(google))

#install.packages("FinTS")
library(FinTS)
ArchTest(google)
McLeod.Li.test(y=google,cex=1)
text(par("usr")[1], 0.05,
     labels = "0.05",
     pos = 4, col = "red")


model <- garch(google,order=c(1,1))
summary(model)


plot(residuals(model),ylab=
       "Standardized residuals",
     main=
       "Standardized Residual Plots")
acf(residuals(model)^2,
    na.action = na.omit,
    main="Sample ACF of Square 
    Standardized Residuals",
    xlim=c(0,21))


gBox(model,method='squared')
title(main="Generalized Portmanteau
      Test p-Values for the Squared
      Standardized Residuals")

plot((fitted(model)[,1])^2,type='l',ylab='Conditional Variance', 
     xlab='t')

#######################################
#help(oil.price)
#data <- diff(log(oil.price))
#adf.test(data)
#plot(data)
#acf(data)
#pacf(data)
#eacf(data)
#fit_oil <-  auto.arima(data,stepwise = T)
#plot(residuals(fit_oil)^2,
#     main="Squared Standardized Residual")
#McLeod.Li.test(y=residuals(fit_oil)
#              ,main="McLeod.Li.test")
#eacf(residuals(fit_oil)^2)
#eacf(abs(residuals(fit_oil)))

#library(rugarch)
#spec <- ugarchspec(
#  variance.model = list(model = "sGARCH",
#                        garchOrder = c(1,1)),
#  mean.model = list(armaOrder = c(0, 1)
#                    , include.mean = TRUE),
#  distribution.model = "norm")
#fit_AG <- ugarchfit(spec = spec, data =data)

#plot(residuals(fit_AG))
#gBox(residuals(fit_AG),method="Squared")

##################################
#install.packages("quantmod")
#install.packages("rugarch")
#install.packages("forecast")
library(rugarch)
library(quantmod)
library(forecast)
getSymbols("^GDAXI", src = "yahoo")
data <- GDAXI["2021-01-01/2022-12-31"]
data <- data$GDAXI.Adjusted
data_ <- na.omit(diff(log(data)))
plot(data_)
adf.test(data_)
acf(data_,main="ACF of GDAX")


GS_ <- auto.arima(data_,
                  stepwise = T)
ArchTest(data_)
McLeod.Li.test(y=data_,main="McLeod Li test")
spec <- ugarchspec(
  variance.model = list(model = "sGARCH",
                        garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0, 0)
                    , include.mean = TRUE),
)


fit <- ugarchfit(spec,data_)
fit
plot(fit)



spec_e <- ugarchspec(
  variance.model = list(model = "eGARCH",
                        garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0, 0)
                    , include.mean = TRUE),
)
fit_e <- ugarchfit(spec_e,data_)
fit_e
plot(fit_e)
#res <- fit@fit$residuals

##################################

getSymbols("^GDAXI", src = "yahoo")#德
dataG <- GDAXI["2021-01-01/2022-12-31"]
getSymbols("^AXJO", src = "yahoo")#澳
dataAU <- AXJO["2021-01-01/2022-12-31"]
getSymbols("^GSPC", src = "yahoo")#美
dataA <- GSPC["2021-01-01/2022-12-31"]

data_G <- na.omit(diff(log(dataG$GDAXI.Adjusted)))
data_AU <- na.omit(diff(log(dataAU$AXJO.Adjusted)))
data_A <- na.omit(diff(log(dataA$GSPC.Adjusted)))

adf.test(data_G)#0.01
adf.test(data_AU)#0.01
adf.test(data_A)#0.01

AU_ <- auto.arima(data_AU,stepwise = T)
#(0,0,0)
A_ <- auto.arima(data_A,stepwise=T)
#(0,0,0)


#install.packages("rmgarch")
library(rmgarch)
spec_m <- dccspec(uspec=multispec(replicate(3,spec)),
                  dccOrder=c(1,1),distribution = "mvnorm")
merged_data <- na.omit(merge(data_G, data_A, data_AU))
fit_m <- dccfit(spec_m,merged_data)
fit_m
