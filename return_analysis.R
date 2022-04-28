
# Loading of packages ---------------------------------------------------------
library("quantmod")
library("PerformanceAnalytics")
library("rugarch")
library("GAS")

# Get the stock price data for CVX from 1990 till 2022
getSymbols("CVX",
           from = "1999-01-01",
           to = "2022-02-28")

# Calculate the returns of the full period and of the selected periods
CVXreturns <- Return.calculate(CVX[,6],method="log")
CVXreturns <- CVXreturns[(-1)]
CVXreturns.2015 <- CVXreturns["2015-04-01/2015-09-30"]
CVXreturns.2022 <- CVXreturns["2021-09-01/2022-02-28"]


# Compute the annualized return for every period
252 * mean(CVXreturns)
252 * mean(CVXreturns.2015)
252 * mean(CVXreturns.2022)

# Compute the annualized volatility of every period
sqrt(252) * sd(CVXreturns)
sqrt(252) * sd(CVXreturns.2015)
sqrt(252) * sd(CVXreturns.2022)

# Plot the annualized return of the stock returns
chart.RollingPerformance(R = CVXreturns,
                         width = 66, FUN = "Return.annualized",
                         scale = 252, ylab = "",
                         main = "Rolling 3 month Annualized return of CVX")

# Plot the annualized volatility of the stock returns
chart.RollingPerformance(R = CVXreturns,
                         width = 66, FUN = "sd.annualized",
                         scale = 252, ylab = "",
                         main = "Rolling 3 month Annualized Volatility of CVX")


#GARCH-model ------------------------------------------------------------------
garchspec <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                        variance.model = list(model = "sGARCH",
                                              garchOrder = c(1,1)),
                        distribution.model = "norm")
garchfit <- ugarchfit(garchspec, data = CVXreturns)

# Summary of model
coef(garchfit)

# Get the range of the volatility
min(sigma(garchfit))
max(sigma(garchfit))

# Likelihood of the model
likelihood(garchfit)

# plot the volatility estimates by the model
plot(sigma(garchfit))

# Plot the 1 month annualized volatility and the predicted volatility
par(mfrow=c(2,1))
chart.RollingPerformance(R = CVXreturns,
                         width = 22, FUN = "sd.annualized", scale = 252,
                         main = "1 month Annualized Volatility of CVX",
                         ylab = "")
plot(sigma(garchfit) * sqrt(252), col = "red",
     main = "Annualized GARCH predictions")



# DOWNSIDE RISK ----------------------------------------------------------------
var = qnorm(p = 0.05, mean = fitted(garchfit), sd = sigma(garchfit))
chartSeries(var, theme = "white")

# Rolling estimate to prevent look ahead bias
garchroll <- ugarchroll(garchspec, data = CVXreturns,
                        n.start = 2500,
                        refit.window = "moving", refit.every = 500)

garchVaR <- quantile(garchroll, probs=0.05)

actual <- xts(as.data.frame(garchroll)$Realized, time(garchVaR))
VaRplot(alpha = 0.05, actual = actual, VaR = garchVaR)
title(main = "GARCH predictions of 5% value at risk")

#calculate the coverage
# This is the amount of times the return is less than the value at risk
mean(actual < garchVaR)

# Is the coverage significantly different from the actual?
out <- BacktestVaR(alpha = 0.05, data = actual, VaR = garchVaR)
out$LRuc

# Get the VaR for the first of April 2016
garchVaR["2016-04-01"]



# MORE COMPLEX MODELS----------------------------------------------------------

# GARCH model with normal distribution, with 2 period lag
long_norm_spec <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                        variance.model = list(model = "sGARCH",
                                              garchOrder = c(2,2)),
                        distribution.model = "norm")
long_norm_fit <- ugarchfit(long_norm_spec, data = CVXreturns)


# GARCH model with student distribution, with 1 period lag
short_std_spec <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                             variance.model = list(model = "sGARCH",
                                                   garchOrder = c(1,1)),
                             distribution.model = "std")
short_std_fit <- ugarchfit(short_std_spec, data = CVXreturns)


# GARCH model with student distribution, with 2 period lag
long_std_spec <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                             variance.model = list(model = "sGARCH",
                                                   garchOrder = c(2,2)),
                             distribution.model = "std")
long_std_fit <- ugarchfit(long_std_spec, data = CVXreturns)


# Create a dataframe with the likelihood of the different GARCH models
df_like <- data.frame(likelihood(garchfit),
                      likelihood(long_norm_fit),
                      likelihood(short_std_fit),
                      likelihood(long_std_fit))
names(df_like) <- c("short_normal", "long_normal", "short_student", "long_student")
df_like

# Create a dataframe with the information criteria of the models
df_info <- data.frame(infocriteria(garchfit),
                      infocriteria(long_norm_fit),
                      infocriteria(short_std_fit),
                      infocriteria(long_std_fit))
names(df_info) <- c("short_normal", "long_normal", "short_student", "long_student")
df_info


# Plots to examine the predictions of the models
# plot 8 & 9 offer support to fit a student distribution 
# plot 4 indicates that larger lag does not contribute much (until maybe  lag 4, 6 and 7)
###plot(garchfit)
###plot(short_std_fit)

# The plots are commented out so the script can be run at once


# Plot the predictions of the student model over the observed volatility
chart.RollingPerformance(R = CVXreturns,
                         width = 22, FUN = "sd.annualized", scale = 252,
                         main = "Observed volatility vs GARCH prediction with student distribution",
                         ylab = "")
lines(sigma(short_std_fit) * sqrt(252), col = "red")
legend(x = "topleft",
       legend = c("1 month annualized volatility", "GARCH predictions"),
       col = c("black", "red"),
       lwd = 1)


## DOWNSIDE RISK
var = qnorm(p = 0.05, mean = fitted(short_std_fit), sd = sigma(short_std_fit))
chartSeries(var, theme = "white")

# Rolling estimate to prevent look ahead bias
std_roll <- ugarchroll(short_std_spec, data = CVXreturns,
                        n.start = 2500, refit.window = "moving", refit.every = 500)

std_VaR <- quantile(std_roll, probs = 0.05)

actual <- xts(as.data.frame(std_roll)$Realized, time(std_VaR))
VaRplot(alpha = 0.05, actual = actual, VaR = std_VaR)


# Calculate the coverage
mean(actual < std_VaR)

# Is the coverage significantly different from the actual?
out_std <- BacktestVaR(alpha = 0.05, data = actual, VaR = std_VaR)
out_std$LRuc

# Get the VaR for the first of April 2016
std_VaR["2016-04-01"]
