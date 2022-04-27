
# Loading of packages ---------------------------------------------------------
library("quantmod")
library("PerformanceAnalytics")

# Get the stock price data for CVX from 1990 till 2022
getSymbols("CVX",
           from = "1999-01-01",
           to = "2022-02-28")

# Plot the adjusted stock price
plot(CVX[, 6], theme = "white",
     main = "CVX stock price from 1999 till 2022",
     ylab = "Price per share (USD)")

# Plot the data of period 1
plot(CVX["2015-04-01/2015-09-30", 6], theme = "white",
     main = "CVX stock price from April 2015 till September 2015",
     ylab = "Price per share (USD)")

# Plot the data of period 2
plot(CVX["2021-09-01/2022-02-28", 6], theme = "white",
     main = "CVX stock price from September 2021 till Februari 2022",
     ylab = "Price per share (USD)")


# Calculate the returns of the full period and of the selected periods
CVXreturns <- Return.calculate(CVX[, 6], method = "simple")
CVXreturns <- CVXreturns[(-1)]
CVXreturns.2015 <- CVXreturns["2015-04-01/2015-09-30"]
CVXreturns.2022 <- CVXreturns["2021-09-01/2022-02-28"]

# Create histograms to display the returns
hist(CVXreturns, col = "skyblue", breaks = 50,
     main = "Distribution of returns from 1999 till 2022",
     xlab = "Simple returns")

hist(CVXreturns.2015,col = "skyblue", breaks = 30,
     main = "Distribution of returns from April 2015 till September 2015",
     xlab = "Simple returns")

hist(CVXreturns.2022,col = "skyblue", breaks = 25,
     main = "Distribution of returns from September 2021 till Februari 2022",
     xlab = "Simple returns")

# Create a density plot to show the returns in one graph
plot(density(CVXreturns), lwd = 2,  col = "black",
     main = "Density plot for simple returns of selected periods",
     xlab = "Simple returns", ylim = c(0, 40))

lines(density(CVXreturns.2015), lwd = 2, col = "red")
lines(density(CVXreturns.2022), lwd = 2, col = "green")
legend(x = "topright",
       legend = c("returns from 1999 till 2022",
                  "returns from April 2015 till September 2015",
                  "returns from September 2021 till Februari 2022"),
       col = c("black", "red", "green"),
       lwd = 2,
       cex = 0.7)

# Compute the annualized return for every period
252 * mean(CVXreturns)
252 * mean(CVXreturns.2015)
252 * mean(CVXreturns.2022)

# Compute the annualized volatility of every period
sqrt(252) * sd(CVXreturns)
sqrt(252) * sd(CVXreturns.2015)
sqrt(252) * sd(CVXreturns.2022)

# Plot the annualized volatility of the stock returns
chart.RollingPerformance(R=CVXreturns, width = 22, FUN = "sd.annualized", scale = 252, 
                         main = "Rolling 1 month Annualized Volatility CVX", ylab = "")
