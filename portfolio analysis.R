
# Loading of packages ---------------------------------------------------------
library("quantmod")
library("PerformanceAnalytics")
library("quadprog")
library("PortfolioAnalytics")


# GET THE DATA ----------------------------------------------------------------

# Set the start and end date
date.start <- as.Date("2004-01-01")
date.end <- as.Date("2022-02-28")

# Define the tickers of the stocks and ETF's
tickers <- c("CVX", "XOM", "CTRA", "NFG", "GEL",   # Stocks
             "SOXX","VSMAX", "QQQ", "XLE", "XLP")  # ETF's

# Loop over the tickers and get the monthly adjusted price
monthly.close <- c()
for (ticker in tickers) {
  cat(ticker, "\n")
  price <- suppressWarnings(getSymbols(Symbols = ticker, from = date.start, to = date.end))
  adjusted.price <- get(price)[, 6]
  monthly.close <- cbind(monthly.close, xts::to.monthly(adjusted.price, indexAt = "lastof")[, 4])
}

# Rename the columns
names(monthly.close) <- tickers

# Get the data for the S&P500
getSymbols(Symbols = "^GSPC", from = date.start, to = date.end)
sp500_adjusted <- GSPC[, 6]
sp500_monthly <- xts::to.monthly(sp500_adjusted, indexAt = "lastof")[, 4]

# Calculate the returns
returns <- Return.calculate(monthly.close)
returns <- returns[(-1),]

# Returns of SP500
sp500_returns <- Return.calculate(sp500_monthly)
sp500_returns <- sp500_returns[(-1),]


# Portfolio optimization ------------------------------------------------------

# Compute the mean and volatility of the monthly returns
vMu <- colMeans(returns)
mCov <- cov(returns)

# Get the number of tickers in the dataset
N <- ncol(mCov)

# Define the grid of target return
target.mu.grid <- seq(min(vMu),max(vMu),length.out = 100 )  

# Define the weight matrix to store all optimized weights
weight.matrix     <- matrix( NA, nrow = length(target.mu.grid), ncol = N )


# Define the constraints
l <- rep(0, N)
u <- rep(1, N)
dvec <- rep(0, N)
At <- rbind( rep(1, N) , vMu , diag(rep(1, N)) , diag(rep(-1, N)) )
Amat <- t(At)


# Run the loop
for(i  in 1:length(target.mu.grid) ){
  bound <- c(1,target.mu.grid[i], l, -u)
  out <-  try(solve.QP( Dmat = mCov, dvec = dvec, 
                        Amat= Amat, bvec=bound, meq = 2)$solution, silent = TRUE)
  if(class(out)!="try-error"){
    weight.matrix[i, ] <- out
  }
}

# Remove the NAs
weight.matrix <- na.omit(weight.matrix)


# Compute the mean and sd values of optimized weights
sd.optimal <- apply(weight.matrix, 1, FUN = function(w)  sqrt(t(w) %*% mCov %*% w))
mu.optimal <- apply(weight.matrix, 1, FUN = function(w) t(w) %*% vMu)

# Compute the vector of standard deviation of the assets
vSd <- sqrt(diag(mCov))

# Plot the assets
plot(vSd, vMu, col = "gray",
     main = "Efficient frontier of selected assets",
     xlab = "Standard deviation (monthly)",
     ylab = "Average return (monthly)", 
     xlim = c(0, max(vSd) + 0.01), 
     ylim = c(0, max(vMu) + 0.005),
     las = 1)
text(vSd, vMu, labels = colnames(returns), cex = 0.7)

# Indicate in green the efficient frontier
idx <-  mu.optimal >=  mu.optimal[which.min(sd.optimal)]
lines( sd.optimal[idx], mu.optimal[idx], col = "green", lwd = 2)


# 25% weight weight constraint------------------------------------------------

# Define the weight matrix to store all optimized weights
constr.weight.matrix <- matrix(NA, nrow = length(target.mu.grid), ncol = N)

u <- rep(0.25, N) 

# Run the loop
for(i in 1:length(target.mu.grid)){
  bound <- c(1,target.mu.grid[i], l, -u)
  out <-  try(solve.QP(Dmat = mCov, dvec = dvec, 
                        Amat= Amat, bvec=bound, meq = 2)$solution, silent=TRUE)
  if(class(out)!="try-error"){
    constr.weight.matrix[i,] <- out
  }
}

# Remove the NAs
constr.weight.matrix <- na.omit(constr.weight.matrix)

# Compute the mean and sd values of optimized weights
constr.sd.optimal <- apply(constr.weight.matrix, 1, FUN = function(w) sqrt(t(w) %*% mCov %*% w))
constr.mu.optimal <- apply(constr.weight.matrix, 1, FUN = function(w) t(w) %*% vMu)

idx <- constr.mu.optimal >=  constr.mu.optimal[which.min(constr.sd.optimal)]
lines(constr.sd.optimal[idx],  constr.mu.optimal[idx], col = "blue", lwd = 2)
legend(x = "topleft",
       legend = c("weight constrain between 0 and 1", "weight constraint between 0 and 0.25"),
       col = c("green", "blue"),
       lwd = 2)



# Analyze the portfolio weights
ten_colors <- c("red", "orange", "yellow", "green", "lightblue", "blue", "violet", "purple", "pink", "grey")

barplot(t(constr.weight.matrix), beside = FALSE,
        main = "Asset weights for efficient portfolios",
        names.arg = seq(1, nrow(constr.weight.matrix)),
        xlab = "portfolio index",
        ylab = "weights",
        col = ten_colors,
        xlim = c(0,73))
legend("topright", xpd = TRUE, cex = 0.6,
       legend = rev(tickers),
       fill = rev(ten_colors))

# Compute the min and max contribution of ETF's to the portfolios
min(rowSums(constr.weight.matrix[, 6:10]))
max(rowSums(constr.weight.matrix[, 6:10]))



# BACKTESTING------------------------------------------------------------------
# without re-estimation

# We compare minimum variance, maximum Sharpe ratio and equally weighted

w.min.var <- constr.weight.matrix[which.min(constr.sd.optimal), ]
w.eq <- rep(1/N, N)
w.max.sharpe <- constr.weight.matrix[which.max(constr.mu.optimal / constr.sd.optimal), ]


# Calculate the returns for the different portfolios
returns.min.var  <- Return.portfolio(R = returns["2014-01-01/2022-02-28"],
                                     weights = w.min.var, rebalance_on = "months")

returns.eq  <- Return.portfolio(R = returns["2014-01-01/2022-02-28"],
                                weights = w.eq, rebalance_on= "months")

returns.max.sharpe <- Return.portfolio(R = returns["2014-01-01/2022-02-28"],
                                       weights = w.max.sharpe, rebalance_on = "months")


# Merge all returns into a single xts object
preturns <- merge(returns.min.var, returns.eq, returns.max.sharpe, sp500_returns["2014-01-01/2022-02-28"])

# Set the column names
colnames(preturns) <- c("Min Variance", "Equally Weighted", "Max Sharpe ratio", "S&P500")

# Evaluating the performance in terms of mean, sd and Sharpe ratio
table.AnnualizedReturns(preturns)

# Plot the performance over rolling time windows, here 24 months
charts.RollingPerformance(preturns, width = 24, legend.loc = "topleft", scale = 12)

# Plot the cumulative performance
chart.CumReturns(preturns, wealth.index = TRUE, legend.loc = "topleft",
                 main = "Cumulative performance of rebalanced portfolios") 

# Plot the annualized volatility of the portfolio
chart.RollingPerformance(R = preturns["2014-01-01/2022-02-28"],
                         width = 22, FUN = "sd.annualized", scale = 252,
                         main = "Rolling 1 month Annualized Volatility of the portfolios",
                         ylab = "", legend.loc = "topleft")


# BACKTESTING 
# with re-estimation

estim.length <- 60 # Number of months in the estimation sample
tmp <- vector(mode = "numeric", length = nrow(returns) - estim.length)
oos.perf.min.var <- oos.perf.eq <- oos.perf.max.sharpe <- tmp
n.obs <- nrow(returns)

for (j in 1:(n.obs - estim.length)){
  cat("Backtest ", j, "\n")
  
  estim.ret <- returns[j:(j + estim.length - 1), ]
  oos.ret <- returns[(j + estim.length), ]
  
  # Estimate mean and covariance matrix
  vMu <- colMeans(estim.ret)
  mCov <- cov(estim.ret)
  
  # Cardinality
  N <- ncol(mCov)
  
  # Define the grid of target return
  target.mu.grid <- seq(min(vMu),max(vMu), length.out = 100)  
  
  # Define the weight matrix to store all optimized weights
  weight.matrix     <- matrix( NA, nrow = length(target.mu.grid), ncol = N)
  
  # Define the constraints
  l <- rep(0, N)
  u <- rep(1, N)
  dvec <- rep(0, N)
  At <- rbind( rep(1, N) , vMu , diag(rep(1, N)) , diag(rep(-1, N)))
  Amat <- t(At)
  
  # Run the loop
  for(i in 1:length(target.mu.grid)){
    bound <- c(1,target.mu.grid[i] , l, -u)
    out <-  try(solve.QP( Dmat = mCov, dvec = dvec, 
                          Amat = Amat, bvec = bound, meq = 2)$solution, silent=TRUE)
    if(class(out)!="try-error"){
      weight.matrix[i,] <- out
    }
  }
  weight.matrix <- na.omit(weight.matrix)
  # Compute the mean and sd values of optimized weights
  sd.optimal <- apply(weight.matrix, 1, FUN = function(w) sqrt(t(w) %*% mCov %*% w))
  mu.optimal <- apply(weight.matrix ,1, FUN = function(w) t(w) %*% vMu)
  
  weight.min.var    <- weight.matrix[which.min(sd.optimal), ]
  # Assume risk free rate is 0
  weight.max.sharpe <- weight.matrix[which.max(mu.optimal / sd.optimal), ]
  
  oos.perf.min.var[j]    <- sum(oos.ret * weight.min.var)
  oos.perf.max.sharpe[j] <- sum(oos.ret * weight.max.sharpe)
  oos.perf.eq[j]         <- sum(oos.ret * 1 / N)
}

# Out of sample evaluation
oos.ret <- cbind(oos.perf.min.var,oos.perf.eq ,oos.perf.max.sharpe, sp500_returns["2009-02-28/2022-02-28"])
oos.ret <- xts(oos.ret, order.by = index(returns)[(estim.length + 1):nrow(returns)])
names(oos.ret)[names(oos.ret) == "sp500_adjusted.Close"] <- "sp500"


table.AnnualizedReturns(oos.ret)
# Plot the performance over rolling time windows, here 24 months
charts.RollingPerformance(R = oos.ret, width = 24, legend.loc = "topleft", scale = 12,
                          main = "24 month rolling performance of re-estimated and reballanced portfolios")


chart.CumReturns(oos.ret["2014-01-01/2022-02-28"], wealth.index = TRUE, legend.loc = "topleft",
                 main = "Cumulative return of  re-estimated and rebalanced portfolios")


# Relative performance: take 1 as reference case and plot the others relatively to them
chart.RelativePerformance(Ra = oos.ret[, 2:3]["2014-01-01/2022-02-28"],
                          Rb = oos.ret[, 1]["2014-01-01/2022-02-28"], 
                          legend.loc="topleft",
                          cex.axis = 1.3,
                          main = "Relative Performance vs minimum variance portfolio")

# Drawdown plot
chart.Drawdown(oos.ret, legend.loc = "bottomright")

# Create drowdown tables
table.Drawdowns(oos.ret[, "oos.perf.min.var"])
table.Drawdowns(oos.ret[, "oos.perf.eq"])
table.Drawdowns(oos.ret[, "oos.perf.max.sharpe"])


# Plot the annualized volatility of the portfolio
chart.RollingPerformance(R = oos.ret["2014-01-01/2022-02-28"],
                         width = 22, FUN = "sd.annualized", scale = 252,
                         legend.loc = "topleft", ylab = "",
                         main = "Rolling 1 month Annualized Volatility of the portfolios")
