# Chevron stock analysis

In this project I analysed the stock of Chevron (CVX). The analysis consists of four parts: initial analysis, text analysis, return analysis and portfolio analysis.
The main insights are given below.

## Initial analysis
- Chevron is a large US company in the oil & gas industry
- The company has known significant growth in its history
- Chevron has been the topic of many controversies
- The stock has an annualized **return of 13%** and an annualized **volatility of 28%** from 1999 till februari 2022.

## Text analysis
- News articles on Chevron generaly cover **four different topics**
  1. Research
  2. Controversies
  3. Projects
  4. Finance
- The **sentiment** on Chevron is mostly **negative**
- The amount of articles per topic and the sentiment are **not informative for the stock returns**

## Return analysis
- A **GARCH (1, 1)** model with constant mean and **student distribution** is most suited to predict the stocks performance
- This model is **able to estimate the 5% Value at Risk (VaR)**
- The stock is relatively stable with some growth potential

## Portfolio analysis
- Combining the Chevron stock with other stocks and ETF's can improve the returns and lower the risk.
- **Backtesting a maximal Sharpe portfolio resulted in higher returns than the S&P500**
