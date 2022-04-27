
# Loading of packages ---------------------------------------------------------

library('quanteda')
library('quanteda.textplots')
library('seededlda')
library('sentometrics')
library('xts')
library('quantmod')
library('lubridate')

# Set file path ---------------------------------------------------------------

# If using RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Otherwise, set your working directory manually:
# setwd("")


# Load dataset
load("newsCVX.Rdata")

# check out the data
View(news)
# We get a dataframe with four columns: "date", "text", "pubTypes", "origin"  

# Pubtypes and histogram ------------------------------------------------------

sort(table(news$pubTypes), decreasing = TRUE)

# Histogram of articles from 1999 to 2016
hist(as.Date(news$date), breaks = "week", freq = TRUE, 
     main = "Histogram of News Publications from 1999 to 2016",
     xlab = "year", cex.lab = 1.5, cex.main = 1.6, cex.axis = 1.5)

# A first step is to tokenize the text.
toks <- tokens(news$text, remove_punct = TRUE, remove_symbols = TRUE,
               remove_numbers = TRUE, remove_url = TRUE)
toks <- tokens_tolower(toks)

# Beyond tokenization, textual analysis often takes a "bag-of-words" approach.
# We will call it a "document-feature matrix" (dfm).
dfm <- dfm(toks)
dfm

# Since a dfm records the frequencies at which words appear, this a good basis
# to create wordclouds.
textplot_wordcloud(dfm)

# Clean the dfm by removing stopwords 
stopwords("en")
dfm <- dfm_remove(dfm, stopwords("en"))
textplot_wordcloud(dfm)

# The result is better, but still some un-informative words remains.

# get the most frequent words
topfeatures(
  dfm,
  n = 20,
  decreasing = TRUE,
  scheme = c("count", "docfreq"),
  groups = NULL
)


# Remove uninformative words
# Remove words that have less than three characters
# Make a new wordcloud
dfm <- dfm_remove(dfm, c("said", "new", "per", "also", "us"))
dfm <- dfm_remove(dfm, c("one", "two", "three"))
dfm <- dfm_remove(dfm, c("first", "second", "third", "last"))
dfm <- dfm_remove(dfm, c("mr", "year", "years"))
dfm <- dfm_remove(dfm, c("says", "inc" ,"according"))
dfm <- dfm_remove(dfm, c("also", "can", "ing"))
dfm <- dfm_remove(dfm, min_nchar = 3)
textplot_wordcloud(dfm, max_word = 200)

# Also remove Chevron, since all the news is about this company
dfm <- dfm_remove(dfm, c("chevron"))
textplot_wordcloud(dfm, max_word = 200)


## The dfm object can then be used directly to estimate a LDA model.
## The model was saved after training and loaded again to save time

## Code to train the LDA model
#system.time({
#  set.seed(1)
#  ldaModel <- textmodel_lda(dfm, k = 4, max_iter = 2000, verbose = T,
#                            alpha = .1, beta = 0.01)
#})

## Code to save the model
#save(ldaModel, file = paste(getwd(), "ldamodel.rda", sep = "/"))

# Load the model that was previously trained
load(paste(getwd(), "ldamodel.rda", sep = "/"))

# Visualize using wordclouds
words <- t(as.matrix( ldaModel$words))
colnames(words) <- colnames( ldaModel$phi)
rownames(words) <- rownames( ldaModel$phi)
suppressWarnings(textplot_wordcloud(as.dfm(words),
                                    max_words = 800, comparison = TRUE))


# Get the most likely topic for each text
text_topic = as.data.frame(ldaModel[["theta"]])
text_topic = apply(text_topic, MARGIN = 1, FUN = which.max)
news$topic = text_topic

# Visualize the presence of topics throughout time-----------------------------



# Get the year and month of the text
news$month_yr <- floor_date(as.Date(news$date), unit = "month")
news$year     <- floor_date(as.Date(news$date), unit = "year")


# Get number of articles per year and month
topics_month_yr <- table(news$month_yr, news$topic)
topics_month_yr <- as.data.frame.matrix(topics_month_yr)

topics_year <- table(news$year, news$topic)
topics_year <- as.data.frame.matrix(topics_year)

# Add missing rows to monthly data, fill with zeros for every topic
topics_month_yr <- rbind(topics_month_yr,
                         "2000-11-01"= rep(0,4),
                         "2001-02-01"= rep(0,4),
                         "2001-06-01"= rep(0,4))
# Order the data frame
topics_month_yr <- topics_month_yr[order(as.Date(rownames(topics_month_yr))),]


# Plot the number of articles for every topic throughout time
plot(as.Date(rownames(topics_year)), topics_year[, 1],
     type = "l", col = "red", lwd = 2, xaxt = "n", 
     main = "Number of texts per category from 1999 till 2016",
     ylab = "number of texts", xlab = "year",
     ylim = c(0, ceiling(max(topics_year)/100)*100),
     xlim = c(min(as.Date(rownames(topics_year))), max(as.Date(rownames(topics_year)))))

axis(side = 1, at = as.Date(rownames(topics_year)),
     labels = format(as.Date(rownames(topics_year)), "%Y"), cex.axis = 0.8)

lines(as.Date(rownames(topics_year)), topics_year[, 2], col = "blue", lwd = 2)
lines(as.Date(rownames(topics_year)), topics_year[, 3], col = "orange", lwd = 2)
lines(as.Date(rownames(topics_year)), topics_year[, 4], col = "green", lwd = 2)
legend("topleft",
       legend = c("research", "controversies", "projects", "financial"),
       col = c("red", "blue", "orange", "green"),
       lwd = 1)


# Analyze the impact of news articles on the stock return----------------------

# Get the stock price data for CVX from 1999 till 2022
getSymbols("CVX",
           from = "1998-12-01",
           to = "2016-12-31")

CVX_adjusted <- CVX[, 6]
CVX_monthly <- xts::to.monthly(CVX_adjusted, indexAt = "firstof")[, 4]

# Calculate the returns
CVXreturns <- Return.calculate(CVX_monthly, method = "log")
CVXreturns <- CVXreturns[(-1), ]


# Get correlations of the number of news articles and the returns
ccf(topics_month_yr[, 1], coredata(CVXreturns)[, 1])
ccf(topics_month_yr[, 2], coredata(CVXreturns)[, 1])
ccf(topics_month_yr[, 3], coredata(CVXreturns)[, 1],
    main="Cross correlation of news on projects and monthly returns")
ccf(topics_month_yr[, 4], coredata(CVXreturns)[, 1])

#-------------------------------------------------------------------------------
## Compute a time series of weekly sentiment.

# We can re-use the tokens from the previous analysis
head(toks[[1]], 20)

# We will apply a lexicon on the tokens to compute sentiment
# The Loughran-McDonald lexicon is available within the sentometrics package
LM <- list(positive = list_lexicons$LM_en[y > 0, x],
           negative = list_lexicons$LM_en[y < 0, x])
sapply(LM, head, 10)

lexs <- sento_lexicons(lexiconsIn = list_lexicons["LM_en"],
                       valenceIn = list_valence_shifters[["en"]])
sentiment <- compute_sentiment(news$text,
                               lexicons = sento_lexicons(list_lexicons["LM_en"]),
                               how = "proportionalPol")

# "sentiment" now contains the measured sentiment for each text.
# We need to aggregate this sentiment for each text in a given month,
# to create a monthly sentiment

### Creating an xts object and aggregate to monthly sentiment 
sentiment <- xts(sentiment$`LM_en`, order.by = as.Date(news$date) )
aggregatedSentiment <- apply.monthly(sentiment , mean)
head(aggregatedSentiment)

plot(aggregatedSentiment, main = "Monthly aggragated sentiment")

# Correlation of sentiment and returns
ccf(coredata(aggregatedSentiment)[, 1], coredata(CVXreturns)[, 1],
    main = "Cross correlation of aggragated sentiment and monthly returns")