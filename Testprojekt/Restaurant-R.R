# Constants
kPredictionQuarters <- 4
kConfidenceLevel <- 0.95

# Import Data from csv file
sales.csv <- read.csv(file = "./data/RestaurantUmsaetze.csv", header = TRUE, sep = ";", strip.white = TRUE, dec = ",")

# Fit linear model. Formula = sales_teur ~ t + seasonal_trend 
sales.prediction.lm <- lm(sales_teur ~ t + seasonal_trend, data = sales.csv)

# Dataframe in which to look for variables with which to predict
sales.prediction.newdata <- data.frame(t = seq(max(sales.csv$t) + 1, max(sales.csv$t) + kPredictionQuarters, 1))

# Calculate mean seasonal trend for each quarter
sales.seasonaltrend.mean <- aggregate(
  sales.csv$seasonal_trend, 
  by = list(quarter = sales.csv$quarter), 
  FUN = mean, 
  na.rm = TRUE)$x

# Repeat mean seasonal trend to fit kPredictionQuarters
sales.prediction.newdata$seasonal_trend <- rep(sales.seasonaltrend.mean, length.out=kPredictionQuarters)

# Predict sales based on linear model sales.prediction.lm
sales.prediction.predict <- predict(sales.prediction.lm, newdata = sales.prediction.newdata,  se.fit = TRUE, interval = "confidence", level = kConfidenceLevel)

# Return named dataframe with weeknumber, predicted values and confidence level
sales.prediction.frame <- data.frame(sales.prediction.newdata$t, sales.prediction.predict, kConfidenceLevel)
names(sales.prediction.frame) <- c("t", "FIT", "MIN", "MAX", "SEFIT", "DF", "RESIDUALSCALE","CONFIDENCE_LEVEL")

sales.prediction.frame

# Plot
names(sales.prediction.frame)[names(sales.prediction.frame)=="FIT"] <- "sales_teur"
output <- rbind(sales.csv[, c("t", "sales_teur") ], sales.prediction.frame[, c("t","sales_teur")])
plot(output$t,output$sales_teur, main = "Umsätze mit Prognose für Folgejahr", xlab = "Quartal", ylab = "Umsatz in TEUR", type = "b", col = "red")
abline(sales.prediction.lm, lty = 2, col = "blue")