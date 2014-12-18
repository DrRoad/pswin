# default values
confidenceLevel = 0.95


# TODO: Support currency 
# TODO: Append weeknumbers without any sales

# import data -> data frame
sales.csv = read.table(file = "./data/AA_Umsatzbelege.csv", header = TRUE, sep=";")

# remove all entrys not having S as SOLL.HABEN.KENNZSICHEN
sales.csv <- sales.csv[sales.csv$SOLL.HABEN.KENNZSICHEN=="S",]

# split data frame by AUSGLEICHSDATUM, BUCHUNGSKREIS, sum BETRAG
sales.aggregated <- aggregate(
  sales.csv$BETRAG, 
  list(
    companycode=sales.csv$BUCHUNGSKREIS,
    weeknumber=as.numeric(format(as.Date(sales.csv$AUSGLEICHDATUM, format = "%d.%m.%Y"), "%U")) + 1
  ), 
  function(x){
    # TODO: maybe merge into sum function
    return(sum(x))
  }
)

# Support for multiple company codes
sales.prediction.list <- by(sales.aggregated, list(companycode=sales.aggregated$companycode), function(x) {
  
  # removes companycode from data.frame
  # y <- subset(x, select= -companycode)

  sales.wn.max <- max(x$weeknumber)
  sales.aggregated.length <- nrow(sales.aggregated)
  
  # linear regression
  sales.aggregated.lm <- lm(x ~ weeknumber, data = x)
  
  # newdata with variables with which to predict
  sales.prediction.data <- data.frame(weeknumber=seq(sales.wn.max + 1, sales.wn.max + 1 + round(sales.aggregated.length/3), 1))

  # prediction
  sales.prediction.predict <- predict(sales.aggregated.lm, sales.prediction.data,  se.fit = TRUE, interval = "confidence", level = confidenceLevel)
  
  # returns df of weeknumber, prediction, and confidence level
  sales.prediction.frame <- data.frame(sales.prediction.data$weeknumber, sales.prediction.predict, confidenceLevel)
  names(sales.prediction.frame) <- c("weeknumber", "fit", "min", 'max', 'sefit', 'df', 'residualscale','confidence')
  
  return (sales.prediction.frame)
})

# implode list of dataframes to dataframe
sales.prediction.df = do.call(rbind.data.frame, sales.prediction.list)

# clean up
rm(list = ls())