# default values
confidenceLevel = 0.95
predictionNum = 4


# TODO: Lieferungskosten abziehen

# import data -> data frame
sales.csv = read.table(file = "./data/AA_Umsatzbelege.csv", header = TRUE, sep=";")

# remove all entrys not having "S" as SHKZG (SOLL.HABEN.KENNZSICHEN)
sales.csv <- sales.csv[sales.csv$SHKZG=="S",]


# KOART (Kontoart) (D = Debitoren, K = Kreditoren)

# Split data frame by AUGDT (Datum des Ausgleichs), BUKRS (Buchungskreis), sum DMBTR (Betrag in Hauswährung)
sales.aggregated <- aggregate(
  sales.csv$DMBTR, 
  by = list(
    companycode = sales.csv$BUKRS,
    weeknumber = as.numeric(format(as.Date(sales.csv$AUGDT, format = "%d.%m.%Y"), "%U")) + 1
  ),
  FUN=sum, 
  na.rm=TRUE
)

# Support for multiple company codes
sales.prediction.list <- by(sales.aggregated, list(companycode=sales.aggregated$companycode), function(x) {  

  sales.wn.max <- max(x$weeknumber)
  sales.wn.min <- min(x$weeknumber)
  
  # Add 0 for missing weeknumbers 
  x.expanded <- merge(expand.grid(companycode = unique(x$companycode), weeknumber = seq(sales.wn.min, sales.wn.max, 1)), 
                      x, all=TRUE, by=c("companycode","weeknumber"))
  x.expanded[is.na(x.expanded)] <- 0

  sales.aggregated.length <- nrow(sales.aggregated)
  
  # linear regression
  sales.aggregated.lm <- lm(x ~ weeknumber, data = x.expanded)
  
  # newdata with variables with which to predict
  sales.prediction.data <- data.frame(weeknumber=seq(sales.wn.max + 1, sales.wn.max + predictionNum, 1))

  # prediction
  sales.prediction.predict <- predict(sales.aggregated.lm, sales.prediction.data,  se.fit = TRUE, interval = "confidence", level = confidenceLevel)
  
  # returns df of weeknumber, prediction, and confidence level
  sales.prediction.frame <- data.frame(sales.prediction.data$weeknumber, sales.prediction.predict, confidenceLevel)
  names(sales.prediction.frame) <- c("weeknumber", "fit", "min", 'max', 'sefit', 'df', 'residualscale','confidence')
  
  return (sales.prediction.frame)
})

# implode list of dataframes to dataframe
sales.prediction = do.call(rbind.data.frame, sales.prediction.list)

# clean up
rm(list = ls())