# default values
confidenceLevel = 0.80
predictionNum = 4

# import data -> data frame
bseg.table = read.table(file = "./data/BSEG.csv", header = TRUE, sep=";", strip.white=TRUE, dec = ",")

# remove all entrys not having "S" as SHKZG (SOLL.HABEN.KENNZEICHEN)
bseg.table <- bseg.table[bseg.table$SHKZG=="S", ]   # TODO (Sascha): Bei Kreditoren auch S?!

# Remove rows where KOART != D or K
bseg.table <- bseg.table[bseg.table$KOART=="K" | bseg.table$KOART=="D", ]  # KOART (Kontoart) (D = Debitoren, K = Kreditoren)
bseg.table$KOART <- factor(bseg.table$KOART)  # Reset factor values

# Fix for german thousand and decimal seperator
bseg.table$DMBTR <- as.double(gsub(',', '.', gsub('\\.', '', bseg.table$DMBTR)))

# Make prediction for sales (K) and charges (D)
bseg.prediction <- by(bseg.table, list(type = bseg.table$KOART), simplify = FALSE, function(x) {  
  
  # Split data frame by AUGDT (Datum des Ausgleichs), BUKRS (Buchungskreis) AND sum DMBTR (Betrag in Hauswährung)
  sales.aggregated <- aggregate(
    x = as.double(x$DMBTR), 
    by = list(
      companycode = x$BUKRS,
      weeknumber = as.numeric(format(as.Date(x$AUGDT, format = "%d.%m.%Y"), "%U")) + 1
    ),
    FUN=sum, 
    na.rm=TRUE
  )
  
  # Proceed prediction for each companycode
  sales.prediction.list <- by(sales.aggregated, list(companycode=sales.aggregated$companycode), function(x) {  
    
    # Add 0 for missing weeknumbers 
    x.expanded <- merge(expand.grid(companycode = unique(x$companycode), weeknumber = seq(min(x$weeknumber), max(x$weeknumber), 1)), 
                        x, all=TRUE, by=c("companycode","weeknumber"))
    x.expanded[is.na(x.expanded)] <- 0
    
    # linear regression
    sales.aggregated.lm <- lm(x ~ weeknumber, data = x.expanded)
    
    # newdata with variables with which to predict
    sales.prediction.data <- data.frame(weeknumber=seq(max(x$weeknumber) + 1, max(x$weeknumber) + predictionNum, 1))
    
    # prediction
    sales.prediction.predict <- predict(sales.aggregated.lm, sales.prediction.data,  se.fit = TRUE, interval = "confidence", level = confidenceLevel)
    
    # returns df of weeknumber, prediction, and confidence level
    sales.prediction.frame <- data.frame(unique(x.expanded$companycode), sales.prediction.data$weeknumber, sales.prediction.predict, confidenceLevel)
    names(sales.prediction.frame) <- c("COMPANYCODE", "WEEKNUMBER", "FIT", "MIN", 'MAX', 'SEFIT', 'DF', 'RESIDUALSCALE','CONFIDENCE')
    
    return (sales.prediction.frame)
  })
  
  # implode list of dataframes to dataframe
  sales.prediction = do.call(rbind.data.frame, sales.prediction.list)
  
  # add k and d
  sales.prediction["TAG"] <- unique(x$KOART)
  
  return (sales.prediction)
})

bseg.prediction <- do.call(rbind.data.frame, bseg.prediction)

# Remove NAN
bseg.prediction <- bseg.prediction[complete.cases(bseg.prediction), ]

bseg.prediction
# clean up
#rm(list = ls())