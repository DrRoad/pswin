# Constants
kConfidenceLevel <- 0.80
kPredictionNum <- 4

# Import Data from csv file
bseg.table <- read.table(file = "./data/BSEG.csv", header = TRUE, sep = ";", strip.white = TRUE, dec = ",")

# Remove all entrys not having "S" as SHKZG (SOLL.HABEN.KENNZEICHEN)
bseg.table <- bseg.table[bseg.table$SHKZG == "S", ]

# Remove rows where KOART != D or K
bseg.table <- bseg.table[bseg.table$KOART == "K" | bseg.table$KOART == "D", ]  # KOART (Kontoart) (D = Debitoren, K = Kreditoren)
bseg.table$KOART <- factor(bseg.table$KOART)  # Reset factor values

# Fix for german thousand and decimal seperator
bseg.table$DMBTR <- as.double(gsub(',', '.', gsub('\\.', '', bseg.table$DMBTR)))

# Apply function to each level of factor KOART (D and K) to achieve individual predictions
bseg.prediction <- by(bseg.table, list(type = bseg.table$KOART), simplify = FALSE, function(x) {  
  
  # Split data frame by AUGDT (Datum des Ausgleichs), BUKRS (Buchungskreis) AND sum DMBTR (Betrag in Hauswährung)
  sales.aggregated <- aggregate(
    x = as.double(x$DMBTR), 
    by = list(
      companycode = x$BUKRS,
      weeknumber = as.numeric(format(as.Date(x$AUGDT, format = "%d.%m.%Y"), "%U")) + 1
    ),
    FUN = sum,
    na.rm = TRUE
  )
  
  # Proceed individual prediction for each companycode
  sales.prediction.list <- by(sales.aggregated, list(companycode = sales.aggregated$companycode), function(x) {  
    
    # Add dataframe with missing weeknumbers filled with zero
    x.expanded <- merge(expand.grid(companycode = unique(x$companycode), weeknumber = seq(min(x$weeknumber), max(x$weeknumber), 1)), 
                        x, all = TRUE, by=c("companycode","weeknumber"))
    x.expanded[is.na(x.expanded)] <- 0
    
    # Fit linear model
    sales.aggregated.lm <- lm(x ~ weeknumber, data = x.expanded)
    
    # Calculate next weeknumbers 
    sales.prediction.newdata <- data.frame(weeknumber = seq(max(x$weeknumber) + 1, max(x$weeknumber) + kPredictionNum, 1))
    
    # Predict sales / expenses based on linear model sales.aggregated.lm
    sales.prediction.predict <- predict(sales.aggregated.lm, sales.prediction.newdata,  se.fit = TRUE, interval = "confidence", level = kConfidenceLevel)
    
    # Return named dataframe with companycode, weeknumber, predicted values and confidence level
    sales.prediction.frame <- data.frame(unique(x.expanded$companycode), sales.prediction.newdata$weeknumber, sales.prediction.predict, kConfidenceLevel)
    names(sales.prediction.frame) <- c("COMPANYCODE", "WEEKNUMBER", "FIT", "MIN", 'MAX', 'SEFIT', 'DF', 'RESIDUALSCALE','CONFIDENCE')
    
    return (sales.prediction.frame)
  })
  
  # Implode list of dataframes to extensive dataframe
  sales.prediction = do.call(rbind.data.frame, sales.prediction.list)
  
  # Add K and D factors to dataframe
  sales.prediction["TAG"] <- unique(x$KOART)
  
  return (sales.prediction)
})

# Merge the splitted list of type D and K
bseg.prediction <- do.call(rbind.data.frame, bseg.prediction)

# Update list set NaN to zero to fit the database structure
bseg.prediction[is.na(bseg.prediction)] <- 0

# Set negative values to zero for semantic reasons
bseg.prediction$FIT[bseg.prediction$FIT < 0] <- 0
bseg.prediction$MIN[bseg.prediction$MIN < 0] <- 0
bseg.prediction$MAX[bseg.prediction$MAX < 0] <- 0

# Return the computed result list to HANA
bseg.prediction