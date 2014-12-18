# Wochen in Vektor speichern
geschaeftsWochen = c()
trendwerte = c()
prognoseUmsatz = c()
umsatzaggregiert = c()

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

sales.prediction.df

# clean up
rm(list = ls())



# TMP
AA_Umsatzbelege = sales.csv

zeilenAnzahlTabelle = nrow(AA_Umsatzbelege)

# Aggregation der Umsatzaetze pro Woche
kw = as.numeric(format(as.Date(AA_Umsatzbelege[1, 5], format = "%d.%m.%Y"), "%U")) + 1
ersteKalenderWoche = kw
letzteKalenderWoche = as.numeric(format(as.Date(AA_Umsatzbelege[nrow(AA_Umsatzbelege), 5], format = "%d.%m.%Y"), "%U")) + 1
anzahlGeschaeftswochen = ( letzteKalenderWoche - kw ) + 1

# KW Geschäftswochen bestimmen

for (i in kw : letzteKalenderWoche){
  geschaeftsWochen[i - (kw-1)] = i
  umsatzaggregiert[i - (kw-1)] = 0
}


for (i in 1:zeilenAnzahlTabelle){
  if(AA_Umsatzbelege[i, 7] == "S"){
    datumX = as.Date(AA_Umsatzbelege[i, 5], format = "%d.%m.%Y")
    weekX = as.numeric(format(datumX, "%U")) + 1
    if(weekX > kw){
      kw = kw+1
    }
    # Umsatz aktuelle Geschaeftswoche berechnen
    if(weekX == kw){
      umsatzaggregiert[kw-(ersteKalenderWoche-1)] = umsatzaggregiert[kw-(ersteKalenderWoche-1)] + AA_Umsatzbelege[i, 8]
    }
  }
}

# Regressionsgerade ermitteln

fit = lm(umsatzaggregiert ~ geschaeftsWochen)

# Trendwerte bestimmen

regressionsGerade = coefficients(fit)
steigungReg = regressionsGerade[2]
regressionY_Achse = regressionsGerade[1]

#Trendwerte befuellen

for (i in ( 1 + anzahlGeschaeftswochen) : (4 + anzahlGeschaeftswochen) ) {
  t = letzteKalenderWoche + (i - anzahlGeschaeftswochen) 
  geschaeftsWochen[i] = t
  umsatzaggregiert[i] = regressionY_Achse + (steigungReg * t )  
}

# Regression refreshen
fit = lm(umsatzaggregiert ~ geschaeftsWochen)

# Intervallschaetzung
new = data.frame(Umsatz = c(anzahlGeschaeftswochen + 1, anzahlGeschaeftswochen + 2 , anzahlGeschaeftswochen + 3 , anzahlGeschaeftswochen + 4))
intervallschaetzung = predict(fit, newdata = new, se.fit = TRUE, interval = "confidence", level = confidenceLevel)
intervallschaetzungTabelle1 = data.frame(intervallschaetzung)

for (i in 1 : nrow(intervallschaetzungTabelle1) ){
  intervallschaetzungTabelle1[i, 7] = confidenceLevel  
}

colnames(intervallschaetzungTabelle1, do.NULL = FALSE)
colnames(intervallschaetzungTabelle1) = c("Predictive_Average_Value", "Predictive_Lower_Value", "Predictive_Upper_Value", "Standardabweichung", "T-Freiheitsgrade", "Varianz", "Confidence_Level")