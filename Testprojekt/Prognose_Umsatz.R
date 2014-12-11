# Wochen in Vektor speichern
geschaeftsWochen = c()
trendwerte = c()
prognoseUmsatz = c()
umsatzaggregiert = c()

confidenceLevel = 0.90
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