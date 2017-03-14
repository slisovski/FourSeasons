## ---- message=F, warning=F-----------------------------------------------
library(FourSeasons)
  data("tempYNP")

tempYNP[1:5, c("Date", "Tmax", "Tmin")]

## ------------------------------------------------------------------------
range(tempYNP$Date)

## ---- fig.height=5, fig.width=7------------------------------------------
sTab <- defineSeasons(tempYNP$Date, tempYNP$Tmin, frequency = "daily")

## ------------------------------------------------------------------------
head(sTab)

## ---- fig.height=5, fig.width=7------------------------------------------
seas <- AmpPred(sTab, info.periods = 4, forecast.periods = 1)

## ------------------------------------------------------------------------
seas$summary[1:8,] ## First 8 rows of the summary output

