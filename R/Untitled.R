
Data <- read.csv("~/Downloads/ENV1_PrecipForSimeon.csv", header = T)
Data$Date <- as.character(Data$Date)
    yr <- as.numeric(substring(Data$Date, nchar(Data$Date)-1))
    Data$Date <- as.POSIXct(paste(substring(Data$Date, 1, nchar(Data$Date)-2),
                                ifelse(yr>17, 1900, 2000)+yr),format = "%m/%d/%Y")


  sTab <- defineSeasons(Data$Date, Data[,8], frequency = "monthly", plot=T)
  seas <- AmpPred(sTab, info.periods = 4, forecast.periods = 1)







SD <- function(x) {

  sTab <- try(defineSeasons(Date, x, frequency = "monthly", plot=F))

  seas <- try(AmpPred(sTab, info.periods = 4, forecast.periods = 1, plot=F))

  Amp <- try(mean(seas$summary$amplitude, na.rm=T))

  Pre <- try(mean(seas$summary$predictability, na.rm=T))

  AmpSt <- try(mean( (seas$summary$amplitude - min(seas$summary$amplitude)) / (max(seas$summary$amplitude) - min(seas$summary$amplitude))))

  Seasonality <- try(AmpSt * Pre)

  return(list(Amp=Amp, Pre=Pre, Seasonality = Seasonality))

}


res <- lapply(Precip,SD)
res2 <- do.call(rbind,res)
head(res2)
