##' Function that helps to check whether a time series has significant periodicity.
##'
##' @title Check for Periodicity in time series using wavelet analysis
##' @param {tm} the time (date; as a POSIXlt or POSIXct class object) of the observation
##' @param {y} the observations.
##' ##' @param {frequency} the frequency of the observations; so far either 'daily', 'weekly' or 'monthly'.
##' @param {plot} logical, if TRUE a plot will be produced.
##' @return ...
#' \describe{
#'   \item{date}{...}
#' }
##' @author Simeon Lisovski
##' @examples
##' data(tempYNP)
##' # sTab <- defineSeasons(tempYNP$Date, tempYNP$Tmin, frequency = "daily")
##' @importFrom biwavelet wt
##' @importFrom zoo na.approx
##' @importFrom graphics plot lines abline axis
##' @importFrom stats ts stl optim
##' @export checkPeriodicity
checkPeriodicity <- function(tm, y, frequency = "daily", plot = TRUE) {

  if(!all(class(tm)%in%c("POSIXct", "POSIXt"))) {
    stop(sprintf("Date must be provided as POSIXct class objects"), call. = F)
  }

  difft <- apply(cbind(tm[-length(tm)], tm[-1]), 1, function(x) (x[2] - x[1])/60/60/24)
  freq  <- as.numeric(as.character(as.data.frame(table(round(difft,0)))[order(as.data.frame(table(round(difft,0)))[, 2], decreasing = T), ][1, 1]))
  if((frequency=="daily" & freq!=1) | (frequency=="weekly" & !freq%in%c(6:7)) | (frequency=="monthly" & !freq%in%c(28:33))) {
    stop(sprintf("The specified frequency does not fit the data."), call. = F)
  }

  tmp   <- data.frame(date = seq(min(tm), max(tm), by = ifelse(frequency=="daily", "day", ifelse(frequency=="weekly", "week", "month"))))
  tmp$y <- merge(tmp, data.frame(date = tm, y = y), all.x = T)$y
  tmp$y <- na.approx(tmp$y, rule = 2)

  f    <- nrow(tmp)/ifelse(frequency=="daily", 365, ifelse(frequency=="weekly", 52, 12))
  freq <- ifelse(frequency=="daily", 365, ifelse(frequency=="weekly", 52, 12))

  wt  <- wt(cbind(1:length(y),y))
  power  <- log2(wt$power.corr)

  time   <- wt$t
  period <- wt$period/

  # tmp.pow <- apply(power[,-c(1:freq, (ncol(power)-(freq-1)):ncol(power))], 1, median, na.rm = T)
  # tmp.sig <- apply(wt$signif[,-c(1:freq, (ncol(power)-(freq-1)):ncol(power))], 1, median, na.rm = T)
  #
  # plot(period, tmp.pow, type = "o")
  # points(period, tmp.pow, pch = 16, col = ifelse(tmp.sig>1, "red", "grey90"))
  #

  if(plot) {

    fill.cols <- c("#00007F", "blue", "#007FFF", "cyan",
                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
    col.pal <- colorRampPalette(fill.cols)

    plot(wt, yaxt = "n")
    axis(2, at = 1.5)




    axis(2, at = c(1:dim(wt$power)[1])[c(TRUE, rep(FALSE, 5))], labels = c(round(wt$period/freq, 1))[c(TRUE, rep(FALSE, 5))], las = 1)

  }

  return(out)
}








##' Function that helps to define the seasonality (periodicity) of the time-series.
##'
##' Based on time-series analysis, this function first decomposes the data into the following components; seasonal, trend and remainder.
##' Next, the periodicity of the time-series will be defined by using a cosine function fitted to the seasonal component (using least-square).
##' The minima of the cosine curve can then be used to separate the periods (nessesary in other functions of the DOSeasons package).
##'
##' @title Define seasonality of time-series
##' @param {tm} the time (date; as a POSIXlt or POSIXct class object) of the observation
##' @param {y} the observations
##' @param {frequency} the frequency of the observations; so far either 'daily', 'weekly' or 'monthly'.
##' @param {plot} logical, if TRUE a plot will be produced.
##' @return A data frame with 7 variables:
  #' \describe{
  #'   \item{date}{teh date of the time-series observations}
  #'   \item{y}{the data}
  #'   \item{period}{the number of the period}
  #'   \item{seasonal}{the seasonal component}
  #'   \item{trend}{the trend component}
  #'   \item{remainder}{the remainder component}
  #'   \item{cos.fit}{the best fit of the cosine function}
  #' }
##' @author Simeon Lisovski
##' @examples
##' data(tempYNP)
##' sTab <- defineSeasons(tempYNP$Date, tempYNP$Tmin, frequency = "daily")
##' @importFrom zoo na.approx
##' @importFrom graphics plot lines abline axis
##' @importFrom stats ts stl optim
##' @export defineSeasons
defineSeasons <- function(tm, y, frequency = "daily", plot = TRUE) {

  if(!all(class(tm)%in%c("POSIXct", "POSIXt"))) {
    stop(sprintf("Date must be provided as POSIXct class objects"), call. = F)
  }

  difft <- apply(cbind(tm[-length(tm)], tm[-1]), 1, function(x) (x[2] - x[1])/60/60/24)
  freq  <- as.numeric(as.character(as.data.frame(table(round(difft,0)))[order(as.data.frame(table(round(difft,0)))[, 2], decreasing = T), ][1, 1]))
  if((frequency=="daily" & freq!=1) | (frequency=="weekly" & !freq%in%c(6:7)) | (frequency=="monthly" & !freq%in%c(28:33))) {
    stop(sprintf("The specified frequency does not fit the data."), call. = F)
  }

  tmp   <- data.frame(date = seq(min(tm), max(tm), by = ifelse(frequency=="daily", "day", ifelse(frequency=="weekly", "week", "month"))))
    tmp$y <- merge(tmp, data.frame(date = tm, y = y), all.x = T)$y
    tmp$y <- na.approx(tmp$y, rule = 2)

  f <- nrow(tmp)/ifelse(frequency=="daily", 365, ifelse(frequency=="weekly", 52, 12))

  ts <- ts(tmp$y, frequency = f,
           start = as.numeric(unlist(strsplit(format(tmp$date[1], "%Y %j"), " "))))
  fit = stl(ts, s.window='periodic')

  Mx <- fit$time.series[,1] - mean(fit$time.series[,1], na.rm = T)

  fit0  <- optim(fn = leastS.cos, par = c(a = 25, b = 0), f = f, Mx = Mx, sd = 0.001)
  curve <- fit0$par[1]*cos(pi*((1:length(Mx))/(length(Mx)/((length(Mx)/f)*2))) +
                               (pi+fit0$par[2])) +  mean(fit$time.series[,1], na.rm=T)


  spl  <- which(diff(curve[-length(curve)])<0 & diff(curve[-1])>0) +1
  tmp3 <- split(data.frame(tmp), f = cut(1:nrow(tmp), breaks = c(0, spl, nrow(tmp))))



  out <- data.frame(date = tmp$date, y = tmp$y,
                    period = unlist(as.vector(sapply(1:length(tmp3), function(x) rep(x[1], nrow(tmp3[[x[1]]]))))),
                    seasonal = fit$time.series[,1],
                    trend    = fit$time.series[,2],
                    remainder = fit$time.series[,3],
                    cos.fit = curve)

  if(plot) {
    opar <- par(mfrow = c(4, 1), mar = c(0,6,0,6), oma = c(3,0,3,0), cex.lab = 1.5)
    plot(out$date, out$y, pch = 16, type = "o", cex = 0.5, xaxt = "n", ylab = "Data")
    plot(out$date, out$seasonal, pch = 16, type = "o", cex = 0.5, xaxt = "n", ylab = "Seasonal", yaxt = "n")
    axis(4)
    plot(out$date, out$trend, type = "l", lwd = 1.5, xaxt = "n", ylab = "Trend")
    plot(out$date, out$y, pch = 16, type = "o", cex = 0.5, xaxt = "n", ylab = "Output", col = "grey90")
    par(new = T)
    plot(out$date, out$cos.fit, type ="n", yaxt = "n", ylab = "", xlab = "")
    foo <- lapply(split(out, f = out$period), function(x) lines(x[,1], x[,7], lwd = 1.5,
                  col = ifelse((x[1,3]/2)==floor(x[1,3]/2), "firebrick", "cornflowerblue")))
    abline(v = out$date[spl], lty = 2, col = "grey30")
    par(opar)
  }

  return(out)
}




##' Function that helps to define the seasonality (periodicity) of the time-series.
##'
##' Based on time-series analysis, this function first decomposes the data into the following components; seasonal, trend and remainder.
##' Next, the periodicity of the time-series will be defined by using a cosine function fitted to the seasonal component (using least-square).
##' The minima of the cosine curve can then be used to separate the periods (nessesary in other functions of the DOSeasons package).
##'
##' @title Seasonal amplitude and predictability within time-series observations
##' @param {data} data.frame as produced by defineSeasons
##' @param {info.periods} the number of periods used as prior inforamtion to generate predictions
##' @param {forecast.period} the number of periods to be forecasted within each step
##' @param {cuttoff} exlude incomplete periods (in the beginning and the end); the cuttoff defines a percetnage of the complete periods.
##' @param {amp.probs} the qunatiles used to define the seasonal amplitude.
##' @param {plot} logical, of TRUE a plot will be drawn.
##' @return A list with 2 data frames:
#' \describe{
#'   \item{summary}{data frame with amplitude and predictability for each seasonal period}
#'   \item{output}{data frame with all output variables}
#' }
##' @author Simeon Lisovski
##' @examples
##' data(tempYNP)
##' sTab <- defineSeasons(tempYNP$Date, tempYNP$Tmin, frequency = "daily")
##' seas <- AmpPred(sTab)
##' @importFrom parallel detectCores makeCluster
##' @importFrom doParallel registerDoParallel stopImplicitCluster
##' @importFrom foreach foreach
##' @importFrom forecast stlf
##' @importFrom graphics plot lines arrows
##' @importFrom stats ts quantile aggregate
##' @export AmpPred
AmpPred <- function(data, info.periods = 4, forecast.periods = 1, cuttoff = 70, amp.probs = c(0.975, 0.025), plot = TRUE) {

  prds <- as.data.frame(table(data$period))
  if(prds[1,2]<(median(prds[,2])*(cuttoff/100))) data <- data[data$period!=prds[1,1],]
  if(prds[nrow(prds),2]<(median(prds[,2])*(cuttoff/100))) data <- data[data$period!=prds[nrow(prds),1],]

  tmp <- unique(data$period)

  ind.prds <- suppressWarnings(cbind(min(tmp):(max(tmp)-info.periods), ((min(tmp)-1)+info.periods):(max(tmp)-1),
                                     prds[(prds[,1]%in%tmp),2][-c(1:4)]))

  cl0 <- detectCores()
  cl  <-makeCluster(cl0-1)
  registerDoParallel(cl)

  fc <- foreach:::foreach(loop=1:nrow(ind.prds), .combine = rbind, .packages = "forecast") %dopar% {
    ts  <- ts(data$y[data$period%in%c(ind.prds[loop,1]:ind.prds[loop,2])], frequency = median(ind.prds[loop,3]))
    ets <- stlf(ts, method = "arima", h = ind.prds[loop,3])
    cbind(c(ets$mean), as.matrix(ets$lower), as.matrix(ets$upper))
    }

  out <- data.frame(data[data$period%in%c((ind.prds[1,2]+1):(max(ind.prds[,2])+1)), ], as.matrix(fc))

  fcy <- foreach:::foreach(loop=unique(out$period), .combine = rbind) %dopar% {
    tmp <- out[out$period==loop,]
    RSS <- tmp$y - mean(tmp$y, na.rm = T)
    SSE <- sum((tmp$y-tmp$c.ets.mean.)^2, na.rm = T)

    cbind(prds  = loop,
          pred  = (sum(RSS^2, na.rm = T)-SSE)/sum(RSS^2, na.rm = T))
   }

  stopImplicitCluster()

  year <- aggregate(as.numeric(format(data$date, "%Y")), by = list(data$period), FUN = mean)$x[1]

  out.summary <- data.frame(mean.year  = round(year+c(0:(length(unique(data$period))-1)),0),
                            period     = unique(data$period),
                            amplitude  = c(aggregate(data$y, by = list(data$period), function(x) diff(quantile(x, rev(amp.probs))))$x),
                            predictability = fcy[match(unique(data$period), fcy[,1]),2])
  if(plot) {
    opar <- par(mfrow = c(3, 1), mar = c(0,6,0,6), oma = c(3,0,3,0), cex.lab = 1.5)
    plot(data$date, data$y, type = "o", cex = 0.25, pch = 16, col = "grey60",
         ylab = "Amplitude", xaxt = "n", xlab = "")
    arrows(aggregate(data$date, by = list(data$period), FUN = mean)$x,
           aggregate(data$y, by = list(data$period), function(x) quantile(x, amp.probs[1]))$x,
           aggregate(data$date, by = list(data$period), FUN = mean)$x,
           aggregate(data$y, by = list(data$period), function(x) quantile(x, amp.probs[2]))$x,
           code = 3, angle = 90, length = 0.05, lwd = 2)

    plot(data$date, data$y, type = "n",
         ylim = range(c(out$as.matrix.ets.lower..95., out$as.matrix.ets.upper..95.), na.rm = T),
         xaxt = "n", xlab = "", ylab = "Predictions")
    polygon(c(out$date, rev(out$date)),
            c(out[,10], rev(out[,12])), col = rgb(.4,.6,.92, alpha = 0.5), border  = NA)
    polygon(c(out$date, rev(out$date)),
            c(out[,9], rev(out[,11])), col = rgb(.4,.6,.92, alpha = 0.9), border  = NA)
    lines(out$date, out$c.ets.mean., col = "darkblue", lwd = 1)
    plot(out$date, out$y-out$c.ets.mean., type = "h", xlim = range(data$date),
         xaxt = "s", xlab = "", ylab = "Residuals")
    par(opar)
  }

  return(list(summary = out.summary,
              output  = out))
}


## Internal function
leastS.cos <- function(params, f, Mx, sd = 0.001) {
  fit  <- params[1]*cos(pi*((1: length(Mx))/(length(Mx)/((length(Mx)/f)*2))) + (pi+params[2]))
  -sum(dnorm(x= Mx[!is.na(Mx)], mean=fit[!is.na(Mx)], sd=sd, log=TRUE))
}



#' Temperature data from Yosemity National Park
#'
#' Daily minimum and maximum temperatures.
#'
#' Data source: NOAA (Weather station: Lake Yellowstone, WY US)
#'
#' @name tempYNP
#' @docType data
#' @format A data frame with 7234 rows and 5 variables:
#' \describe{
#'   \item{Sation}{the official name of the weather station}
#'   \item{Location}{name of location}
#'   \item{Date}{date of daily summary}
#'   \item{Tmax}{maximum temperature}
#'   \item{Date}{minimum temperature}
#' }
#' @source \url{https://www.ncdc.noaa.gov/}
NULL
