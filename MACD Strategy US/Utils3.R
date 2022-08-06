
library('scales')
Sys.setlocale("LC_TIME","English")

# ----------------------------------------------------------------------------------------------------------------------
# compute EMA
ema = function(price,n) {
  
  # initialize price with SMA
  price[n] = mean(price[1:n])
  
  # loop through all time steps, and compute step-wise ema
  for (i in (n+1):length(price)) {
    price[i] = price[i]*2/(n + 1) + price[i-1]*(1-2/(n + 1))
  }
  
  # set initial prices to NaN since you cannot compute it
  price[1:(n-1)] = NA
  
  return(price)
  
}

# ----------------------------------------------------------------------------------------------------------------------
# compute macd
macd = function(price,ma1,ma2,ma_smooth) {
  
  # compute macd line
  MACD   = ema(price, ma1) - ema(price, ma2)
  
  # compute signal line
  signal = c(rep(NA, ma2), ema(MACD[(ma2+1):length(price)], ma_smooth))
  
  return(list(MACD=MACD, signal=signal))
  
}

# ----------------------------------------------------------------------------------------------------------------------
# compute location of cross over
crossover = function(x1,x2) {
  
  # find max time steps
  l = max(c(length(x1),length(x2)))
  
  # duplicate values if only singular numeric value is provided
  if (length(x1) == 1) x1 = rep(x1,l)
  if (length(x2) == 1) x2 = rep(x2,l)
  
  # find which indices are true, if NA then set as false. x1 was more, now is less
  #cross_ind = c(FALSE,x1[1:(l-1)] >= x2[1:(l-1)] & x1[2:l] <= x2[2:l])
  cross_ind = c(FALSE,x1[1:(l-1)] > x2[1:(l-1)] & x1[2:l] < x2[2:l])
  cross_ind[is.na(cross_ind)] = FALSE
  
  return(cross_ind)
  
}

# ----------------------------------------------------------------------------------------------------------------------
# compute local extrema
local_extrema = function(x,max=TRUE) {
  
  # find local maxima and minima
  if (max) {
    return(which(diff(sign(diff(x)))==-2) + 1) # add 1 to shift original index, another to include next point
  } else {
    return(which(diff(sign(diff(x)))==+2) + 1)
  }
  
}

# ----------------------------------------------------------------------------------------------------------------------
# smoothing data using past data and overall data (naive)
smooth = function(x,s) {
  
  index = 1:length(x)
  temp  = predict(loess(x ~ index,span=s))
  return(temp)
  
}

# ----------------------------------------------------------------------------------------------------------------------
# Identify if current price action is a bullish candle stick pattern
# input is vector of minimum 3 rows of a data frame with open/high/low/close
bullish_candle = function(x) {

  # define variable for the number of rows of x
  n = nrow(x)
  
  # Bullish Pin
  lower_tail_length = abs(min(c(x$close[n],x$open[n])) - x$low[n])
  candle_length     = abs(x$high[n] - x$low[n])
  tail_candle_ratio = lower_tail_length/candle_length
  tail_candle_ratio = ifelse(is.nan(tail_candle_ratio),0,tail_candle_ratio)
  
  if (tail_candle_ratio >= (2/3)) return("Pin")
  
  # Tweezer Bottom
  tweezer_open  = x$open[n-1]
  tweezer_close = x$close[n]
  tweezer_low   = min(c(x$low[n-1],x$low[n]))
  tweezer_high  = max(c(x$high[n-1],x$high[n]))
  
  lower_tail_length = abs(min(c(tweezer_open,tweezer_close)) - tweezer_low)
  candle_length     = abs(tweezer_high - tweezer_low)
  tail_candle_ratio = lower_tail_length/candle_length
  tail_candle_ratio = ifelse(is.nan(tail_candle_ratio),0,tail_candle_ratio)
  
  first_bear  = x$open[n-1] > x$close[n-1]
  second_bull = x$open[n] < x$close[n]
  body_match  = abs(abs(x$open[n-1] - x$close[n-1]) - abs(x$open[n] - x$close[n])) / abs(x$open[n] - x$close[n]) < 0.1

  if (tail_candle_ratio >= (2/3) & first_bear & second_bull & body_match) return("Tweezer")
  
  # One White Soldier
  first_bear  = x$open[n-1] > x$close[n-1]
  second_bull = x$open[n] < x$close[n]
  
  open_higher  = x$open[n] > x$close[n-1]
  close_higher = x$close[n] > x$open[n-1]
  higher_high  = x$low[n] > x$low[n-1]
  higher_low   = x$high[n] > x$high[n-1]
  
  if(first_bear & second_bull & open_higher & close_higher & higher_high & higher_low) return("WhiteSoldier")
  
  # Morning Star
  bear_start        = x$close[n-2] < x$open[n-2]
  bear_candle_ratio = abs(x$open[n-2] - x$close[n-2]) / abs(x$open[n-1] - x$close[n-1])
  
  gap_down = x$open[n-1] < x$close[n-2]
  gap_up   = x$close[n-1] < x$open[n]
  
  bull_end           = x$close[n] > x$open[n]
  bull_closed_higher = x$close[n] > (abs(x$open[n-2] + x$close[n-2]) / 2)
  
  if (bear_start & bear_candle_ratio > 2 & gap_down & gap_up & bull_end & bull_closed_higher) return("MorningStar")
  
  # Bullish Engulfing
  first_bear  = x$open[n-1] > x$close[n-1]
  second_bull = x$open[n] < x$close[n]
  
  close_higher = x$close[n] > x$open[n-1]
  open_lower   = x$open[n] < x$close[n-1]
  
  if (first_bear & second_bull & close_higher & open_lower) return("Engulf")
  
  # No bullish pattern at all here
  return("")
  
}

# ----------------------------------------------------------------------------------------------------------------------
plot_stock = function(data,sub="volume",emas=list(),macd=c(),
                      entry=c(),exit=c(),entry.price=NA,exit.price=NA,
                      save=FALSE,savename="") {
  
  if (save) png(paste0(savename,".png"), width = 1000, height = 1000)

  # set up plot area
  layout(matrix(1:2,2,1),heights=c(2.5,1))
  
  # # # Main Plot
  # set up plot range
  xrange = range(data$Date)
  yrange = range(c(data$High,data$Low,range(sapply(emas,function(x) range(x)))),na.rm=TRUE)

  # initialize plot with margin sizes
  par(mar=c(1,4,1,1))
  plot(-999,-999,xlim=xrange,ylim=yrange,xlab="",ylab="Price (USD)",xaxt="n",yaxt="n")

  # label axis
  axis(2,at=round(seq(yrange[1]*0.9,yrange[2]*1.1,by=10^(floor(log10(yrange[2]))-1)),-(floor(log10(yrange[2]))-1)),
       labels=round(seq(yrange[1]*0.9,yrange[2]*1.1,by=10^(floor(log10(yrange[2]))-1)),-(floor(log10(yrange[2]))-1)),las=2)
  
  # moving average
  if (length(emas) > 0) {
    cols = c("purple","blue","aquamarine","orange","red","black")
    for (i in 1:length(emas)) {
      lines(data$Date,emas[[i]],col=cols[i],lwd=2)
    }
  }
  
  # add entries and exits
  if (length(entry) > 0) {
    points(entry,data$Low[data$Date==entry]*0.995,pch=24,cex=2,col="blue",bg="blue")
    abline(h=entry.price,col=alpha("goldenrod2",0.5))
    mtext(as.character(round(entry.price,2)),at=entry.price,side=2,las=1,col="goldenrod2",cex=1.5)
  }
  if (length(exit) > 0) {
    points(exit,data$High[data$Date==exit]*1.005,pch=25,cex=2,col="blue",bg="blue")
    abline(h=exit.price,col=alpha("deeppink",0.5))
    mtext(as.character(round(exit.price,2)),at=exit.price,side=2,las=1,col="deeppink",cex=1.5)
  }
  
  # construct candlesticks
  colors = ifelse(data$Close >= data$Open,"forestgreen","tomato")
  segments(x0  = data$Date, y0 = data$Low
          ,x1  = data$Date, y1 = data$High
          ,col = colors, lwd=1
  )
  rect(xleft   = data$Date-0.3
      ,ybottom = ifelse(data$Close >= data$Open,data$Open,data$Close)
      ,xright  = data$Date+0.3
      ,ytop    = ifelse(data$Close >= data$Open,data$Close,data$Open)
      ,col     = colors,border=colors
  )

  # initialize plot with margin sizes
  par(mar=c(6,4,0,1))
  
  # # # Volume Plot
  if (sub == "volume") {
    
    # set up plot range
    yrange2 = range(c(0,as.numeric(data$Volume)/1e6),na.rm=TRUE)
    
    plot(-999,-999,xlim=xrange,ylim=yrange2,xlab="",ylab="Vol (M)",xaxt="n",las=1)
    
    # label axis
    axis(1,at=seq(xrange[1],xrange[2],by=25),
         labels=seq(xrange[1],xrange[2],by=25),las=2)
    
    # add volume
    rect(xleft   = data$Date-0.5
         ,ybottom = 0
         ,xright  = data$Date+0.5
         ,ytop    = as.numeric(data$Volume)/1e6
         ,col     = colors,border=NA
    )
    
  } 
  
  # # # MACD Plot
  else if (sub == "macd") {
    
    # set up plot range
    yrange2 = range(c(macd$MACD,macd$signal),na.rm=TRUE)
    
    plot(-999,-999,xlim=xrange,ylim=yrange2,xlab="",ylab="MACD Value",xaxt="n",las=1)
    
    # label axis
    axis(1,at=seq(xrange[1],xrange[2],by=25),
         labels=seq(xrange[1],xrange[2],by=25),las=2)
    
    # Add MACD
    abline(h=0,col="grey")
    lines(data$Date,macd$MACD,col="deepskyblue")
    lines(data$Date,macd$signal,col="orange")
    
  }
  
  if (save) dev.off()

}
