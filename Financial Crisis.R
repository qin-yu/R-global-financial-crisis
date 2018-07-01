rm(list=ls(all=TRUE))
library("corrgram")
library("colorspace")

### Loss Operator Function:
lo.fn <- function(x,weights,value, linear=FALSE){
  # parameter x should be a matrix or vector of N returns (risk-factors)
  # parameter lo.weights should be a vector of N weights 
  # parameter lo.value should be a scalar representing a portfolio value
  if ((!(is.vector(x))) & (!(is.matrix(x)))) 
    stop("x must be vector or matrix with rows corresponding to risk 
         factor return observations")
  N <- length(weights)
  T <- 1
  if (is.matrix(x)) T <- dim(x)[1]
  # Weights in a matrix
  weight.mat <- matrix(weights,nrow=T,ncol=N,byrow=TRUE)
  # Starting values for each individual risk factor in matrix form
  tmp.mat <- value * weight.mat
  
  # On the parameter 'linear': The default is nonlinear which means the 
  # function which calculated the return factor (e.g. mk.returns() used
  # a default log() (or some other nonlinear) transform.  Hence using 
  # exp(x) will reconvert the log() so the summand will reflect actual 
  # changes in value  
  if (linear) { summand <- x*tmp.mat } else { summand <- (exp(x)-1)*tmp.mat }
  
  # We return a vector where each row is the change in value summed across 
  # each of the risk factors on a single day.
  # By taking the negative we convert to losses
  loss <- -rowSums(summand)
  return(loss)
}

#setwd("H:/...")

###################################################################################
###### Reading and preparing data from csv file downloaded from Wharton:

dataraw <- read.csv("data/JPM WFC BAC GS USB COF AXP INTC SI IBM MSFT BHGE FE XOM AEIS.csv")
dataadd <- read.csv("data/MA.csv")
dataraw <- rbind(dataraw, dataadd)
names(dataraw) 
ids<-unique(dataraw[,1])
tks<-unique(dataraw[,3])
nAssets<-length(ids)
nObs<-nrow(dataraw)/nAssets

### check which stock is wrong:
for (tik in ids) {
  print(c(tik, length(dataraw[dataraw[,1] == tik, 2])))
}

data<-matrix(NA,nObs,nAssets+1)
data[,1]<-dataraw[dataraw[,1]==ids[1],2]  # array[condition, no.]
for (i in 1:nAssets){
  data[,i+1]<-dataraw[dataraw[,1]==ids[i],4]
}
time<-as.Date(as.character(data[,1]),"%Y%m%d")
ticker<-as.character(tks)
colnames(data)[2:(nAssets+1)]<-ticker

rearranged_data <- matrix(NA,nObs,nAssets+1)
rearranged_data[,  1] <- data[, 1]
rearranged_data[,  9:12 + 1] <- data[, c( 1,  3,  8, 15) + 1]
rearranged_data[, 13:16 + 1] <- data[, c( 2,  4, 11, 13) + 1]
rearranged_data[,  1: 8 + 1] <- data[, c( 5,  6,  7,  9, 10, 12, 14, 16) + 1]
data <- rearranged_data

rearranged_tckr <- vector('numeric', nAssets)
rearranged_tckr[ 9:12] <- ticker[c( 1,  3,  8, 15)]
rearranged_tckr[13:16] <- ticker[c( 2,  4, 11, 13)]
rearranged_tckr[ 1: 8] <- ticker[c( 5,  6,  7,  9, 10, 12, 14, 16)]
ticker <- rearranged_tckr
colnames(data)[2:(nAssets+1)]<-ticker


###################################################################################
###### Plot the stock price dynamics:

#par(mfrow=c(4,4))
#for (i in 1:nAssets){
#  plot(time[127:1134],data[127:1134,(i+1)],ylab=ticker[i],xlab="", type = "l", col = i)
#  title(paste("Fig.2.", i, " Stock Price of ", ticker[i], sep = ""))
#}

average_data <- numeric(nrow(data))
par(mfrow=c(3,3))
for (i in 1:8){
  plot(time[127:1134],data[127:1134,(i+1)],ylab=ticker[i],xlab="", type = "l", col = i)
  title(paste("Fig.2.", i, " Stock Price of ", ticker[i], sep = ""))
  average_data <- average_data + data[,i + 1]
}
average_data <- average_data/8
plot(time[127:1134], average_data[127:1134], ylab = "Avg", xlab = "", type = "l", col = 1)
title("Fig.2.x Average Stock Price")

par(mfrow=c(2,2))
for (i in 9:12){
  plot(time[127:1134],data[127:1134,(i+1)],ylab=ticker[i],xlab="", type = "l", col = i)
  title(paste("Fig.3.", i-8, " Stock Price of ", ticker[i], sep = ""))
}

par(mfrow=c(2,2))
for (i in 13:16){
  plot(time[127:1134],data[127:1134,(i+1)],ylab=ticker[i],xlab="", type = "l", col = i)
  title(paste("Fig.4.", i-12, " Stock Price of ", ticker[i], sep = ""))
}


###################################################################################
###### Calculate and plot the log return:

prices <- data[, ticker]
logprices <- log(prices)
X <- diff(logprices)
T <- nrow(X)  # 1007
prices <- prices[2:(T+1),]  # abandon the first day for alignment
logprices <- logprices[2:(T+1),]

dev.new()
plot(time[2:length(time)], logprices[,1], type="l", ylim=c(min(logprices), max(logprices)), xlab = "", ylab="")
title("Fig.1 Overview: Log Prices of All 16 Stocks")
for (i in 2:ncol(logprices)) {
  lines(time[2:length(time)], logprices[,i],type="l",col=i)
}

graphics.off()


###################################################################################
###### Plot pair-wise scatter plots for stock return:

dev.new()
#pairs(X)

Y = data.frame(X)
corrgram(Y, lower.panel = panel.shade, upper.panel = panel.pie)

graphics.off()


###################################################################################
###### Form a portfolio:

### Parameters for Portfolio
p <- 0.95

long_end <- 1134  # ends on 2010-12-31
short_end <- 882  # ends on 2009-12-31
prev_start <- 127  # start on 2007-01-01
next_start <- 883  # start on 2010-01-01
this_start <- next_start
this_end <- long_end

time <- time[this_start:this_end]
T <- length(time)
prices <- prices[(this_start - 2):(length(time) - 1),]  
# end - 1 because prices lost first row, 
# start - 2 to get the first diff between years
logprices <- log(prices)
X <- diff(logprices)  # nrow(X) == nrow(logprices) - 1 == 1008 is true.
#Y <- diff(log(prices[prices[(next_start - 2):(long_end - next_start + 1 - 1),]]))

individual_investment <- numeric(ncol(X))
individual_investment[1:length(individual_investment)] <- 1000
pf.value <- sum(individual_investment)
pf.weights <- matrix(rep(1/ncol(X), ncol(X)), nrow = 1)

### Losses of selected stocks over full sample period
loss <- lo.fn(X,pf.weights,pf.value)

### Mean and variance adjusted for the R-implied Bessel correction
mu.hat <- colMeans(X)
sigma.hat <- var(X)*(T-1)/T 

### ES and VaR
meanloss <- -sum(pf.weights*mu.hat)*pf.value
varloss <- pf.value^2 *(pf.weights %*% sigma.hat %*% t(pf.weights))
VaR.normal <- meanloss + sqrt(varloss) * qnorm(p)
ES.normal <- meanloss + sqrt(varloss) * dnorm(qnorm(p))/(1-p)

### Historical simulation
hs.data <- lo.fn(X,pf.weights,pf.value)

#write.csv(hs.data, file = "Data/hs.data.0709", row.names = FALSE)
#write.csv(hs.data, file = "Data/hs.data.10", row.names = FALSE)
hs.data.0709 <- as.matrix(read.csv(file = "Data/hs.data.0709")) 
hs.data.10 <- as.matrix(read.csv(file = "Data/hs.data.10"))
dev.new()
qqplot(hs.data.0709, hs.data.10, xlab = "historical data 2007-2009", ylab = "historical data 2010")
title("Fig.7 QQ Test (2007-09 vs 2010)")

qqnorm(hs.data)
VaR.hs <- quantile(hs.data,p)
ES.hs <- mean(hs.data[hs.data > VaR.hs])

### Plot results
hist(hs.data, nclass=100, prob=TRUE, xlab="Loss Distribution",
     col=diverge_hsv(90), xlim = range(-2500:2500),
     main = paste("Historical simulation", time[1], "to", time[length(time)]))
abline(v=c(VaR.normal,ES.normal),col=1,lty=2);
abline(v=c(VaR.hs,ES.hs),col=2,lty=2)
legendnames <- c(paste(p,"normal VaR and ES"),paste(p,"HS VaR and ES"))
legend("topleft", legend = legendnames, col=1:2, pch="-") 

graphics.off()


