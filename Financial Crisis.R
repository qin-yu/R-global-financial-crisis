rm(list=ls(all=TRUE))
library("corrgram")
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



#data <- data[127:1134,]

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
pairs(X)

Y = data.frame(X)
corrgram(Y)


graphics.off()
