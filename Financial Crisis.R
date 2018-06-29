rm(list=ls(all=TRUE)) 
library("corrgram")
#setwd("H:/...")

###################################################################################
###### Reading and preparing data from csv file downloaded from Wharton:

dataraw<-read.csv("data/JPM WFC BAC C GS MS INTC SI IBM AAPL COF CSIQ FE 01Jan2007 - 31Dec2010.csv")
names(dataraw) 
ids<-unique(dataraw[,1])
nAssets<-length(ids)
nObs<-nrow(dataraw)/nAssets
data<-matrix(NA,nObs,nAssets+1)
data[,1]<-dataraw[dataraw[,1]==ids[1],2]  # array[condition, no.]
for (i in 1:nAssets){
  data[,i+1]<-dataraw[dataraw[,1]==ids[i],3]
}
time<-as.Date(as.character(data[,1]),"%Y%m%d")
ticker<-c("IBM", "AAPL", "FE", "WFC", "JPM", "INTC", "BAC", "MS", "C", "COF", "GS", "SI", "CSIQ")
colnames(data)[2:(nAssets+1)]<-ticker

###################################################################################
###### Plot the stock price dynamics:

par(mfrow=c(3,2))
for (i in 1:nAssets){
  plot(time,data[,(i+1)],ylab=ticker[i],xlab="", type = "l", col = i)
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
plot(time[2:length(time)], logprices[,1],type="l",ylim=c(min(logprices),max(logprices)),ylab="")
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
