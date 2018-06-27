# Wharton data in R

rm(list=ls(all=TRUE)) 

#setwd("H:/...")

dataraw<-read.csv("JPM WFC BAC C GS MS INTC SI IBM AAPL COF CSIQ FE 01Jan2007 - 31Dec2010.csv")
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

par(mfrow=c(3,2))
for (i in 1:nAssets){
  plot(time,data[,(i+1)],ylab=ticker[i],xlab="", type = "l", col = i)
}

# This is an alternative method to get data online:
#install.packages("tseries")
library("tseries")
#?get.hist.quote
tseries.ibm <- get.hist.quote(instrument = "ibm",  start = "1998-01-01", quote = "Adj")
#plot(tseries.ibm)
tseries.c <- get.hist.quote(instrument = "c",  start = "1998-01-01", quote = "Adj")
#plot(tseries.c)


