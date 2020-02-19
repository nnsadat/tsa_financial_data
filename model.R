#Take inputs
args <- commandArgs(trailingOnly=TRUE)


stock<- args[1]
start<-args[2]
end<-args[3]
p<-args[4]
d<-args[5]
q<-args[6]
return<-args[7]
fms<- args[8]
acfplot<-args[9]
pacfplot<-args[10]
qq<-args[11]
residualplot<-args[12]
box<- args[13]
forecast<-args[14]
sumofret<-args[15]
sumoff<-args[16]
fplot<-args[17]
mean<-args[18]
method<-args[19]
forecasterror<-args[20]
if(as.numeric(mean)==0){
  meanb=FALSE
}else{
  meanb=TRUE
}
if(as.numeric(method)==0){
  metb="CSS-ML"
}else if(as.numeric(method)==1){
  metb="CSS"
}else{
  metb="ML"
}

#Import libraries
library(TSA)
library(forecast)
library(quantmod)
library(DBI)
library(RMySQL)

#Dummies
# p<-"0"
# d<-"2"
# q<-"0"
# return<-"ret.png"
# fms<- "fms.txt"
# acfplot<-"acf.png"
# pacfplot<-"pacf.png"
# qq<-"qq.png"
# residualplot<-"res.png"
# box<- "box.txt"
# forecast<-"for.txt"
# sumofret<-"sumofret.txt"
# sumoff<-"sumoff.txt"
# fplot<-"fplot.png"
# stock<-"AAPL"
# start<-"2016-07-01"
# end<-"2016-07-31"
# mean<-"0"
# method<-"0"

#create Directory
# mainDir <- "C:/xampp/htdocs/cse400/"
# subDir <- "model"
# dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
# setwd(file.path(mainDir, subDir))
mydb=dbConnect(MySQL(),user='root',password='',dbname='stock_db',host='localhost')
rs = dbSendQuery(mydb, paste("SELECT row_names,Close FROM `hp` WHERE Symbol='",stock,"' AND row_names >= '",start,"' AND row_names <= '",end,"'", sep=""))
dat = fetch(rs, n=-1)
data<-dat[,2]


#extract data
#dat<-as.data.frame(getSymbols(stock,from=start,to=end,env=NULL))
#data=dat[,4]

#Calculate returns
returns<-100*diff(log(data))
summaryofreturns<-capture.output(summary(returns))
cat(paste("Summary of",stock,"stock returns", sep=" "), summaryofreturns, file=sumofret, sep="\n", append="FALSE")

#Plot returns
png(filename=return, width=500, height=400)
par(col.lab = '#00009B', col.axis = '#A52A2A',bg='white',fg='#006400')
plot(ts(returns),lwd=1.5, main=paste(stock,"Stock Return Values", sep=" ",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5),xlab="Time", ylab = "Returns");abline(h=0)
dev.off()

#Fitting model
fit<-try(Arima(returns, order=c(as.numeric(p),as.numeric(d),as.numeric(q)),include.mean = meanb,method = metb))
fitted<-capture.output(summary(fit))
cat("Fitted Model Summary ", fitted, file=fms, sep="\n", append="FALSE")

#Residual Analysis
residuals<-fit$residuals
#Plot
png(filename=residualplot, width=500, height=400)
par(col.lab = '#00009B', col.axis = '#A52A2A',bg='white',fg='#006400')
plot(residuals,lwd=1.5, main="Plot of Residuals of after Model Fitting",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
dev.off()
#ACF
png(filename=acfplot, width=500, height=400)
par(col.lab = '#00009B', col.axis = '#A52A2A',bg='white',fg='#006400')
acf(ts(residuals),lwd=1.5, main="ACF of Residuals",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
dev.off()
#PACF
png(filename=pacfplot, width=500, height=400)
par(col.lab = '#00009B', col.axis = '#A52A2A',bg='white',fg='#006400')
pacf(ts(residuals), lwd=1.5,main="PACF of Residuals",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
dev.off()
#QQPlot
png(filename=qq, width=500, height=400)
par(col.lab = '#00009B', col.axis = '#A52A2A',bg='white',fg='#006400')
qqnorm(residuals,lwd=1.5, main="Normal QQ plot of Residuals",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5);qqline(residuals)
dev.off()
#Ljung-Box Test
boxtesttry<-try(Box.test(residuals))
boxtest<-capture.output(boxtesttry)
cat("BOX TEST RESULT ON RESIDUALS: ", boxtest, file=box, sep="\n", append="FALSE")

#Forecast
ftry<-try(forecast.Arima(fit,h=10))
#Summary
summaryofforecast<-capture.output(ftry)
cat("Summary of Forecast: ", summaryofforecast, file=sumoff, sep="\n", append="FALSE")
#Plot
startf<-as.Date(end)+1
endf<-as.Date(startf)+30
mydb=dbConnect(MySQL(),user='root',password='',dbname='stock_db',host='localhost')
rs = dbSendQuery(mydb, paste("SELECT row_names,Close FROM `hp` WHERE Symbol='",stock,"' AND row_names > '",startf,"' AND row_names < '",endf,"'", sep=""))
dat = fetch(rs, n=-1)
data<-dat[,2]

future<-100*diff(log(data))

fe<-capture.output(accuracy(ftry,future[1:10]))
cat("Forecast errors: ", fe, file=forecasterror,sep="\n",append="FALSE")
true<-c(returns,future[1:10])

png(filename=fplot, width=500, height=400)
par(col.lab = '#00009B', col.axis = '#A52A2A',bg='white',fg='#006400')
plot.forecast(ftry,cex.lab=1.5, lwd=1.5,cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(true,lwd=2)
dev.off()



