#Take inputs
args <- commandArgs(trailingOnly=TRUE)


stock<- args[1]
start<-args[2]
end<-args[3]
d<-args[4]
return<-args[5]
sumofret<-args[6]
acfplot<-args[7]
pacfplot<-args[8]
qq<- args[9]
eacftest<-args[10]
adftest<-args[11]
hist<-args[12]

#Import libraries
library(TSA)
library(forecast)
library(quantmod)
library(DBI)
library(RMySQL)

#Dummies
# stock<- "AAPL"
# start<-"1980-07-01"
# end<-"2016-07-06"
# return<-"ret.png"
# sumofret<-"sum.txt"
# acfplot<-"acf.png"
# pacfplot<-"pacf.png"
# qq<- "qq.png"
# eacftest<-"eacf.txt"
# adftest<-"adf.txt"
# hist<-"hist.png"

#create Directory
# mainDir <- "C:/xampp/htdocs/cse400/"
# subDir <- "analysis"
# dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
# setwd(file.path(mainDir, subDir))
mydb=dbConnect(MySQL(),user='root',password='',dbname='stock_db',host='localhost')
rs = dbSendQuery(mydb, paste("SELECT row_names,Close FROM `hp` WHERE Symbol='",stock,"' AND row_names >= '",start,"' AND row_names <= '",end,"'", sep=""))
# rs = dbSendQuery(mydb, "SELECT * FROM `hp` WHERE Symbol='YHOO' AND row_names >='2016-06-01' AND row_names <= '2016-06-31' ")
# dat = fetch(rs, n=-1)
# rs = dbSendQuery(mydb, "SELECT row_names,Close FROM `hp` WHERE Symbol='YHOO' AND row_names > '2016-01-01'")
dat = fetch(rs, n=-1)
data<-dat[,2]
head(dat)
tail(dat)
#extract data
#dat<-as.data.frame(getSymbols(stock,from=start,to=end,env=NULL))
#data=dat[,4]

#Calculate returns
returns<-100*diff(log(data))
if(as.numeric(d)>0){
returns<-diff(data, differences = as.numeric(d))
}
summaryofreturns<-capture.output(summary(returns))
cat(paste("Summary of",stock,"stock returns", sep=" "), summaryofreturns, file=sumofret, sep="\n", append="FALSE")

#Plot returns
png(filename=return, width=500, height=400)
par(col.lab = '#00009B', col.axis = '#A52A2A',bg='white',fg='#006400')
plot(ts(returns), lwd=1.5, main=paste(stock,"Stock Return Values", sep=" "),xlab="Time", ylab = "Returns",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5);abline(h=0)
dev.off()

#ACF
png(filename=acfplot, width=500, height=400)
par(col.lab = '#00009B', col.axis = '#A52A2A',bg='white',fg='#006400')
acf(ts(returns), main=paste("ACF of Returns of",stock,sep=" "),lwd=1.5,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
dev.off()

#PACF
png(filename=pacfplot, width=500, height=400)
par(col.lab = '#00009B', col.axis = '#A52A2A',bg='white',fg='#006400')
pacf(ts(returns), lwd=1.5,main=paste("PACF of Returns of",stock,sep=" "),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
dev.off()

#EACF
tryit<-try(eacf(returns))
if(inherits(tryit,"try-error")){
  cat("EACF of Returns", "The execution returned errors.", file=eacftest, sep="\n", append="FALSE")
}else{
eacftry<-eacf(returns)
eacfresult<-capture.output(eacftry)
cat("EACF of Returns", eacfresult, file=eacftest, sep="\n", append="FALSE")
}


#QQPlot
png(filename=qq, width=500, height=400)
par(col.lab = '#00009B', col.axis = '#A52A2A',bg='white',fg='#006400')
qqnorm(returns,lwd=1.5, main="Normal QQ plot of Resturns",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5);qqline(returns)
dev.off()

#Histogramplot
png(filename=hist, width=500, height=400)
par(col.lab = '#00009B', col.axis = '#A52A2A',bg='white',fg='#006400')
hist(returns,col="lightgreen",lwd=1.5,prob=TRUE,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
curve(dnorm(x,mean=mean(returns),sd=sd(returns)),add=TRUE,lwd=2,col="blue")
dev.off()

#ADF Test
adftry<-try(adf.test(returns))
adfresults<-capture.output(adftry)
cat("Augmented Dickey Fuller Test on Returns ", adfresults, file=adftest, sep="\n", append="FALSE")
