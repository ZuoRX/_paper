library(zoo)
library(ggplot2)

steel<-read.csv("c:/users/lenovo/desktop/stock/R code/steel.csv")



a<-data.frame(steel$时间,steel$收盘)
names(a)<-c("time","close price")


plot(zoo(a$`close price`,as.Date(a$time)), ylab="Close Price",xlab="Date",
     col = "red")
box(which="figure")

bank<-read.csv("c:/users/lenovo/desktop/stock/bank/bank.csv")

a<-data.frame(bank$time,bank$close)

plot(zoo(a$bank.close,as.Date(a$bank.time)), ylab="bank industry closing price",xlab="Date",
     col = "red")

box(which="figure")


