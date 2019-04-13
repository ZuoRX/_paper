library(Rlibeemd)
library(gtools)
library(xts)
library(plyr)
library(dplyr) 
library(dtplyr)
library(vmd)
library(mice)  



#==================#
#===钢铁行业处理===#
#==================#

steel<-read.csv("c:/users/lenovo/desktop/stock/R code/steel.csv")
steel1<-steel[,c(1,5)]
names(steel1)<-c("time","steel_close")
#2018/10/26    2011/1/4
#一阶差分处理

d1_steel<- log(steel1$steel_close[-1900])-log(steel1$steel_close[-1])%>%as.data.frame()

d1_steel<-cbind(steel1$时间[-1900],d1_steel)
names(d1_steel)<-c("time","d1_close")
d1_steel$time<-as.Date(d1_steel$time)

filenames<-list.files("C:/Users/lenovo/Desktop/stock/steel industry/individual_industry/raw24/csv")


for (i in 1:24) {
  i=23
  url<-paste("C:/Users/lenovo/Desktop/stock/steel industry/individual_industry/raw24/csv/",filenames[i],sep = "") 
  
  a1<-read.csv(url)
  names(a1)<-c("time","open","high","low","close","volumn")
  
  #去掉，后的星期
  a1$time<-gsub(",|[\u4e00-\u9fa5]","",a1$time)%>%unlist()%>%as.vector()
  
  #降序排列
  a2<-arrange(a1,desc(time))
  
  # grep("2018-10-26",a2$time,fixed = TRUE)
  # grep("2011-01-04",a2$time,fixed = TRUE)
  a3<-a2[grep("2018-10-26",a2$time,fixed = TRUE):grep("2011-01-04",a2$time,fixed = TRUE),]
  
  a3$time<-as.Date(a3$time)
  
  a4<-join(d1_steel,a3,by="time")
  
 
  a5<-na.locf(a4,fromLast=TRUE)
  #把行业指数放在末尾
  a5$d1_c<-a5$d1_close
  
  raw<-a5[,3:7]
  raw_d1<-a5[,3:8]
  
  u1<-paste("C:/Users/lenovo/Desktop/stock/steel industry/individual_industry/processed_raw24/raw",i,".csv",sep = "")
  u2<-paste("C:/Users/lenovo/Desktop/stock/steel industry/individual_industry/processed_raw24/raw_d",i,".csv",sep = "")
  
  write.csv(raw,u1)
  write.csv(raw_d1,u2)
}


#==================#
#===银行行业处理===#
#==================#

#---行业指数处理---#
bank<-read.csv("c:/users/lenovo/desktop/stock/bank/industry-individual/bank.csv")
bank1<-bank[,c(1,5)]
names(bank1)<-c("time","bank_close")
bank1$time<-as.Date(bank1$time)
#2018/11/28    2008/11/21

aA<-read.csv("c:/users/lenovo/desktop/stock/bank/industry-individual/A.csv")
aA$time<-as.Date(aA$time)

names(aA)<-"time"

bank2<-join(aA,bank1,by="time")

#后补发补全缺失值
bank3<-na.locf(bank2,fromLast=TRUE)
#sum(is.na(bank3))

remove(aA,bank,bank1,bank2)


#---一阶差分处理---#
d1_bank<- log(bank3$bank_close[-2438])-log(bank3$bank_close[-1])%>%as.data.frame()

d1_bank<-cbind(bank3$time[-2438],d1_bank)
names(d1_bank)<-c("time","d1_close")
d1_bank$time<-as.Date(d1_bank$time)


#---读取个股数据---#

filenames<-list.files("c:/users/lenovo/desktop/stock/bank/industry-individual/individual/csv")


for (i in 1:14) {
  url<-paste("c:/users/lenovo/desktop/stock/bank/industry-individual/individual/csv/",filenames[i],sep = "") 
  
  a1<-read.csv(url)
  names(a1)<-c("time","open","high","low","close","volumn")
  
  #去掉，后的星期
  a1$time<-gsub(",|[\u4e00-\u9fa5]","",a1$time)%>%unlist()%>%as.vector()
  
  #降序排列
  a2<-arrange(a1,desc(time))
  
  #2018/11/28    2008/11/21
  # grep("2018-10-26",a2$time,fixed = TRUE)
  # grep("2011-01-04",a2$time,fixed = TRUE)
  a3<-a2[grep("2018-11-28",a2$time,fixed = TRUE):grep("2008-11-24",a2$time,fixed = TRUE),]
  
  a3$time<-as.Date(a3$time)
  
  a4<-join(d1_bank,a3,by="time")
 
  
  a5<-na.locf(a4,fromLast=TRUE)
  #把行业指数放在末尾
  a5$d1_c<-a5$d1_close
  
  raw<-a5[,3:7]
  raw_d1<-a5[,3:8]
  
  u1<-paste("C:/Users/lenovo/Desktop/stock/bank/industry-individual/processed_raw14/raw",i,".csv",sep = "")
  u2<-paste("C:/Users/lenovo/Desktop/stock/bank/industry-individual/processed_raw14/raw_d",i,".csv",sep = "")
  
  write.csv(raw,u1)
  write.csv(raw_d1,u2)
}




























