library(xlsx)
library(tcltk)
library(progress)
library(plyr)
library(magrittr)  
library(mice)      #处理缺失值
library(xts)
library(rJava)
library(xlsxjars)
#------------------------#
#第一步，读取生物行业数据
#------------------------#
biology<-read.csv("c:/users/lenovo/desktop/stock/biology/data/csv/biology.xls")
#2013-04-22----2018-10-26
b1<-biology[-1,]
b2<-b1[,-7]
names(b2)<-c("time","open","high","low","close","volumn")

#去掉，后的星期
b2$time<-gsub(",|[\u4e00-\u9fa5]","",b2$time)%>%unlist()%>%as.vector()

#降序排列
b3<-arrange(b2,desc(time))
b4<-b3[-1:-10,]

remove(b1,b2,b3,biology)
# b1$time<-b1$time%>%unlist()%>%as.vector()
# b2<-b1[1:grep("2010-12-31",b1$time,fixed = TRUE),]

#-----------------------#
#第二步，以A股时间为基准
#-----------------------#
aA<-read.csv("c:/users/lenovo/desktop/stock/biology/data/A.csv")
aA$time<-as.Date(aA$time)
aA1<-aA[1:grep("2013-04-22",aA$time,fixed = TRUE),]%>%unlist%>%
  as.character()%>%as.data.frame.Date()
names(aA1)<-"time"

b3<-join(aA1,b4,by="time")

remove(b4,b2,b1,aA)
#后补发补全缺失值
b<-na.locf(b3,fromLast=TRUE)
which(is.na(b))

remove(b3)

#-----------------------#
#第三步，收集个股数据
#-----------------------#
filenames<-list.files("c:/users/lenovo/desktop/stock/biology/data/24/csv")

all<-as.data.frame(matrix(NA,nrow = 1344))

for(i in 1:24){
  url<-paste("c:/users/lenovo/desktop/stock/biology/data/24/csv/",
             filenames[i],sep = "") 
  
  a2<-read.csv(url)
  #去掉，后的星期
  a2$时间<-gsub(",|[\u4e00-\u9fa5]","",a2$时间)%>%unlist()%>%as.vector()
  
  #降序排列
  a3<-arrange(a2,desc(时间))
  a4<-a3[-1:-10,]
  a5<-a4[1:grep("2013-04-22",a4$时间,fixed = TRUE),]
  #注意可能会不存在这个时间点
  names(a5)<-c("time","close")
  
  #按大盘补全数据
  a6<-join(aA1,a5,by="time")
  
  #后补法补全个股数据
  a7<-na.locf(a6,fromLast=TRUE)
  
  remove(a2,a3,a4,a5,a6)
  
  all<-cbind(all,a7$close)
}

all1<-all[,-1]

#-----------------------#
#第四步，合并raw_v_25数据
#-----------------------#

all2<-cbind(b,all1)

write.csv(all2,"c:/users/lenovo/desktop/stock/biology/all.csv")






