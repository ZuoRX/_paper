library(Benchmarking)
library(rJava)
library(xlsxjars)
library(xlsx)
library(tcltk)
library(progress)
library(plyr)
library(magrittr)  #管道符
library(xts)
library(mice)      #处理缺失值



#------------------#
#第一步，读取股本表
#------------------#

a<-read.csv("c:/users/lenovo/desktop/stock/steel/data2/csv/000825_diy_report.csv",1,
          encoding = "GBK")
a1<-t(a)%>%as.data.frame()
a2<-a1[-1,]
a4<-a2[1:32,]

names(a4)<-c("time","stock")


#---------------------#
#第二步，读取每日行情表
#---------------------#

#最少是10年9月30开始有数据，从 10年12.31开始
b<-read.table("c:/users/lenovo/desktop/stock/steel/data1/000825太钢不锈.xls",1,
              encoding = "GBK")
names(b)<-c("time","open","high","low","close","volumn")

#去掉，后的星期
b$time<-gsub(",|[\u4e00-\u9fa5]","",b$time)%>%unlist()%>%as.vector()

#降序排列
b1<-arrange(b,desc(time))
b1$time<-b1$time%>%unlist()%>%as.vector()

b2<-b1[1:grep("2010-12-31",b1$time,fixed = TRUE),]#%>%as.data.frame()
# b2<-b1[1:1744,]#%>%as.data.frame()
# 
# b2$time[1744]<-"2010-12-31"
#write.csv(b2,"C:/Users/lenovo/Desktop/stock/steel/000825_1791.csv")
#---------------------#
#第三步，补全停牌时间
#---------------------#
aA<-read.table("c:/users/lenovo/desktop/stock/steel/A.xls",1,
               encoding = "GBK")
names(aA)<-c("time","open","high","low","close")

b3<-join(aA,b2,by="time")
b4<-b3[,c(-2,-3,-4,-5)]

#---------------------#
#第四步，补全停牌数据
#---------------------#

# 用当前列中，NA的后值来填充           <-----------------------超赞！
b5<-na.locf(b4,fromLast=TRUE)
#write.csv(b5,"C:/Users/lenovo/Desktop/stock/steel/000825_1901.csv")

#---------------------------#
#第五步，补充完股本数据
#---------------------------#

#1.放一起 
a5<-data.frame(a4,open=NA,high=NA,low=NA,close=NA,volumn=NA)
a5$time<-as.Date(a5$time)

b5$time<-as.Date(b5$time)
b6<-data.frame(b5,stock=NA)
all<-rbind(a5,b6)
#2. 排序  
all1<-arrange(all,desc(time),open)
#3.补充股本缺失值 
s1<-all1[,c(1,2)]
s2<-na.locf(s1,fromLast=TRUE)
#4.再合并，最后删除其他变量缺失值
all1$stock<-s2$stock
all2<-na.omit(all1)

#---------------------------#
#第六步，补充净流入资金
#---------------------------#
all2$stock<-all2$stock%>%as.character()%>%as.numeric()#因子变量处理方法
all2$t_value<-all2$stock*all2$close
net_in<-all2$t_value[-1901]-all2$t_value[-1]
#昨日收盘价
l_close<-all2$close[-1]
all3<-all2[-1901,]
all3$net_in<-net_in
all3$l_close<-l_close

write.csv(all3,"c:/users/lenovo/desktop/stock/steel/data/600022.csv",row.names = F)






























