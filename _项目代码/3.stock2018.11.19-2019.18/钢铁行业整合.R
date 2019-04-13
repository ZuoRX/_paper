library(xlsx)
library(tcltk)
library(progress)
library(plyr)
library(magrittr)  #管道符
library(highfrequency)

#=====================#
#---1.提取行业振幅数据#
#=====================#
steel<-read.csv("C:/Users/lenovo/Desktop/stock/steel industry/steel.csv")

# names(steel)<-c("time",   "open" ,   "high"  ,  "low" ,    "close" ,
#                 "rise","fluctuate","volumn" ,"money")
#time1<-format(steel$time[2],format='%m-%d-%Y')  #日期格式转化
#steel$time<-as.Date(steel$time) 
#1407,1408
a1<-steel[2:6]


#==========================#
#---2.提取个股收盘价数据---#
#==========================#
filenames<-list.files("C:/Users/lenovo/Desktop/stock/steel industry/data")

all<-as.data.frame(matrix(NA,nrow = 1900))

pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed",
                       total = 29, clear = FALSE, width= 60)

for(i in 1:29){
  
  pb$tick()
  url<-paste("C:/Users/lenovo/Desktop/stock/steel/data/",filenames[i],sep = "") 
  
  a2<-read.csv(url,1,encoding="UTF-8")
  b1<-a2[6]
 
  all<-cbind(all,b1)
}
all<-all[,-1]


write.csv(all,"C:/Users/lenovo/Desktop/stock/steel/all.csv")

#==========================#
#--------3.合并数据--------#
#==========================#

aa<-cbind(a1,all)
write.csv(aa,"C:/Users/lenovo/Desktop/stock/steel industry/all.csv")




#=============================#
#--------计算7日波动率--------#
#=============================#1018.11.19
steel<-read.csv("C:/Users/lenovo/Desktop/stock/R code/steel.csv")
names(steel)<-c("time",   "open" ,   "high"  ,  "low" ,    "close" ,
                "rise","fluctuate","volumn" ,"money")









