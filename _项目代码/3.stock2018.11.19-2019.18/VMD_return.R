
######批量读入文本 
library(Rlibeemd)
library(gtools)
library(xts)
library(plyr)
library(dplyr) 
library(dtplyr)
library(vmd)
library(magrittr)  #管道符
rm(list=ls())#清空环境
#快捷键Ctrl+L 清空console

#----------------------------#
#---1.分析效率值，选择个股---#
#----------------------------#
e<-read.csv("C:/Users/lenovo/Desktop/stock/steel/e.csv")

#统计每一列里面等于1的值
f<-function(x){
   sum(x==1)
 }
apply(e,2,f)

e1<-read.csv("C:/Users/lenovo/Desktop/stock/steel/e1.csv")#没啥区别
apply(e1,2,f)

#统计超效率里面的值  #超效率里面还是全部 标准化更有区分度
se<-read.csv("C:/Users/lenovo/Desktop/stock/steel/super_e1.csv")
f1<-function(x){
  sum(x>=1)
}
apply(se,2,f1)

#000825太钢不锈  V6
table(se$V6<=0.8)               #1
table(se$V6>0.8  & se$V6<=0.9)  #99
table(se$V6>0.9  & se$V6<=1  )  #1577
table(se$V6>1    & se$V6<=2  )  #177  #223
table(se$V6>2    & se$V6<=4 )   #3
table(se$V6==Inf)               #43


#--------------------#
#---2.提取效率值---#
#--------------------#
a <- read.csv("C:/Users/lenovo/Desktop/stock/steel/super_e1.csv")
#000717
a1<-a$V6%>%as.data.frame()
names(a1)<-"e"

#将Inf转换成4
a1$e[a1$e==Inf]<-4

e1<-a1%>%unlist() %>%as.numeric()

#--------------------#
#---3.提取趋势值-----#
#--------------------#

v = vmd(e1,alpha=2000,tau=0,DC=FALSE,init=0,tol=1e-6,K=7,orderModes=TRUE)

modes = as.data.frame(v)

write.csv(modes,file="C:/Users/lenovo/Desktop/stock/steel/vmd.csv")


#--------------------#
#------4.合并值------#
#--------------------#
e<-a1
vmd<-read.csv("C:/Users/lenovo/Desktop/stock/steel/vmd.csv")
vmd<-vmd[,c(-1,-2)]
all1<-read.csv("C:/Users/lenovo/Desktop/stock/steel/000825_1901.csv")
time1<-all1[,2]%>%as.data.frame()

time<-time1[-1901,]%>%as.data.frame()


#这里面是1900条数据
e_vmd<-data.frame(time,e,vmd)
names(e_vmd)[1]<-"time"

#这里是1791条数据
all<-read.csv("C:/Users/lenovo/Desktop/stock/steel/000825_1791.csv")
names(all)

gru<-join(all,e_vmd,by="time")

write.csv(gru,"C:/Users/lenovo/Desktop/stock/steel/gru.csv")













#



a <- read.csv("C:/Users/lenovo/Desktop/stock/steel/29data/601003.csv",header=F)
#000717
a1<-a$V6%>%as.data.frame()
names(a1)<-"e"

e1<-a1%>%unlist() %>%as.numeric()

v = vmd(e1,alpha=2000,tau=0,DC=FALSE,init=0,tol=1e-6,K=7,orderModes=TRUE)

modes = as.data.frame(v)

v1<-cbind(a,modes[,c(-1,-2)])
write.csv(v1,file="C:/Users/lenovo/Desktop/stock/steel/29data/601003vmd.csv")




































# minmaxscale<-function(a){
#   center <- sweep(a, 2, apply(a, 2, min),'-')
#   R <- apply(a, 2, max) - apply(a,2,min)
#   #算出极差
#   return(sweep(center, 2, R, "/"))
# }
# v <-minmaxscale(as.data.frame(v))

#List of Results
l = v$getResult()
names(l)

#To Data Frame
modes = as.data.frame(v)

write.csv(modes,file="C:/Users/lenovo/Desktop/stock/steel1/modes.csv")

pdf(file="C:/Users/lenovo/Desktop/stock/yiqi_fan/modes.pdf")
plot(v)
plot(v,facet='bymode',scales='free')
plot(v,facet='byclass',scales='free')
dev.off()
