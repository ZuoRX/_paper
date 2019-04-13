rm(list=ls())
library(bitops)
library(RCurl)
library(XML)
library(xlsx)
library(dplyr)
library(readr)
library(stringi)
library(rvest)
library(tcltk)
library(progress)
cat("\014") 

#----------------————------------------------#
#------读取所有选秀球员的数据，并整合--------#
#----------------————------------------------#

filenames<-list.files("C:/Users/lenovo/Desktop/BALL/team-NBA/all")
dir<-paste("C:/Users/lenovo/Desktop/BALL/team-NBA/all/",filenames,sep="")
dir<-dir[-46]

for (i in 1:45){
  b=dir[i]
  new_1<-read.csv(dir[i],1,encoding = "GBK")
  write.table(new_1,"C:/Users/lenovo/Desktop/BALL/team-NBA/all/all.csv",append = TRUE,
              sep=",",col.names = F,row.names =F)
}
#----------------#
#对照组  30只队伍
#----------------#           #重构一个30只队伍的数据
filenames<-list.files("C:/Users/lenovo/Desktop/BALL/team-NBA16/all/control")
dir<-paste("C:/Users/lenovo/Desktop/BALL/team-NBA16/all/",filenames,sep="")

for (i in 1:30){
  b=dir[i]
  new_1<-read.csv(dir[i],1,encoding = "GBK")
  write.table(new_1,"C:/Users/lenovo/Desktop/BALL/team-NBA16/all/control/all_control.csv",append = TRUE,
              sep=",",col.names = F,row.names =F)
}

#注意修改变量名称
# a<-read.csv("C:/Users/lenovo/Desktop/BALL/team-NBA/all/Yongshi_f14.csv",1)
# names(a)

#----------------————------------------------#
#------多30只队伍的总体数据进行处理----------#
#----------------————------------------------#
rm(list=ls())
data<-read.table("C:/Users/lenovo/Desktop/BALL/team-NBA16/all/control/all_control.csv",
                 sep=",",header=T )

data$pk<-gsub(" ","",data$pk)
#七六人98-78湖人  转换成 湖人78-98七六人    通过这样的方式匹配对手的数据
#-----------先拆分两队比分----------#
wl<-gsub("\\d+-\\d+[\u4e00-\u9fa5]+","",data$pk)
wr<-gsub("[\u4e00-\u9fa5]+\\d+-\\d+","",data$pk)
tscore<-gsub("[\u4e00-\u9fa5]","",data$pk)
sl<-gsub("-\\d+","",tscore)
sr<-gsub("\\d+-","",tscore)
#-------再重组-----#
pk1<-list(NULL)  #构建一个空值的list
length(pk1)<-2460
for(i in 1:2460){
  pk1[[i]]<-paste(wr[i],sr[i],"-",sl[i],wl[i],sep = "")
}
pk1<-unlist(pk1)
data$pk1<-pk1

# data<-data[-1817,]
# data<-data[-2145,]  #字符串“循环”处理，一定要注意处理“空格”


data_copy<-data
dat<-data.frame()

pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed",
                       total = 2460, clear = FALSE, width= 60)
for(i in 1 :2460)try({
  pb$tick()
  data1<-cbind(data[i,],data_copy[which(data_copy$pk==data[i,]$pk1),])
  dat<-rbind(dat,data1)
  Sys.sleep(0.01)
 },
 silent = T)

write.table(data1,"C:/Users/lenovo/Desktop/BALL/team-NBA16/all/control/all_f1.csv",append = TRUE,
            sep=",",col.names = F,row.names =F)





