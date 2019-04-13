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
#------读取16年所有选秀球员的数据，并整合----#
#----------------————------------------------#

filenames<-list.files("C:/Users/lenovo/Desktop/BALL/team-NBA/team16/all")
dir<-paste("C:/Users/lenovo/Desktop/BALL/team-NBA/team16/all/",filenames,sep="")
all<-data.frame()
for (i in 1:45){
  b=dir[i]
  new_1<-read.csv(dir[i],1)
  all<-rbind(all,new_1)
}
all$pk<-gsub(" ","",all$pk)
all$pk<-gsub("76人","七六人",all$pk)
write.csv(all,"C:/Users/lenovo/Desktop/BALL/team-NBA/team16/all.csv")
#----------------#
#对照组  30只队伍
#----------------#           #重构一个30只队伍的数据
filenames<-list.files("C:/Users/lenovo/Desktop/BALL/team-NBA/team16/all30")
dir<-paste("C:/Users/lenovo/Desktop/BALL/team-NBA/team16/all30/",filenames,sep="")
all30<-data.frame()
for (i in 1:30){
  b=dir[i]
  new_1<-read.csv(dir[i],1)
  all30<-rbind(all30,new_1)
}
write.csv(all30,"C:/Users/lenovo/Desktop/BALL/team-NBA/team16/all30.csv")

#挑选出30支队伍的对手数据
data<-all30
data$pk<-gsub(" ","",data$pk)
data$pk<-gsub("76人","七六人",data$pk)
#七六人98-78湖人  转换成 湖人78-98七六人    通过这样的方式匹配对手的数据

#先拆分两队比分
wl<-gsub("\\d+-\\d+[\u4e00-\u9fa5]+","",data$pk)
wr<-gsub("[\u4e00-\u9fa5]+\\d+-\\d+","",data$pk)
tscore<-gsub("[\u4e00-\u9fa5]","",data$pk)
sl<-gsub("-\\d+","",tscore)
sr<-gsub("\\d+-","",tscore)
#再重组
pk1<-list(NULL)  #构建一个空值的list
length(pk1)<-2460
for(i in 1:2460){
  pk1[[i]]<-paste(wr[i],sr[i],"-",sl[i],wl[i],sep = "")
}
pk1<-unlist(pk1)
data$pk1<-pk1


#依据data中的pk1将对手数据赋给all
dat<-data.frame()

pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed",
                       total = 3690, clear = FALSE, width= 60)
for(i in 1 :3690)try({
  pb$tick()
  data1<-cbind(all[i,],data[which(data$pk1==all[i,]$pk),])
  dat<-rbind(dat,data1)
  Sys.sleep(0.01)
 },
 silent = T)

write.csv(dat,"C:/Users/lenovo/Desktop/BALL/team-NBA/team16/all16.csv")


#----------------————------------------------#
#------读取17年所有选秀球员的数据，并整合----#
#----------------————------------------------#

filenames<-list.files("C:/Users/lenovo/Desktop/BALL/team-NBA/team17/all")
dir<-paste("C:/Users/lenovo/Desktop/BALL/team-NBA/team17/all/",filenames,sep="")
all<-data.frame()
for (i in 1:48){
  b=dir[i]
  new_1<-read.csv(dir[i],1)
  all<-rbind(all,new_1)
}
all$pk<-gsub(" ","",all$pk)
all$pk<-gsub("76人","七六人",all$pk)
write.csv(all,"C:/Users/lenovo/Desktop/BALL/team-NBA/team17/all.csv")
#----------------#
#对照组  30只队伍
#----------------#           #重构一个30只队伍的数据
filenames<-list.files("C:/Users/lenovo/Desktop/BALL/team-NBA/team17/all30")
dir<-paste("C:/Users/lenovo/Desktop/BALL/team-NBA/team17/all30/",filenames,sep="")
all30<-data.frame()
for (i in 1:30){
  b=dir[i]
  new_1<-read.csv(dir[i],1)
  all30<-rbind(all30,new_1)
}
write.csv(all30,"C:/Users/lenovo/Desktop/BALL/team-NBA/team17/all30.csv")

#挑选出30支队伍的对手数据
data<-all30
data$pk<-gsub(" ","",data$pk)
data$pk<-gsub("76人","七六人",data$pk)
#七六人98-78湖人  转换成 湖人78-98七六人    通过这样的方式匹配对手的数据

#先拆分两队比分
wl<-gsub("\\d+-\\d+[\u4e00-\u9fa5]+","",data$pk)
wr<-gsub("[\u4e00-\u9fa5]+\\d+-\\d+","",data$pk)
tscore<-gsub("[\u4e00-\u9fa5]","",data$pk)
sl<-gsub("-\\d+","",tscore)
sr<-gsub("\\d+-","",tscore)
#再重组
pk1<-list(NULL)  #构建一个空值的list
length(pk1)<-2460
for(i in 1:2460){
  pk1[[i]]<-paste(wr[i],sr[i],"-",sl[i],wl[i],sep = "")
}
pk1<-unlist(pk1)
data$pk1<-pk1


#依据data中的pk1将对手数据赋给all
dat<-data.frame()

pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed",
                       total = 3936, clear = FALSE, width= 60)
for(i in 1 :3936)try({
  pb$tick()
  data1<-cbind(all[i,],data[which(data$pk1==all[i,]$pk),])
  dat<-rbind(dat,data1)
  Sys.sleep(0.01)
},
silent = T)

write.csv(dat,"C:/Users/lenovo/Desktop/BALL/team-NBA/team17/all17.csv")



