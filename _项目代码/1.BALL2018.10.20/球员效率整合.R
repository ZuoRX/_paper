rm(list=ls())
library(rJava)
library(xlsx)    
library(plyr)
library(dplyr) 
library(dtplyr)
library(openxlsx)
#数据包络模型
library(lpSolveAPI)   
library(ucminf)
library(Benchmarking)
library(bootstrap)
library(TFDEA)

#球员效率可视化



#==========================================================================#
#--------------------------16年球员平均效率--------------------------------#
#==========================================================================#
final16<-read.csv("C:/Users/lenovo/Desktop/BALL/team-NBA/final16.csv")
f16<-filter(final16,final16$order!=0)

#-----------------------#
#-----82场均值效率------#
#-----------------------#

#提取16年第一阶段效率加和
a1<-list()
length(a1)<-3198

a1[[1]]<-f16$e1[1]
for (i in 2:3198) {
  if(f16$order[i]==f16$order[i-1]){
    a1[[i]]<-f16$e1[i]+a1[[i-1]]
  }
  else{
    a1[[i]]<-f16$e1[i]
  }
}
a1<-as.data.frame(as.vector(unlist(a1)))

#提取16年第二阶段效率加和
a2<-list()
length(a2)<-3198

a2[[1]]<-f16$e2[1]
for (i in 2:3198) {
  if(f16$order[i]==f16$order[i-1]){
    a2[[i]]<-f16$e2[i]+a2[[i-1]]
  }
  else{
    a2[[i]]<-f16$e2[i]
  }
}
a2<-as.data.frame(as.vector(unlist(a2)))

#提取16年球员整体效率82场加和
a3<-list()
length(a3)<-3198

a3[[1]]<-f16$e[1]
for (i in 2:3198) {
  if(f16$order[i]==f16$order[i-1]){
    a3[[i]]<-f16$e[i]+a3[[i-1]]
  }
  else{
    a3[[i]]<-f16$e[i]
  }
}
a3<-as.data.frame(as.vector(unlist(a3)))

#TTTTTTTTTTTTTTTTTTTTTTTTTT
#39个选秀球员82场的平均效率
e82<-data.frame(f16$game,f16$team,f16$player,f16$order,
                a1,a2,a3)  
eff82<-filter(e82,e82$f16.game==82)
names(eff82)<-c("game","team","player","order","a1","a2","a3")
eff82$a1<-eff82$a1/82
eff82$a2<-eff82$a2/82
eff82$a3<-eff82$a3/82


#---------------------------#
#-----有效场数均值效率------#
#---------------------------#
ff16<-filter(f16,f16$e!=0)

#提取16年第一阶段效率加和
b1<-list()
length(b1)<-1804
bb1<-rep(1,1804)
bbb1<-rep(0,1804)


b1[[1]]<-ff16$e1[1]

for (i in 2:1804) {
  if(ff16$order[i]==ff16$order[i-1]){
    b1[[i]]<-ff16$e1[i]+b1[[i-1]]
    bb1[i]<-1+bb1[i-1]
  }
  else{
    b1[[i]]<-ff16$e1[i]
    bb1[i]<-1
    bbb1[i-1]<-bb1[i-1]
  }
}
b1<-as.data.frame(as.vector(unlist(b1)))

bbb1[1804]<-1
fb1<-data.frame(b1,bb1,bbb1)

fb1<-filter(fb1,fb1$bbb1!=0)
names(fb1)<-c("e","n","nn")
e_fb1<-fb1$e/fb1$n           #有效上场平均结果

#-----------------------提取16年第2阶段效率均值----------------------#
b2<-list()
length(b2)<-1804
bb2<-rep(1,1804)
bbb2<-rep(0,1804)

b2[[1]]<-ff16$e1[1]
for (i in 2:1804) {
  if(ff16$order[i]==ff16$order[i-1]){
    b2[[i]]<-ff16$e2[i]+b2[[i-1]]
    bb2[i]<-1+bb2[i-1]
  }
  else{
    b2[[i]]<-ff16$e2[i]
    bb2[i]<-1
    bbb2[i-1]<-bb2[i-1]
  }
}
b2<-as.data.frame(as.vector(unlist(b2)))

bbb2[1804]<-1
fb2<-data.frame(b2,bb2,bbb2)

fb2<-filter(fb2,fb2$bbb2!=0)
names(fb2)<-c("e","n","nn")
e_fb2<-fb2$e/fb2$n

#-------------------提取16年整体效率均值---------------------------#
b3<-list()
length(b3)<-1804
bb3<-rep(1,1804)
bbb3<-rep(0,1804)

b3[[1]]<-ff16$e1[1]

for (i in 2:1804) {
  if(ff16$order[i]==ff16$order[i-1]){
    b3[[i]]<-ff16$e[i]+b3[[i-1]]
    bb3[i]<-1+bb3[i-1]
  }
  else{
    b3[[i]]<-ff16$e[i]
    bb3[i]<-1
    bbb3[i-1]<-bb3[i-1]
  }
}
b3<-as.data.frame(as.vector(unlist(b3)))

bbb3[1804]<-1
fb3<-data.frame(b3,bb3,bbb3)

fb3<-filter(fb3,fb3$bbb3!=0)
names(fb3)<-c("e","n","nn")
e_fb3<-fb3$e/fb3$n

#----整理导出-------------#
nn16<-cbind(eff82,fb3$n,e_fb1,e_fb2,e_fb3)
names(nn16)[5:11]<-c("e82_1","e82_2","e82_A","n_game","evalid_1","evalid_2","evalid_A")
write.csv(nn16,"C:/Users/lenovo/Desktop/BALL/team-NBA/f16.csv")



#==========================================================================#
#--------------------------17年球员平均效率--------------------------------#
#==========================================================================#
final17<-read.csv("C:/Users/lenovo/Desktop/BALL/team-NBA/final17.csv")
f17<-filter(final17,final17$order!=0)

#-----------------------#
#-----82场均值效率------#
#-----------------------#

#提取17年第一阶段效率加和
a1<-list()
length(a1)<-3198

a1[[1]]<-f17$e1[1]
for (i in 2:3198) {
  if(f17$order[i]==f17$order[i-1]){
    a1[[i]]<-f17$e1[i]+a1[[i-1]]
  }
  else{
    a1[[i]]<-f17$e1[i]
  }
}
a1<-as.data.frame(as.vector(unlist(a1)))

#提取17年第二阶段效率加和
a2<-list()
length(a2)<-3198

a2[[1]]<-f17$e2[1]
for (i in 2:3198) {
  if(f17$order[i]==f17$order[i-1]){
    a2[[i]]<-f17$e2[i]+a2[[i-1]]
  }
  else{
    a2[[i]]<-f17$e2[i]
  }
}
a2<-as.data.frame(as.vector(unlist(a2)))

#提取17年球员整体效率82场加和
a3<-list()
length(a3)<-3198

a3[[1]]<-f17$e[1]
for (i in 2:3198) {
  if(f17$order[i]==f17$order[i-1]){
    a3[[i]]<-f17$e[i]+a3[[i-1]]
  }
  else{
    a3[[i]]<-f17$e[i]
  }
}
a3<-as.data.frame(as.vector(unlist(a3)))

#TTTTTTTTTTTTTTTTTTTTTTTTTT
#39个选秀球员82场的平均效率
e82<-data.frame(f17$game,f17$team,f17$player,f17$order,
                a1,a2,a3)  
eff82<-filter(e82,e82$f17.game==82)
names(eff82)<-c("game","team","player","order","a1","a2","a3")
eff82$a1<-eff82$a1/82
eff82$a2<-eff82$a2/82
eff82$a3<-eff82$a3/82


#---------------------------#
#-----有效场数均值效率------#
#---------------------------#
ff17<-filter(f17,f17$t15!=0)

#提取17年第一阶段效率加和
b1<-list()
length(b1)<-2101
bb1<-rep(1,2101)
bbb1<-rep(0,2101)

b1[[1]]<-ff17$e1[1]

for (i in 2:2101) {
  if(ff17$order[i]==ff17$order[i-1]){
    b1[[i]]<-ff17$e1[i]+b1[[i-1]]
    bb1[i]<-1+bb1[i-1]
  }
  else{
    b1[[i]]<-ff17$e1[i]
    bb1[i]<-1
    bbb1[i-1]<-bb1[i-1]
  }
}
b1<-as.data.frame(as.vector(unlist(b1)))

bbb1[2101]<-1
fb1<-data.frame(b1,bb1,bbb1)

fb1<-filter(fb1,fb1$bbb1!=0)
names(fb1)<-c("e","n","nn")
e_fb1<-fb1$e/fb1$n           #有效上场平均结果

#-----------------------提取17年第2阶段效率均值----------------------#
b2<-list()
length(b2)<-2101
bb2<-rep(1,2101)
bbb2<-rep(0,2101)

b2[[1]]<-ff17$e1[1]
for (i in 2:2101) {
  if(ff17$order[i]==ff17$order[i-1]){
    b2[[i]]<-ff17$e2[i]+b2[[i-1]]
    bb2[i]<-1+bb2[i-1]
  }
  else{
    b2[[i]]<-ff17$e2[i]
    bb2[i]<-1
    bbb2[i-1]<-bb2[i-1]
  }
}
b2<-as.data.frame(as.vector(unlist(b2)))

bbb2[2101]<-1
fb2<-data.frame(b2,bb2,bbb2)

fb2<-filter(fb2,fb2$bbb2!=0)
names(fb2)<-c("e","n","nn")
e_fb2<-fb2$e/fb2$n

#-------------------提取17年整体效率均值---------------------------#
b3<-list()
length(b3)<-2101
bb3<-rep(1,2101)
bbb3<-rep(0,2101)

b3[[1]]<-ff17$e1[1]

for (i in 2:2101) {
  if(ff17$order[i]==ff17$order[i-1]){
    b3[[i]]<-ff17$e[i]+b3[[i-1]]
    bb3[i]<-1+bb3[i-1]
  }
  else{
    b3[[i]]<-ff17$e[i]
    bb3[i]<-1
    bbb3[i-1]<-bb3[i-1]
  }
}
b3<-as.data.frame(as.vector(unlist(b3)))

bbb3[2101]<-1
fb3<-data.frame(b3,bb3,bbb3)

fb3<-filter(fb3,fb3$bbb3!=0)
names(fb3)<-c("e","n","nn")
e_fb3<-fb3$e/fb3$n

#----整理导出-------------#
nn17<-cbind(eff82,fb3$n,e_fb1,e_fb2,e_fb3)
names(nn17)[5:11]<-c("e82_1","e82_2","e82_A","n_game","evalid_1","evalid_2","evalid_A")
write.csv(nn17,"C:/Users/lenovo/Desktop/BALL/team-NBA/f17.csv")




#=======================================================#
#==============60选秀球员两年效率对比===================#
#=======================================================#
f16<-read.csv("C:/Users/lenovo/Desktop/BALL/team-NBA/f16.csv")
f16$e1<-round(f16$e1,3)
f16$e2<-round(f16$e2,3)
f16$eA<-round(f16$eA,3)
f16$ev1<-round(f16$ev1,3)
f16$ev2<-round(f16$ev2,3)
f16$evA<-round(f16$evA,3)

f<-paste(f16$o,"&",f16$player,"&",f16$win,"&",f16$team,"&",
         f16$s,"&",f16$n16,"&",f16$e1,"&",f16$e2,"&",f16$eA,"&",
         f16$ev1,"&",f16$ev2,"&",f16$evA,"\\",
           sep="")

write.csv(f,"C:/Users/lenovo/Desktop/f1.csv")







