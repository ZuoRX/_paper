library(ucminf)
library(Benchmarking)
library(bootstrap)
library(TFDEA)
library(productivity) #Malmquist Productivity Index
#library(rDEA)         #考虑环境变量
library(frm)         #分式规划
library(xlsx)
library(tcltk)
library(progress)
library(plyr)
library(magrittr)  
#---------------------------------------#
#---------钢铁行业差分处理--------------#
#---------------------------------------#
v29<-read.csv("c:/users/lenovo/desktop/stock/steel industry/two type/raw_v_29.csv",
              header = F)
v29<-v29[,c(-1:-5)]

#先取自然对数
a<-apply(v29, 2, log())
#再做差分
d1<-function(a){
  -diff(a,1)   #反向
}

a1<-apply(a,2,d1)


write.csv(a1,"c:/users/lenovo/desktop/stock/steel industry/two type/d29.csv")


#统计列里面停牌的天数
zero<-function(a){
  table(a==0)
}

day0<-apply(a1,2,zero)

write.csv(day0,"c:/users/lenovo/desktop/stock/steel industry/two type/停牌天数.csv")

#---------------------------------------#
#---------生物行业差分处理--------------#
#---------------------------------------#
v24<-read.csv("c:/users/lenovo/desktop/stock/biology/three type/autoencoder/24.csv",
              header = F)


#先取自然对数
a<-apply(v24, 2, log)
#再做差分
d1<-function(a){
  -diff(a,1)   #反向
}

a1<-apply(a,2,d1)


write.csv(a1,"c:/users/lenovo/desktop/stock/biology/three type/autoencoder/d24.csv")


#统计列里面停牌的天数
zero<-function(a){
  table(a==0)
}

day0<-apply(a1,2,zero)

write.csv(day0,"c:/users/lenovo/desktop/stock/biology/three type/autoencoder/停牌天数.csv")






