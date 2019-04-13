library(urca)
library(plyr)
library(tseries)#载入tseries包
library(vmd)
library(magrittr)  #管道符


data<-read.csv("e:/text/data/goods1/behavour_user6216day23.csv")

data<-read.csv("e:/text/data/goods2/b_user6423day23.csv")
data<-read.csv("e:/text/data/goods3/b_user4520day23.csv")
data<-read.csv("e:/text/data/goods4/b_user6421day23.csv")

head(data)

feature<-data[,2:18]

#---------------------ADF检验---------------------------#
#Augmented Dickey-Fuller Test
adf<-apply(feature, 2,adf.test)

d<-data.frame()   #收集list格式中的数据
for(i in 1 : 17){
  a<-data.frame(unlist(adf[[i]]))%>%t()
  d<-rbind(d,a)
}

#---------------------pp检验---------------------------#
#Phillips-Perron Test
pp<-apply(feature, 2,pp.test)

d1<-data.frame()
for(i in 1 : 17){
  a1<-data.frame(unlist(pp[[i]]))%>%t()
  d1<-rbind(d1,a1)
}

stationary_test<-cbind(d[,c(1,4)],d1[,c(1,4)])

write.csv(stationary_test,"e:/text/data/useful data/ADF_PP1.csv")



#------------------一阶差分-----------------------------#

#先取自然对数
a<-apply(v24, 2, log)
#再做差分
d1<-function(a){
  -diff(a,1)   #反向
}

a1<-apply(a,2,d1)



#================================VMD分解========================================#
data<-read.csv("e:/text/data/goods1/behavour_user6216day23.csv")

data<-read.csv("e:/text/data/goods2/b_user6423day23.csv")

data<-read.csv("e:/text/data/goods3/b_user4520day23.csv")

data<-read.csv("e:/text/data/goods4/b_user6421day23.csv")

data<-read.csv("e:/text/data/goods_all/buy_vmd/all.csv")

head(data)
time<-read.csv("e:/text/data/useful data/time.csv")

b<-data[,c(2,3,4,5)]
b<-cbind(b,time$time)
names(b)[5]<-"order"
b<-arrange(b,order)

#------buy------#
#提取趋势值
v = vmd(b[,1],alpha=2000,tau=0,DC=FALSE,init=0,tol=1e-6,K=8,orderModes=TRUE)

modes = as.data.frame(v)

#画模态图
pdf(file="C:/Users/lenovo/Desktop/stock/yiqi_fan/modes.pdf")
plot(v)
plot(v,facet='bymode',scales='free')
plot(v,facet='byclass',scales='free')
dev.off()




#合并
b_vmd<-cbind(b,modes[,3:10])
write.csv(b_vmd,"e:/text/data/goods4/buy+pv+fav+vmd.csv",row.names = F)

#提取性别特征
b_vmd_gender<-cbind(b_vmd,data[,c(6,7)])
write.csv(b_vmd_gender,"e:/text/data/goods4/feature/all4+vmd+gender.csv",row.names = F)

#提取年龄特征
b_vmd_age<-cbind(b_vmd,data[,c(8,9,10,11,12,13)])
write.csv(b_vmd_age,"e:/text/data/goods4/feature/all4+vmd+age.csv",row.names = F)

#提取学生信息
b_vmd_student<-cbind(b_vmd,data[,c(14,15)])
write.csv(b_vmd_student,"e:/text/data/goods4/feature/all4+vmd+student.csv",row.names = F)

#提取城市信息
b_vmd_city<-cbind(b_vmd,data[,c(16,17,18)])
write.csv(b_vmd_city,"e:/text/data/goods4/feature/all4+vmd+city.csv",row.names = F)




#------cart------#
#提取趋势值
v = vmd(b[,2],alpha=2000,tau=0,DC=FALSE,init=0,tol=1e-6,K=7,orderModes=TRUE)

modes = as.data.frame(v)

#合并
b1<-cbind(b,modes[,3:10])

write.csv(b1,"e:/text/data/vmd/cart_vmd.csv",row.names = F)

#------fav------#
#提取趋势值
v = vmd(b[,3],alpha=2000,tau=0,DC=FALSE,init=0,tol=1e-6,K=7,orderModes=TRUE)

modes = as.data.frame(v)

#合并
b1<-cbind(b,modes[,3:10])

write.csv(b1,"e:/text/data/vmd/fav_vmd.csv",row.names = F)

#------pv------#
#提取趋势值
vm = vmd(b[,4],alpha=2000,tau=0,DC=FALSE,init=0,tol=1e-6,K=7,orderModes=TRUE)

modes = as.data.frame(vm)

#合并
b1<-cbind(b,modes[,3:10])

write.csv(b1,"e:/text/data/vmd/pv_vmd.csv",row.names = F)



#商品6261统计量分析  峰度偏度
#b4+gender2+age6+student2+depth3
data<-read.csv("e:/text/data/useful data/commodity6261.csv")
data1<-data[,2:18]
names(data1)<-c("buy","cart","fav","pv",
                "gender1","gender2",
                "age1","age2","age3","age4","age5","age6",
                "student1","student2",
                "depth1","depth2","depth3")


#方差  var()  标准差  sd()  均值  mean()
library(psych)
a<-describe(data1)

write.csv(a,"e:/text/data/useful data/describe6261.csv")

library(plyr)
library(TTR)#波动率
n<-c(1:6047)
data1$n<-n
data1<-arrange(data1,desc(n))
data1$buy[which(data1$buy==0)]<-0.01
v<-volatility(data1[,1], n = 6, calc = "close", N = 260, mean0 = FALSE)



#multDM  






