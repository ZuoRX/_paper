rm(list=ls())
library(rJava)
library(xlsx)     #读表
library(plyr)
library(dplyr)   #截取   很强大
#s神经网络
library(Rcpp)#与RSNNS关联
library(RSNNS)#涉及到神经网络中的其它拓扑结构和网络模型
#Stuttgart Neural Network Simulator（SNNS）是德国斯图加特大学开发的优秀神经网络仿真软件
library(nnet)#提供了最常见的前馈反向传播神经网络算法
library(AMORE)#提供了更为丰富的控制参数，并可以增加多个隐藏层
library(neuralnet)#提供了弹性反向传播算法和更多的激活函数形式
library(autoencoder)
library(deepnet)#实现了一些Deep Learning结构和Neural Network相关算法，
#数据包络模型
library(lpSolveAPI)   #这些包要用R的32位操作系统
library(ucminf)
library(Benchmarking)
library(bootstrap)
library(TFDEA)

#在execl表格中圈住想要读取的数据部分，选择“复制”，
#在R中执行命令read.table("clipboard")可以直接读出选中的数据
#d<-read.table("clipboard")  

#----------------------数据预处理-------------------#
data2016<-read.csv("c:/users/lenovo/desktop/BALL/data_NBA/2016_1/input2016.csv",1)
data1<-filter(data2016,data2016$season=="16-17")
write.csv(data1,"c:/users/lenovo/desktop/BALL/data_NBA/2016_1/16_17.csv")
data2<-filter(data2016,data2016$season=="17-18")
write.csv(data2,"c:/users/lenovo/desktop/BALL/data_NBA/2016_1/17_18.csv")


#----------------------DEA分层-------------------#
b1<-read.csv("c:/users/lenovo/desktop/BALL/data_NBA/2016_1/16_17.csv",1)
b2<-read.csv("c:/users/lenovo/desktop/BALL/data_NBA/2016_1/17_18.csv",1)
data<-rbind(b1,b2)
#all<-write.csv(data,"c:/users/lenovo/desktop/BALL/data_NBA/2016_1/data.csv")
x<-data.frame(data$appearance,data$first,data$time,data$error,data$a.foul)#3输入2负向产出
x<-as.matrix(x)
y<-data.frame(data$total.rebounds,
              data$assists,data$score,
              data$total.rebounds,data$steals,data$blocks)
y<-as.matrix(y)
e<-dea(x,y)
e$eff
summary(e$eff==1)


#-----想法之一-----#
#将数据分为上下两部分
data<-cbind(data,e$eff)
data1<-filter(data,e$eff==1)
x1<-data.frame(data1$appearance,data1$first,data1$time,data1$error,data1$a.foul)#3输入2负向产出
x1<-as.matrix(x1)
y1<-data.frame(data1$shoot,data1$total.rebounds,
              data1$assists,data1$steals,data1$blocks,data1$score)#7产出
y1<-as.matrix(y1)
e1<-dea(x1,y1)
e1$eff
data2<-filter(data,e$eff!=1)


#-----想法之二-----#
#产出太多，导致有效DMU太多，精简输出
yy1<-data.frame(data$total.rebounds,
                data$assists,data$score)
yy2<-data.frame(data$total.rebounds,data$steals,data$blocks)
yy1<-as.matrix(yy1)
yy2<-as.matrix(yy2)
e<-dea(x,yy2)
e$eff
summary(e$eff==1)

#-----想法之三-----#
#用超效率：inf的为一类，大于1小于inf的为一类，等于1的为一类，下面低效率的可分三类，取2类
data<-rbind(b1,b2)
e<-sdea(x,y)
e$eff
summary(e$eff==Inf)
summary(e$eff>2&e$eff<Inf)
summary(e$eff>1.3&e$eff<2)  #？
summary(e$eff>=1&e$eff<=1.3)
summary(e$eff<1)

#-----想法四------#
#混入联盟高水平数据
b3<-read.xlsx("c:/users/lenovo/desktop/BALL/data_NBA/2016/all_NBA.xlsx",1)
data<-rbind(data,b3)
x<-data.frame(data$appearance,data$first,data$time,data$error,data$a.foul)#3输入2负向产出
x<-as.matrix(x)
y<-data.frame(data$total.rebounds,
              data$assists,data$score,
              data$total.rebounds,data$steals,data$blocks)
y<-as.matrix(y)
e<-dea(x,y)
e$eff
summary(e$eff==1)
data1<-cbind(data,e$eff)
summary(e$eff==1&data1$season=="16-17")
#分层
data2<-filter(data1,data1$`e$eff`!=1)
x<-data.frame(data2$appearance,data2$first,data2$time,data2$error,data2$a.foul)#3输入2负向产出
x<-as.matrix(x)
y<-data.frame(data2$total.rebounds,
              data2$assists,data2$score,
              data2$total.rebounds,data2$steals,data2$blocks)
y<-as.matrix(y)
e<-dea(x,y)
e$eff
summary(e$eff==1)























