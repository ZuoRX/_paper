rm(list=ls())
library(rJava)
library(xlsx)    
library(plyr)
library(dplyr) 
library(dtplyr)
library(openxlsx)
library(Hmisc)
library(ggplot2)
library(gcookbook)
library(tidyr) #数据转换包
library(splines) #数据差值包
library(maps)
library(sp)
library(RColorBrewer)
library(lattice) 
library(scatterplot3d)
library(gcookbook)
library(ggthemes)

f16<-read.csv("C:/Users/lenovo/Desktop/BALL/team-NBA/f16.csv")
f17<-read.csv("C:/Users/lenovo/Desktop/BALL/team-NBA/f17.csv")

#输出选项
png("C:/Users/lenovo/Desktop/1.png",height = 365,width = 1076)
pdf("C:/Users/lenovo/Desktop/1.pdf")
#png("C:/Users/lenovo/Desktop/BALL/plot/1.png")


stage1<-data.frame(f16$order,f16$e1,-f16$ev1)
names(stage1)<-c("order","e1","ev1")

ggplot(stage1,aes(x=order,y=e1))+geom_bar(stat = "identity")+
       scale_x_continuous(breaks=seq(1, 60, 1))




#horiz=T

############################对称条形图######################################
stage1<-data.frame(f16$order,f16$e1)
names(stage1)<-c("order","e1")
stage2<-data.frame(f16$order,f16$ev1)
names(stage2)<-c("order","ev1")

x<-barplot(as.matrix(stage1[,2:3]), beside=T,  #横着并排还是堆叠
           col=brewer.pal(8,"Set1"),border="white",
           space=c(0.1,3),     #组间距离和 柱子之间的距离
           ylim = c(-0.04,1),
           ylab="Efficiency",cex.lab=1.2
           #xlab = "Acutual Efficiency and Potential Maximum Efficiency"
)
#y<-as.matrix(stage1[,2:3])
text(x,-0.02,labels=stage1$order,cex = 0.7)

#box(which="plot")
box(which="figure")

#输出像素：（1076,365） （679,231）   对应pdf 0.9倍文本宽和5cm高   （1625,565）

















#############################分组条形图######################################
stage1<-data.frame(f16$order,f16$e1,f16$ev1)
names(stage1)<-c("order","e1","ev1")


x<-barplot(as.matrix(stage1[,2:3]), beside=T,  #横着并排还是堆叠
           col=brewer.pal(8,"Set1"),border="white",
           space=c(0.1,3),     #组间距离和 柱子之间的距离
           ylim = c(-0.04,1),
           ylab="Efficiency",cex.lab=1.2
           #xlab = "Acutual Efficiency and Potential Maximum Efficiency"
           )
#y<-as.matrix(stage1[,2:3])
text(x,-0.02,labels=stage1$order,cex = 0.7)

#box(which="plot")
box(which="figure")

#输出像素：（1076,365） （679,231）   对应pdf 0.9倍文本宽和5cm高   （1625,565）

#############################簇状条形图######################################
#----------------#
#------17e2------#
#----------------#
df<-data.frame(f17$order,f17$e2,-f17$ev2)
names(df)<-c("order","e2","ev2")

df1 <- df %>% gather("item",value,-1) %>% 
  bind_cols(data.frame(item_id=rep(1:2,each=39))) 
#使用tidyr和dplyr包进行数据转换

ggplot(df1,aes(x=order,y=value,fill=item))+
 geom_bar(stat="identity", position=position_dodge())

ggplot(df1,aes(order,value))+   
  theme_set(theme_few())+    #v. n.  dodge  躲闪
  geom_bar(aes(fill=item),stat = "identity",width=0.8)+
  xlab("Draft Order")+ylab("Efficiency")+
  scale_x_continuous(breaks=seq(1, 60, 1))+
  scale_y_continuous(breaks=seq(-1,1, 0.2),
                     labels = c("1.0","0.8","0.6","0.4","0.2","0",
                                "0.2","0.4","0.6","0.8","1.0"),
                     limits = c(-1,1))+
  geom_line(aes(y =0.3979,colour="m_e2"))+
  geom_line(aes(y =-0.5967,colour="m_ev2"))+
  geom_line(aes(y =0))+
  geom_text(aes(x=45,y=0.44,label="0.3979"),size=4,colour="#da5f5f")+
  geom_text(aes(x=45,y=-0.64,label="0.5967"),size=4,colour="#48e4e4")+
  theme(legend.title=element_blank(),axis.line = element_line(colour = "black")) 
  
#####(905,309)  (1205,410)

#----------------#
#------17eA------#
#----------------#
df<-data.frame(f17$order,f17$eA,-f17$evA)
names(df)<-c("order","eA","evA")

df1 <- df %>% gather("item",value,-1) %>% 
  bind_cols(data.frame(item_id=rep(1:2,each=39))) 
#使用tidyr和dplyr包进行数据转换

ggplot(df1,aes(order,value))+   
  theme_set(theme_few())+    #v. n.  dodge  躲闪 position="dodge",
  geom_bar(aes(fill=item),stat = "identity",width=0.8)+
  #ylim(-1,1)+
  xlab("Draft Order")+ylab("Efficiency")+
  scale_x_continuous(breaks=seq(1, 60, 1))+
  scale_y_continuous(breaks=seq(-1,1, 0.2),
                     labels = c("1.0","0.8","0.6","0.4","0.2","0",
                                "0.2","0.4","0.6","0.8","1.0"),
                     limits = c(-1,1))+
  geom_line(aes(y =0.5099,colour="m_eA"))+
  geom_line(aes(y =-0.7637,colour="m_evA"))+
  geom_line(aes(y =0))+
  geom_text(aes(x=45,y=0.55,label="0.5099"),size=4,colour="#da5f5f")+
  geom_text(aes(x=45,y=-0.81,label="0.7637"),size=4,colour="#48e4e4")+
  theme(legend.title=element_blank(),axis.line = element_line(colour = "black")) 

#####(905,309)  (1205,410)

#----------------#
#------17e1------#
#----------------#
df<-data.frame(f17$order,f17$e1,-f17$ev1)
names(df)<-c("order","e1","ev1")

df1 <- df %>% gather("item",value,-1) %>% 
  bind_cols(data.frame(item_id=rep(1:2,each=39))) 
#使用tidyr和dplyr包进行数据转换

ggplot(df1,aes(order,value))+   
  theme_set(theme_few())+    #v. n.  dodge  躲闪
  geom_bar(aes(fill=item),stat = "identity",width=0.8)+
  #ylim(-1,1)+
  xlab("Draft Order")+ylab("Efficiency")+
  scale_x_continuous(breaks=seq(1, 60, 1))+
  scale_y_continuous(breaks=seq(-1,1, 0.2),
                     labels = c("1.0","0.8","0.6","0.4","0.2","0",
                                "0.2","0.4","0.6","0.8","1.0"),
                     limits = c(-1,1))+
  geom_line(aes(y =0.6219,colour="m_e1"))+
  geom_line(aes(y =-0.9307,colour="m_ev1"))+
  geom_line(aes(y =0))+
  geom_text(aes(x=45,y=0.66,label="0.6219"),size=4,colour="#da5f5f")+
  geom_text(aes(x=45,y=-0.97,label="0.9307"),size=4,colour="#48e4e4")+
  theme(legend.title=element_blank(),axis.line = element_line(colour = "black")) 

#####(905,309)  (1205,410)

#############################2016簇状条形图######################################
#----------------#
#------16e2------#
#----------------#
df<-data.frame(f16$order,f16$e2,-f16$ev2)
names(df)<-c("order","e2","ev2")

df1 <- df %>% gather("item",value,-1) %>% 
  bind_cols(data.frame(item_id=rep(1:2,each=39))) 
#使用tidyr和dplyr包进行数据转换

ggplot(df1,aes(order,value))+   
  theme_set(theme_few())+    #v. n.  dodge  躲闪
  geom_bar(aes(fill=item),stat = "identity",width=0.8)+
  #ylim(-1,1)+
  xlab("Draft Order")+ylab("Efficiency")+
  scale_x_continuous(breaks=seq(1, 60, 1),limits=c(1,60))+
  scale_y_continuous(breaks=seq(-1,1, 0.2),
                     labels = c("1.0","0.8","0.6","0.4","0.2","0",
                                "0.2","0.4","0.6","0.8","1.0"),
                     limits = c(-1,1))+
  geom_line(aes(y =0.3553 ,colour="m_e2"))+
  geom_line(aes(y =-0.6103 ,colour="m_ev2"))+
  geom_line(aes(y =0))+
  geom_text(aes(x=45,y=0.40,label="0.3553"),size=4,colour="#da5f5f")+
  geom_text(aes(x=45,y=-0.65,label="0.6103"),size=4,colour="#48e4e4")+
  theme(legend.title=element_blank(),axis.line = element_line(colour = "black")) 

#####(905,309)  (1205,410)

#----------------#
#------16eA------#
#----------------#
df<-data.frame(f16$order,f16$eA,-f16$evA)
names(df)<-c("order","eA","evA")

df1 <- df %>% gather("item",value,-1) %>% 
  bind_cols(data.frame(item_id=rep(1:2,each=39))) 
#使用tidyr和dplyr包进行数据转换

ggplot(df1,aes(order,value))+   
  theme_set(theme_few())+    #v. n.  dodge  躲闪
  geom_bar(aes(fill=item),stat = "identity",width=0.8)+
  #ylim(-1,1)+
  xlab("Draft Order")+ylab("Efficiency")+
  scale_x_continuous(breaks=seq(1, 60, 1),limits=c(1,60))+
  scale_y_continuous(breaks=seq(-1,1, 0.2),
                     labels = c("1.0","0.8","0.6","0.4","0.2","0",
                                "0.2","0.4","0.6","0.8","1.0"),
                     limits = c(-1,1))+
  geom_line(aes(y =0.3985 ,colour="m_eA"))+
  geom_line(aes(y =-0.6989,colour="m_evA"))+
  geom_line(aes(y =0))+
  geom_text(aes(x=45,y=0.44,label="0.3985"),size=4,colour="#da5f5f")+
  geom_text(aes(x=24,y=-0.74,label="0.6989"),size=4,colour="#48e4e4")+
  theme(legend.title=element_blank(),axis.line = element_line(colour = "black")) 

#####(905,309)  (1205,410)
905/28  #323mm
228/28  #82mm
#----------------#
#------16e1------#
#----------------#
df<-data.frame(f16$order,f16$e1,-f16$ev1)
names(df)<-c("order","e1","ev1")

df1 <- df %>% gather("item",value,-1) %>% 
  bind_cols(data.frame(item_id=rep(1:2,each=39))) 
#使用tidyr和dplyr包进行数据转换

ggplot(df1,aes(order,value))+   
  theme_set(theme_few())+    #v. n.  dodge  躲闪
  geom_bar(aes(fill=item),stat = "identity",width=0.8)+
  #ylim(-1,1)+
  xlab("Draft Order")+ylab("Efficiency")+
  scale_x_continuous(breaks=seq(1, 60, 1),limits=c(1,60))+
  scale_y_continuous(breaks=seq(-1,1, 0.2),
                     labels = c("1.0","0.8","0.6","0.4","0.2","0",
                                "0.2","0.4","0.6","0.8","1.0"),
                     limits = c(-1,1))+
  geom_line(aes(y =0.4417,colour="m_e1"))+
  geom_line(aes(y =-0.7875,colour="m_ev1"))+
  geom_line(aes(y =0))+
  geom_text(aes(x=45,y=0.48,label="0.4417"),size=4,colour="#da5f5f")+
  geom_text(aes(x=52,y=-0.83,label="0.7875"),size=4,colour="#48e4e4")+
  theme(legend.title=element_blank(),axis.line = element_line(colour = "black")) 

#####(905,309)  (1205,410)




#=================================================================#
#========================矩阵散点图===============================#
#=================================================================#
m<-read.csv("c:/users/lenovo/desktop/BALL/team-NBA/matrix.csv")

library(plotrix)
plot(c(0,1), c(0,1), type="n", main="test draw.ellipse")
plot(m$eA_16,m$eA_17)
draw.ellipse(0, 0, 0.699,0.764,border='red')
abline(v=0.5,lwd=1,col="red")#添加一条垂直直线x=3
abline(h=0.5,lwd=1,col="red")#添加一条垂直直线x=3


ggplot(m,aes(eA_16,eA_17))+   
  #theme_set(theme_few())+    #v. n.  dodge  躲闪
  geom_point()+
  #ylim(-1,1)+
  xlab("eA in 2016-2017 season")+ylab("eA in 2017-2018 season")+
  scale_x_continuous(breaks=seq(0, 1, 0.1),limits=c(0,1))+
  scale_y_continuous(breaks=seq(0, 1, 0.1),
                     limits = c(0,1))+
  geom_text(aes(label = o), nudge_y = -0.25)   
  #geom_line(aes(x=c(0,0,5),y =0.5,colour="red"))+
  #geom_line(aes(y=c(0,0,5),x =0.5,colour="red"))+
  #theme(legend.title=element_blank(),axis.line = element_line(colour = "black")) 




#





































#=======================================================================
#画个三维动图

#二位动图也可以学习下

#看图书馆有啥好看的书？R  诗歌  发朋友圈利器 ScreenToGif   微博如何上热搜

#游泳视频

#双坐标轴   微信有收藏

#Quanteda  vs tm 文本挖掘

#构建一个县的地图， 挺好玩的  @微信  REmap

#cranly：你的R包管理工具

#秋叶ppt  职场干货很多




