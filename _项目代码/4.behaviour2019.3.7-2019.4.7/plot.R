library(zoo)
library(ggplot2)
library(magrittr)  #管道符
library(tidyr) #数据转换包
library(splines) #数据差值包
library(dplyr) 
library(RColorBrewer)


time<-read.csv("e:/text/data/useful data/time.csv")
time<-arrange(time,time)

goods1<-read.csv("e:/text/data/goods1/behavour_user6216day23.csv")

a<-data.frame(time$date,goods1[,2])
names(a)<-c("time","value")
a$time<-as.POSIXct(a$time)

#plot(a$buy)
ggplot(a,aes(x=time,y=value))+
  xlab("time")+
  ylab("5 min frequency buying quantities")+
  scale_x_datetime(date_labels = "%m/%d",
                   date_breaks = "2 day",
                   date_minor_breaks = "1 day")+
  geom_line(aes(col=("#00ffff")))+
  theme_bw()

scale_colour_manual(values="#ff0000")
 




write.csv(a,"e:/text/data/useful data/time_4-goods1.csv",row.names = F)


#f1<-file.choose()
x<-read.csv("e:/text/data/useful data/time_4-goods1.csv",header=T,row.names = 1)

# #方法一
# x_xts<-as.xts(x)
# plot.xts(x_xts)
# 
# #方法二
# x_zoo<-zoo(x,order.by = as.factor(rownames(x)))
# plot.zoo(x_zoo)


f3<-read.table("e:/text/data/useful data/time_4-goods1.csv",
               header=T,sep = ",")
f3$pv<-f3$pv/100
f3$time.date<-strptime(f3$time.date, "%Y/%m/%d %H:%M")%>%as.POSIXct()

df1 <- f3 %>% gather("item",value,-1) %>% 
  bind_cols(data.frame(item_id=rep(1:4,6048))) 


#ggplot2
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(df1,aes(x=time.date,y=value,fill=item))+
  xlab("time")+
  ylab("5 min frequency")+
  scale_x_datetime(date_labels = "%m/%d",
                   date_breaks = "2 day",
                   date_minor_breaks = "360 min")+
  geom_line()+
  scale_colour_manual(values=cbbPalette)


