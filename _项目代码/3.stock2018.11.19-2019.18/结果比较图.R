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

a<-read.xlsx("c:/users/lenovo/desktop/stock/data/photo-steel.xlsx",1)
a$order<-c(0:24)
df<-a[,c(12,6,11)]
names(df)<-c("order","BiGRU","GRU")
df$BiGRU<-df$BiGRU-0.5
df$GRU<-df$GRU-0.5

df1 <- df %>% gather("item",value,-1) %>% 
  bind_cols(data.frame(item_id=rep(1:2,each=25))) 
#使用tidyr和dplyr包进行数据转换

ggplot(df1,aes(x=order,y=value,fill=item))+
  theme_set(theme_few())+ 
  xlab("steel data")+ylab("rate of return predicted precision")+
  scale_x_continuous(breaks=seq(0,24, 1))+
  geom_bar(stat="identity", position=position_dodge(),width=0.6)+
  scale_y_continuous(breaks=seq(0,0.5, 0.1),
                   labels = c("0.5","0.6","0.7","0.8","0.9","1.0"),
                   limits = c(0,0.5))+
  geom_line(aes(y =0.3605,colour="mean BiGRU"))+
  geom_line(aes(y =0.3140,colour="mean GRU"))+
  geom_text(aes(x=20,y=0.37,label="0.8605"),size=4,colour="#da5f5f")+
  geom_text(aes(x=0,y=0.33,label="0.8063"),size=3,colour="#da5f5f")+#raw-bigru
  geom_text(aes(x=20,y=0.32,label="0.8140"),size=4,colour="#48e4e4")+
  geom_text(aes(x=0.3,y=0.28,label="0.7641"),size=3,colour="#48e4e4")+#raw-gru
theme(legend.title=element_blank(),axis.line = element_line(colour = "black"))

#=====================#
#========银行图=======#
#=====================#
a<-read.xlsx("c:/users/lenovo/desktop/stock/data/photo-bank.xlsx",1)
a$order<-c(0:14)
df<-a[,c(4,2,3)]
names(df)<-c("order","BiGRU","GRU")
df$BiGRU<-df$BiGRU-0.5
df$GRU<-df$GRU-0.5

df1 <- df %>% gather("item",value,-1) %>% 
  bind_cols(data.frame(item_id=rep(1:2,each=15))) 
#使用tidyr和dplyr包进行数据转换

ggplot(df1,aes(x=order,y=value,fill=item))+
  theme_set(theme_few())+ 
  xlab("bank data")+ylab("rate of return predicted precision")+
  scale_x_continuous(breaks=seq(0,14, 1))+
  geom_bar(stat="identity", position=position_dodge(),width=0.6)+
  scale_y_continuous(breaks=seq(0,0.5, 0.1),
                     labels = c("0.5","0.6","0.7","0.8","0.9","1.0"),
                     limits = c(0,0.5))+
  geom_line(aes(y =0.3824,colour="mean BiGRU"))+
  geom_line(aes(y =0.3479,colour="mean GRU"))+
  geom_text(aes(x=1,y=0.395,label="0.8824"),size=4,colour="#da5f5f")+
  geom_text(aes(x=0,y=0.274,label="0.7644"),size=4,colour="#da5f5f")+#raw-bigru
  geom_text(aes(x=1,y=0.36,label="0.8479"),size=4,colour="#48e4e4")+
  geom_text(aes(x=0.25,y=0.24,label="0.7315"),size=4,colour="#48e4e4")+#raw-gru
  theme(legend.title=element_blank(),axis.line = element_line(colour = "black"))



#====================================#
#========steel行业对个股影响图=======#
#====================================#

a<-read.xlsx("c:/users/lenovo/desktop/stock/data/photo-industry-individual.xlsx",1)
a$data1<-a$data1-0.5
#mean(a$data1)  0.7293134
#mean(a$data2)  0.7607101
a$data2<-a$data2-0.5

df1 <- a %>% gather("item",value,-1) %>% 
  bind_cols(data.frame(item_id=rep(1:2,each=24))) 
#使用tidyr和dplyr包进行数据转换

ggplot(df1,aes(x=data,y=value,fill=item))+
  theme_set(theme_few())+ 
  xlab("steel industry individual stocks")+ylab("rate of return predicted precision")+
  scale_x_continuous(breaks=seq(1,24, 1))+
  scale_y_continuous(breaks=seq(0,0.4, 0.1),
                     labels = c("0.5","0.6","0.7","0.8","0.9"),
                     limits = c(0,0.4))+
  geom_bar(stat="identity", position=position_dodge(),width=0.6)+
  geom_line(aes(y =0.2293,colour="mean data1"))+
  geom_text(aes(x=12.5,y=0.24,label="0.7239"),size=4,colour="#da5f5f")+
  geom_line(aes(y =0.2607,colour="mean data2"))+
  geom_text(aes(x=12.5,y=0.27,label="0.7607"),size=4,colour="#48e4e4")+
  theme(legend.title=element_blank(),axis.line = element_line(colour = "black"))
  

#===================================#
#========bank行业对个股影响图=======#
#===================================#

a<-read.xlsx("c:/users/lenovo/desktop/stock/data/photo-I2I-bank.xlsx",1)
a$data1<-a$data1-0.5
a$data2<-a$data2-0.5

# mean(a$data1) #  0.7655577
# mean(a$data2) #  0.8064579



df1 <- a %>% gather("item",value,-1) %>% 
  bind_cols(data.frame(item_id=rep(1:2,each=14))) 
#使用tidyr和dplyr包进行数据转换

ggplot(df1,aes(x=data,y=value,fill=item))+
  theme_set(theme_few())+ 
  xlab("bank industry individual stocks")+ylab("rate of return predicted precision")+
  scale_x_continuous(breaks=seq(1,14, 1))+
  scale_y_continuous(breaks=seq(0,0.4, 0.1),
                     labels = c("0.5","0.6","0.7","0.8","0.9"),
                     limits = c(0,0.4))+
  geom_bar(stat="identity", position=position_dodge(),width=0.6)+
  geom_line(aes(y =0.2656,colour="mean data1"))+
  geom_text(aes(x=1,y=0.275,label="0.7656"),size=4,colour="#da5f5f")+
  geom_line(aes(y =0.3065,colour="mean data2"))+
  geom_text(aes(x=1,y=0.315,label="0.8065"),size=4,colour="#48e4e4")+
  theme(legend.title=element_blank(),axis.line = element_line(colour = "black"))


  
  # 
  
