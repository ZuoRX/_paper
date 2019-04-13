#数据包络模型
#library(lpSolveAPI)   #这些包要用R的32位操作系统
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
library(magrittr)  #管道符

#--------------------------------------#
#---------------整合数据---------------#
#--------------------------------------#
filenames<-list.files("C:/Users/lenovo/Desktop/stock/steel/data")

all<-data.frame()

pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed",
                       total = 29, clear = FALSE, width= 60)
for(i in 1:29){
  
  pb$tick()

  url<-paste("C:/Users/lenovo/Desktop/stock/steel/data/",filenames[i],sep = "") 
  
  a2<-read.csv(url,1,encoding="UTF-8")
  names(a2)

  b2<-a2[,c(-1:-5,-7)]

  name1<-gsub(".csv","",filenames[i])
  name<-rep(name1,1900)

  order<-c(1:1900)
  new<-data.frame(name,order,b2)

  all<-rbind(all,new)
}

write.csv(all,"C:/Users/lenovo/Desktop/stock/steel/all.csv")

#--------------------------------------#
#---------------求效率值---------------#
#--------------------------------------#
all<-read.csv("C:/Users/lenovo/Desktop/stock/steel/all.csv")

dat<-data.frame()
dat_s<-data.frame()

minmaxscale<-function(a){
  center <- sweep(a, 2, apply(a, 2, min),'-') 
  R <- apply(a, 2, max) - apply(a,2,min)   
  #算出极差
  return(sweep(center, 2, R, "/")) 
}



pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed",
                       total = 1900, clear = FALSE, width= 60)

#构建同一天的效率评估数据组  即29个数据一组
for(i in 1:1900){
  
pb$tick()
aa<-subset(all,all$order==i)
aa1<-aa[,4:7]

net_in<-minmaxscale(as.data.frame(aa1$net_in))
t_value<-minmaxscale(as.data.frame(aa1$t_value))
close<-minmaxscale(as.data.frame(aa1$close))
l_close<-minmaxscale(as.data.frame(aa1$l_close))

#投入只有昨日收盘价、今日资金净流入、总市值
x<-data.frame(l_close,net_in,t_value) %>% as.matrix()

#产出只有今日收盘价
y<-close %>% as.matrix()

#收集dea效率值
e=dea(x,y)
e1<-e$eff%>%t()
dat<-rbind(dat,e1)

#收集超效率值
se<-sdea(x,y)
se1<-se$eff%>%t()
dat_s<-rbind(dat_s,se1)
}

write.csv(dat,"C:/Users/lenovo/Desktop/stock/steel/e1.csv")
write.csv(dat_s,"C:/Users/lenovo/Desktop/stock/steel/super_e1.csv")





#age mean考虑单独构建函数
d=function(data){
  
  yiqi$net_in<-minmaxscale(as.data.frame(yiqi$net_in))
  yiqi$net_in<-as.vector(unlist(yiqi$net_in[1]))
  
  #投入只有昨日收盘价和今日资金净流入
  x<-data.frame(yiqi$net_in,yiqi$l_close) %>% as.matrix()
  
  #产出只有今日收盘价
  y<-yiqi$close %>% as.matrix()
  
  e=dea(x,y)
  return(e$eff)
}

ddply(data,.(DMU),.fun=amean)    #得出dataframe的形式








yiqi<-read.csv("c:/users/lenovo/desktop/stock/yiqi.csv")

#names(yiqi)<-c("date","up_down","change","net_in","main net in",
#               "open","highest","lowest","close","l_close","fluctuate","volume")
yiqi1<-yiqi[,c(4,9,10)]
head(yiqi1,7)
names(yiqi1)<-c("net_in","close","l_close")

yiqi$net_in<-minmaxscale(as.data.frame(yiqi$net_in))
yiqi$net_in<-as.vector(unlist(yiqi$net_in[1]))


# 投入指标：
# 1. 净流入（标准化）           （成交量）
# 2. 昨日收盘价                 （开盘价）
# 3. 大盘今日涨跌               （个股受大盘影响）

# 产出指标
# 1. 收盘价

#投入只有昨日收盘价和今日资金净流入
x<-data.frame(yiqi$net_in,yiqi$l_close) %>% as.matrix()

#产出只有今日收盘价
y<-yiqi$close %>% as.matrix()

x<-as.matrix(x)
y<-as.matrix(y)


e1<-dea(x,y)
e1$eff

yiqi$e1<-e1$eff

write.csv(yiqi,"c:/users/lenovo/desktop/yiqi.csv")


table(0.8<=e1$eff<=0.9)














