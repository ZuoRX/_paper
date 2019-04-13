library(dplyr)
library(gdata)


#============================#
#======糅合34个财务数据======#
#============================#
filenames<-list.files("C:/Users/lenovo/Desktop/textming/data/steel")
filenames
dir<-paste("C:/Users/lenovo/Desktop/textming/data/steel/",filenames,sep="")


all<-data.frame()

for(i in 1:34){
 n1<-read.xls(dir[1],1,header=F,fileEncoding="utf-8")
 n1<-n1[,1:15]

 n2<-t(n1) %>% as.data.frame()
 n2$DMU<-c(rep(i,15))

 all<-rbind(all,n2[-1,])
}

names(all)<-c("time","T_asset","T_cost","debt","G_income","N_profits","T_value","share","I_assets","DMU")
#intangible assets无形资产 gerneral income 总收入
write.csv(all,"C:/Users/lenovo/Desktop/textming/data/rawdata.csv")



all<-read.csv("C:/Users/lenovo/Desktop/textming/data/rawdata.csv")
all$name[all$DMU==1]<-"SZ000708	大冶特钢"
all$name[all$DMU==2]<-"SZ002318	久立特材"
all$name[all$DMU==3]<-"SZ002443	金洲管道"
all$name[all$DMU==4]<-"SZ002478	常宝股份"
all$name[all$DMU==5]<-"SZ002756	永兴特钢"
all$name[all$DMU==6]<-"SH600117	西宁特钢"
all$name[all$DMU==7]<-"SH600399	ST抚钢"
all$name[all$DMU==8]<-"SH600507	方大特钢"
all$name[all$DMU==9]<-"SH601005	重庆钢铁"
all$name[all$DMU==10]<-"SZ000709	河钢股份"
all$name[all$DMU==11]<-"SZ000717	韶钢松山"
all$name[all$DMU==12]<-"SZ000761	本钢板材"
all$name[all$DMU==13]<-"SZ000778	新兴铸管"
all$name[all$DMU==14]<-"SZ000825	太钢不锈"
all$name[all$DMU==15]<-"SZ000898	鞍钢股份"
all$name[all$DMU==16]<-"SZ000932	华菱钢铁"
all$name[all$DMU==17]<-"SZ000959	首钢股份"
all$name[all$DMU==18]<-"SZ002075	沙钢股份"
all$name[all$DMU==19]<-"SZ002110	三钢闽光"
all$name[all$DMU==20]<-"SH600010	包钢股份"
all$name[all$DMU==21]<-"SH600019	宝钢股份"
all$name[all$DMU==22]<-"SH600022	山东钢铁"
all$name[all$DMU==23]<-"SH600126	杭钢股份"
all$name[all$DMU==24]<-"SH600231	凌钢股份"
all$name[all$DMU==25]<-"SH600282	南钢股份"
all$name[all$DMU==26]<-"SH600307	酒钢宏兴"
all$name[all$DMU==27]<-"SH600569	安阳钢铁"
all$name[all$DMU==28]<-"SH600581	八一钢铁"
all$name[all$DMU==29]<-"SH600782	新钢股份"
all$name[all$DMU==30]<-"SH600808	马钢股份"
all$name[all$DMU==31]<-"SH601003	柳钢股份"
all$name[all$DMU==32]<-"SH603315	福鞍股份"
all$name[all$DMU==33]<-"SZ300034	钢研高纳"
all$name[all$DMU==34]<-"SZ002423	中原特钢"

write.csv(all,"C:/Users/lenovo/Desktop/textming/data/rawdata1.csv")


#================================#
#======糅合34个季度股票价格======#
#================================#（求市值）

filenames<-list.files("C:/Users/lenovo/Desktop/textming/data/season_price")
filenames#b并没有按预期排序！！！！！！！！！！！！！！！！
dir<-paste("C:/Users/lenovo/Desktop/textming/data/season_price/",filenames,sep="")
#a<-read.xls("C:/Users/lenovo/Desktop/textming/data/season_price/a1Table.xls")




all<-data.frame()

for(i in 1:34){
  n1<-read.xls(dir[1],sheet=1, verbose=FALSE,perl="perl",
               fileEncoding="utf-8", na.strings=c("NA","#DIV/0!"))
  n1<-n1[,1:15]
  
  n2<-t(n1) %>% as.data.frame()
  n2$DMU<-c(rep(i,15))
  
  all<-rbind(all,n2[-1,])
}

names(all)<-c("time","T_asset","T_cost","debt","G_income","N_profits","T_value","share","I_assets","DMU")
#intangible assets无形资产 gerneral income 总收入
write.csv(all,"C:/Users/lenovo/Desktop/textming/data/rawdata.csv")


#====================统一年度与季度报表的差别===========================#
aa<-read.csv("C:/Users/lenovo/Desktop/textming/data/rawdata1.csv")

b<-rep(c(rep(1:4,3),1,2),34)
c<-rep(1:14,34)
aa$c<-c
aa$b<-b

aa$tasset<-















