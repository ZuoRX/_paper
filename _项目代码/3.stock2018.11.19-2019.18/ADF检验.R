library(urca)
library(xlsx)
library(plyr)
library(tseries)#载入tseries包
library(magrittr)
# ADF原假设为，序列存在单位根，即非平稳，对于一个平稳的时序数据，
# 就需要在给定的置信水平上显著，拒绝原假设。 
# 若得到的统计量显著小于3个置信度（1%，5%，10%）的临界统计值时，
# 说明是拒绝原假设的。另外是看P-value是否非常接近0.（4位小数基本即可）

#原始数据
raw24<-read.csv("c:/users/lenovo/desktop/stock/steel industry/two type/raw24.csv")


#差分后的数据
d24<-read.csv("c:/users/lenovo/desktop/stock/steel industry/two type/d24.csv",
              header = F)


#ur.df(raw24)

#apply(raw24, 2, ur.df)
adf<-apply(d24, 2,adf.test)

d<-data.frame()
for(i in 1 : 24){
  a<-data.frame(unlist(adf[[i]]))%>%t()
  d<-rbind(d,a)
}

write.csv(d,"c:/users/lenovo/desktop/stock/R code/adf_d.csv")


#对钢铁行业收盘价进行ADF检验
s<-read.xlsx("c:/users/lenovo/desktop/stock/R code/steel industry ADF.xlsx",1)
apply(s, 2,adf.test)

#银行行业


s<-read.csv("c:/users/lenovo/desktop/stock/bank/industry-individual/bankADF.csv")
apply(s, 2,adf.test)


















