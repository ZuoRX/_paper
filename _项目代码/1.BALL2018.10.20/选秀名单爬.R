rm(list=ls())
library(bitops)
library(RCurl)
library(XML)
library(xlsx)
cat("\014") 

#------------------#
#------网址1-------#
#------------------#
myheader<-c(
  "User-Agent"="Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.1.6) ",
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  "Accept-Language"="en-us",
  "Connection"="keep-alive",
  "Accept-Charset"="GB2312,utf-8,ISO-8859-1,GBK;q=0.7,*;q=0.7"
)
url<-"https://nba.hupu.com/draft/2016?round=2"
temp<-getURL(url,header="myheader")  #"UTF-8" "ISO-8859-1"，"GB2312" "GBK"
#temp
# k1<-xmlParse(temp)         #   解析不了
#k=htmlParse(temp)

k2<-strsplit(temp,"\n")[[1]]#此处\r\n出现问题  \r即回车return   \n即换行new line
k3<-k2[-c(1:12)]
k3[1:10]
# grep("<dt><strong>",k3)
# k3[443]
name=k3[grep("<dt><strong>",k3)+1]  
name
#name1<-gsub("<a target=\"_blank\" href=\"http://bbs.hupu.com/16067126.html\">","",name)
#这里不能用gsub的原因：数字都不相同
name1<-gsub("<a target=\"_blank\" href=\"http://bbs.hupu.com/\\d+.html\">","",name)
name1  #所以此处用转义字符替代数字
name2<-gsub("                                                                        ","",name1)
name2
list(name2)
name3<-gsub("\\(","",name2)
name3<-gsub("\\)","",name3)
name3<-strsplit(name3,",")
name3
as.data.frame(name3)
write.csv(name3,"c:/users/lenovo/desktop/name.csv")

#------------------#
#------网址2-------#
#------------------#
myheader<-c(
  "User-Agent"="Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.1.6) ",
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  "Accept-Language"="en-us",
  "Connection"="keep-alive",
  "Accept-Charset"="GB2312,utf-8,ISO-8859-1,GBK;q=0.7,*;q=0.7"
)
url<-"http://www.stat-nba.com/award/item11isnba1season2016.html"
temp<-getURL(url,header="myheader")  #"UTF-8" "ISO-8859-1"，"GB2312" "GBK"
#temp
# k1<-xmlParse(temp)         #   解析不了
#k=htmlParse(temp)

k2<-strsplit(temp,"\n")[[1]]#此处\r\n出现问题  \r即回车return   \n即换行new line
k3<-k2[-c(1:12)]
k3[1:10]
# grep("<dt><strong>",k3)
# k3[443]
name=k3[grep("<dt><strong>",k3)+1]  
name
#name1<-gsub("<a target=\"_blank\" href=\"http://bbs.hupu.com/16067126.html\">","",name)
#这里不能用gsub的原因：数字都不相同
name1<-gsub("<a target=\"_blank\" href=\"http://bbs.hupu.com/\\d+.html\">","",name)
name1  #所以此处用转义字符替代数字
name2<-gsub("                                                                        ","",name1)
name2
list(name2)
name3<-gsub("\\(","",name2)
name3<-gsub("\\)","",name3)
name3<-strsplit(name3,",")
name3
as.data.frame(name3)
write.csv(name3,"c:/users/lenovo/desktop/name.csv")



