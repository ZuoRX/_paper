library(RCurl)
library(XML)
library(xml2)
library(dplyr)
library(readr)
library(stringi)
library(stringr)
library(rvest)
library(tcltk)
library(progress)

#=============================导出新闻链接=======================================#
DAT1<-data.frame()

pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed",
                       total = 21, clear = FALSE, width= 60)
for (i in 1:21) try({
  pb$tick()
  site1<-"http://www.csteelnews.com/xwzx/xydt/index_"
  site2<-i
  site3<-".html"
  
  #导出帖子首页中的参数
  web1<-read_html(str_c(site1,site2,site3),encoding="UTF-8",options = "HUGE")
  
  #主题链接
  content<-web1%>%html_nodes(xpath ="//div/ul[@class='lieb']/li/a") %>%html_attrs()
  content1<-gsub("href|target|_blank|\\s+","",content)
  content2<-gsub("\"|,.*","",content1)
  content3<-gsub("=./","=http://www.csteelnews.com/xwzx/xydt/",content2)
  content4<-gsub("=../","=http://www.csteelnews.com/xwzx/",content3)
  content5<-gsub(".*http","http",content4)
  content5<-as.data.frame(content5)
  
  DAT1<-rbind(DAT1,content5)
},
silent = T)



#添加第一个页面的url
#http://www.csteelnews.com/xwzx/xydt/

url<-"http://www.csteelnews.com/xwzx/xydt/index.html"

#导出帖子首页中的参数
web1<-read_html(url,encoding="UTF-8",options = "HUGE")

#主题链接
content<-web1%>%html_nodes(xpath ="//div/ul[@class='lieb']/li/a") %>%html_attrs()
content1<-gsub("href|target|_blank|\\s+","",content)
content2<-gsub("\"|,.*","",content1)
content3<-gsub("=./","=http://www.csteelnews.com/xwzx/xydt/",content2)
content4<-gsub("=../","=http://www.csteelnews.com/xwzx/",content3)
content5<-gsub(".*http","http",content4)
content5<-as.data.frame(content5)

DAT1<-rbind(DAT1,content5)


write.csv(DAT1,"c:/users/lenovo/desktop/textming/data/text data/url.csv")







#=============================导出新闻内容=======================================#

url<-read.csv("c:/users/lenovo/desktop/textming/data/text data/url.csv")
url<-url$content5 %>% as.character()
#grep("http://www.csteelnews",url)

url1<-url[grep("http://www.csteelnews",url)]
url2<-url[-grep("http://www.csteelnews",url)]

#对链接进行分类
# 1."http://www.csteelnews.com/xwzx/xydt/201711/t20171107_355704.html" 
# 2."https://mp.weixin.qq.com/s/NIvt64H7pgD2_2F06IQv5g"
# 3."https://mp.weixin.qq.com/s?__biz=MjM5NzY4ODI0MA==&mid=2650638012&idx=2&sn=83e42b673f3c8438081d7b19759377a0&chksm=bedf006389a88975e8f9d91c347457bc64ee9bf0b82d3fc63949c4423ebdef933bbd85636d24#rd"

#---------------导出第一类---------------#

DAT<-data.frame()

pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed",
                       total = 543, clear = FALSE, width= 60)
for (i in 1:543) try({
  pb$tick()
  
  #解析网页
  web1<-read_html(url1[i],encoding="UTF-8",options = "HUGE")
  
  #导出标题
  title<-web1%>%html_nodes(xpath ="//div[@class='zwcon']/h1") %>%html_text()
  
  #导出主体内容
  text<-web1%>%html_nodes(xpath ="//div/div[@class='zw']") %>%html_text()
  text1<-gsub("[^\u4e00-\u9fa5]|宋体","",text)
  
  content<-data.frame(title,text1)
  
  DAT<-rbind(DAT,content)
},
silent = T)

#543，收集到536条
write.csv(DAT,"c:/users/lenovo/desktop/textming/data/text data/text543.csv")

#---------------导出第二类---------------#
DAT<-data.frame()

pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed",
                       total = 447, clear = FALSE, width= 60)
for (i in 1:447) try({
  pb$tick()
  
  #解析网页
  web1<-read_html(url2[i],encoding="UTF-8",options = "HUGE")
  
  #导出标题
  title<-web1%>%html_nodes(xpath ="//div[@id='img-content']/h2") %>%html_text()
  title1<-gsub("[^\u4e00-\u9fa5]","",title)
  
  #导出主体内容
  text<-web1%>%html_nodes(xpath ="//div[@id='js_content']") %>%html_text()
  text1<-gsub("[^\u4e00-\u9fa5]","",text)
  
  content<-data.frame(title1,text1)
  
  DAT<-rbind(DAT,content)
},
silent = T)

write.csv(DAT,"c:/users/lenovo/desktop/textming/data/text data/text447.csv")

DAT1<-read.csv("c:/users/lenovo/desktop/textming/data/text data/text543.csv")
DAT1<-DAT1[,-1]

names(DAT1)<-c("title1","text1")
names(DAT)

data<-rbind(DAT,DAT1)

#983条

write.csv(data,"c:/users/lenovo/desktop/textming/data/text data/textdata.csv")











































