library(bitops)
library(RCurl)
library(XML)
library(xlsx)
library(dplyr)
library(readr)
library(stringi)
library(rvest)
library(tcltk)
library(progress)
#动态爬虫
library(RJSONIO)
library(Rwebdriver)
library(rdom)
library(binman)
library(wdman)
library("RSelenium", lib.loc="D:/R-3.5.1/library")
cat("\014") 



#http://quotes.money.163.com/trade/lszjlx_000708.0.html

#-----------------------------------#
#----------历史资金流向爬爬---------#
#-----------------------------------#

#-----1---000708大冶特钢-----#

#第一步，构造循环url
urllist<-1:26 
i=0
for (page in 0:25) {
    i=i+1
    urllist[i]<-paste("http://quotes.money.163.com/trade/lszjlx_000708," ,
                      page,".html",sep="")
}
urllist

#第二步，循环
pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed",
                       total = 26, clear = FALSE, width= 60)
DAT<-data.frame()

for (j in 1:length(urllist)) try({
  pb$tick()
 
web1<-read_html(urllist[[j]],encoding="utf-8",options = "HUGE")

date<-web1%>%html_nodes(xpath ="//div[@class='inner_box']//tr/td[1]") %>%html_text()

close<-web1%>%html_nodes(xpath ="//div[@class='inner_box']//tr/td[2]") %>%html_text()
close1<-gsub("\r\n| ","",close)

fluctuate<-web1%>%html_nodes(xpath ="//div[@class='inner_box']//tr/td[3]") %>%
           html_text()

capital_in<-web1%>%html_nodes(xpath ="//div[@class='inner_box']//tr/td[5]") %>%
            html_text()

capital_out<-web1%>%html_nodes(xpath ="//div[@class='inner_box']//tr/td[6]") %>%
             html_text()

net_in<-web1%>%html_nodes(xpath ="//div[@class='inner_box']//tr/td[7]") %>%
        html_text()

data<-data.frame(date,close1,fluctuate,capital_in,capital_out,net_in)
#DAT2<-DAT2[!duplicated(DAT2[,1]),]   #去除时间里面的重复数据
DAT<-rbind(DAT,data)

},
silent = T)

write.csv(DAT,"C:/users/lenovo/desktop/stock/steel1/1.csv")





































