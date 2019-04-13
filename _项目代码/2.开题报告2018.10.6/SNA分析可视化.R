

library(igraph)

t<-read.graph("c:/users/lenovo/desktop/textming/data/text data/demo.net",format = "pajek")
#计算边中心度
edge.betweenness(t, e=E(t))
#计算点中心度
betweenness(t, v = V(t))








#----------------------#
#---单向网络操作demo---#
#----------------------#

from<-c("收藏夹书签","微博微信等社交客户端","第三方广告","APP","搜索引擎","进入","付费视频")
to<-c("进入","进入","进入","进入","进入","付费视频","进入")
freq<-c(1071,759,418,267,252,547,445)

data<-data.frame(from,to,freq)
data_stru<-graph.data.frame(data)   #excel格式转换成igraph可读格式
plot(data_stru)

betweenness(data_stru, v = V(data_stru))


















































