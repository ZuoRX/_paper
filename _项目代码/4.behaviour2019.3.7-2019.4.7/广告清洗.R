library(plyr)
library(data.table)
library(magrittr)  #管道符


ad_feature<-read.csv("e:/text/data/raw/ad_feature.csv")

order(table(ad_feature$cate_id))
max(table(ad_feature$cate_id))   #42691


table(ad_feature$customer)#总共25.6万广告主
max(table(ad_feature$customer)) #最多一个广告主投了1653条广告

#----------------------#
#---分组累计次数标记---#
#----------------------#

#标记每个广告主多个商品的分别累积广告次数
# a<-c(0,diff(ad_feature$cate_id))#在最开始额外添加了一个0
# a[which(a!=0)]<-1
# cumsum(a)

ad_feature$cate_id_n<-0L
dt<-data.table(ad_feature$cate_id,ad_feature$cate_id_n)
names(dt)<-c("group","n")
dt[,diff:=c(0,diff(group))]#添加一个列变量
dt$diff[which(dt$diff!=0)]<-1L
dt[,cumsum:=cumsum(diff)]
dt[,n:=seq(.N),by=cumsum]
ad_feature$cate_id_n<-dt$n


#标记每个广告主总广告次数
ad_feature$customer_n<-0L
dt<-data.table(ad_feature$customer,ad_feature$customer_n)
names(dt)<-c("group","n")
dt[,diff:=c(0,diff(group))]#添加一个列变量
dt$diff[which(dt$diff!=0)]<-1L
# dt[,cumsum:=cumsum(diff)]
# dt[,n:=seq(.N),by=cumsum]
dt[,n:=seq(.N),by=list(cumsum(diff))]
ad_feature$customer_n<-dt$n

ad<-ad_feature[,c(1,2,4,6,7,8)]
write.csv(ad,"e:/text/data/process/ad.csv")


#----------#
#---定位---#
#----------#
ad<-read.csv("e:/text/data/process/ad.csv")
#定位广告最多的商品id
max(table(ad$cate_id)) #42691

#同一广告主对某一商品发布最多广告数
max(ad$cate_id_n)  #236
ad[which(ad$cate_id_n==236),]
ad_sample<-ad[which(ad$customer==56049),]
write.csv(ad_sample,"e:/text/data/process/ad_sample.csv")

# #广告主发布的最多广告
# max(ad$customer_n)  #1653


#=========================================================================#

######连接一个广告id    581044

#找到一个点击最多的广告

#----------------------------#
#---raw_sample10.csv文件夹---#
#----------------------------#
raw_sample0<-read.csv("e:/text/data/raw/raw_sample/raw_sample10.csv")
head(raw_sample0,100)

raw_sample0$ad_n<-0L
dt<-data.table(raw_sample0$adgroup_id,raw_sample0$ad_n)
names(dt)<-c("group","n")
dt[,diff:=c(0,diff(group))]#添加一个列变量
dt$diff[which(dt$diff!=0)]<-1L
dt[,n:=seq(.N),by=list(cumsum(diff))]
#head(dt,100)
raw_sample0$ad_n<-dt$n

max(dt$n)
#找出广告id
raw_sample0[which(raw_sample0$ad_n==17301),]
#根据广告id定位user等信息
ad1<-raw_sample0[which(raw_sample0$adgroup_id==659211),]
write.csv(ad1,"e:/text/data/process/ad1.csv")


#----------------------------#
#---raw_sample11.csv文件夹---#
#----------------------------#
raw_sample0<-read.csv("e:/text/data/raw/raw_sample/raw_sample11.csv")
head(raw_sample0,100)

raw_sample0$ad_n<-0L
dt<-data.table(raw_sample0$adgroup_id,raw_sample0$ad_n)
names(dt)<-c("group","n")
dt[,diff:=c(0,diff(group))]#添加一个列变量
dt$diff[which(dt$diff!=0)]<-1L
dt[,n:=seq(.N),by=list(cumsum(diff))]
#head(dt,100)
raw_sample0$ad_n<-dt$n

max(dt$n)
#找出广告id
raw_sample0[which(raw_sample0$ad_n==70929),]
#根据广告id定位user等信息
ad2<-raw_sample0[which(raw_sample0$adgroup_id==710164),]
write.csv(ad2,"e:/text/data/process/ad2.csv")

#----------------------------#
#---raw_sample12.csv文件夹---#
#----------------------------#
raw_sample0<-read.csv("e:/text/data/raw/raw_sample/raw_sample12.csv")
head(raw_sample0)

raw_sample0$ad_n<-0L
dt<-data.table(raw_sample0$adgroup_id,raw_sample0$ad_n)
names(dt)<-c("group","n")
dt[,diff:=c(0,diff(group))]#添加一个列变量
dt$diff[which(dt$diff!=0)]<-1L
dt[,n:=seq(.N),by=list(cumsum(diff))]
#head(dt,100)
raw_sample0$ad_n<-dt$n

max(dt$n)
#找出广告id
raw_sample0[which(raw_sample0$ad_n==25585),]
#根据广告id定位user等信息
ad3<-raw_sample0[which(raw_sample0$adgroup_id==725105),]
write.csv(ad3,"e:/text/data/process/ad3.csv")
#         user    time_stamp adgroup_id    pid       nonclk clk  ad_n
# 3079856 114312 1494504092     725105 430548_1007      1   0 25585


#----------------------------#
#---raw_sample13.csv文件夹---#
#----------------------------#
raw_sample0<-read.csv("e:/text/data/raw/raw_sample/raw_sample13.csv")
head(raw_sample0)

raw_sample0$ad_n<-0L
dt<-data.table(raw_sample0$adgroup_id,raw_sample0$ad_n)
names(dt)<-c("group","n")
dt[,diff:=c(0,diff(group))]#添加一个列变量
dt$diff[which(dt$diff!=0)]<-1L
dt[,n:=seq(.N),by=list(cumsum(diff))]
#head(dt,100)
raw_sample0$ad_n<-dt$n

max(dt$n)
#找出广告id
raw_sample0[which(raw_sample0$ad_n==23516),]
#根据广告id定位user等信息
ad4<-raw_sample0[which(raw_sample0$adgroup_id==836889),]
write.csv(ad4,"e:/text/data/process/ad4.csv")
#          user time_stamp adgroup_id         pid    nonclk clk  ad_n
# 6558496 169072 1494611822     836889 430548_1007      1   0 23516



#=========================================================================#
# 2017/5/13 23:59
# 2017/5/6 0:00

as.numeric(as.POSIXct("2017-5-13 23:59"))  #1494691140
as.numeric(as.POSIXct("2017-5-6 0:00:00")) #1494000000
as.numeric(as.POSIXct("2017-4-23 0:00:00")) #1494000000
#behavour log时间戳转化

behavour_log<-read.csv("e:/text/data/raw/behavior_log/behavior_log10.csv")
head(behavour_log)


behavour_log$time_stamp<-as.character(as.POSIXlt(behavour_log$time_stamp, 
                                                 origin="1970-01-01"))
# Error: memory exhausted (limit reached?)
# Error during wrapup: memory exhausted (limit reached?)

#=========================================================================#

#对ad2进行分析

ad2<-read.csv("e:/text/data/process/ad2.csv")
head(ad2)

#先转化为时间戳
ad2$time<-as.numeric(as.POSIXct(ad2$time))

#先排序user,再时间
ad4<-arrange(ad2,user,time)

#把时间戳转化为北京时间
ad4$time<-as.character(as.POSIXlt(ad4$time,origin="1970-01-01"))


ad5<-ad4[!duplicated(ad4$user),]  #无重复的用户id 51516个

write.csv(ad5,"e:/text/data/process/ad3.csv")

#=========================================================================#

#behavour log时间戳转化

behavour_log<-read.csv("p:/behavior_logz11.csv")

#-----------------------------#
#------第一步：筛选数据戳-----#
#-----------------------------#
# as.numeric(as.POSIXct("2017-5-13 23:59:59"))  #1494691199
# #推后7天
# as.numeric(as.POSIXct("2017-5-20 23:59:59"))  #1495295999
# as.numeric(as.POSIXct("2017-5-6 0:00:00"))    #1494000000

#区间筛选
behavour_log1<-behavour_log[which(1494000000<behavour_log$time_stamp & behavour_log$time_stamp<1495295999),]

# head(behavour_log1)

# #时间戳转化北京时间
# behavour_log1$time_stamp<-as.character(as.POSIXlt(behavour_log1$time_stamp, 
#                                                  origin="1970-01-01"))
# # max(behavour_log1$time_stamp)
# # min(behavour_log$time_stamp)
# 
# #按时间降序排列
# behavour_log1<-arrange(behavour_log1,desc(time_stamp))

#-----------------------------#
#-----第二步：筛选user id-----#
#-----------------------------#
#library(plyr)
ad<-read.csv("p:ad3.csv")

behavour_log2<-match_df(behavour_log1,ad,on="user")

#-----------------------------#
#----第三步：精准商品行为-----#
#-----------------------------#

#浏览商品id 1665  cate
behavour_log4<-behavour_log2[which(behavour_log2$cate==1665),]
head(behavour_log4)

#时间戳转化北京时间
behavour_log4$time_stamp<-as.character(as.POSIXlt(behavour_log4$time_stamp,
                                                  origin="1970-01-01"))
head(behavour_log4)
#要求行为发生在最早看到广告之后

write.csv(behavour_log4,"p:/process/behavour2.csv")


#


#-----------------------------#
#----第四步：精准时间行为-----#待处理
#-----------------------------#


#先排序
behavour_log3<-arrange(behavour_log2,user,time_stamp)
head(behavour_log3,10)


###




#=================================================#
#======5.6-5.13日广告浏览与商品行为时间处理=======#
#=================================================#
#假设：商品行为发生在5.6日之后，且未点击广告，被认为受广告间接影响
#假设：商品行为发生在广告点击之后，受广告直接影响
#假设：同一商品的三条广告，统一处理

#但3条广告，价格不一样
# adgroup_id	cate_id	customer	price	
# 815744	       1665	60305	    129	                   3百+
# 710164	       1665	60305	    109	                   7万+
# 632866	       1665	60305	    99	                   3千+

#=========三组核心数据==========#

# #不同用户对同一商品的行为   在 5.6-5.13日的行为
# user	time_stamp	btag	cate	brand
# 325135	2017/5/13 23:59	pv	1665	186516
# 325135	2017/5/13 23:59	pv	1665	186516

#核心一
user_profile<-read.csv("e:/text/data/process/user_profile1.csv")
names(user_profile)[1]<-"user"

#核心二
behaviour_ad<-read.csv("e:/text/data/process/behaviour_ad.csv")
#合并用户特征
behaviour_ad1<-join(behaviour_ad,user_profile,by="user")
# head(behaviour_ad1)
 sum(is.na(behaviour_ad1$gender1))  #1.2万缺失值
behaviour_ad1[is.na(behaviour_ad1)]<-0
write.csv(behaviour_ad1,"e:/text/data/process/behaviour_ad_user.csv",row.names = F)

#核心三
#浏览或点击最多的1+2条广告信息  总计70929条数据
ad<-read.csv("e:/text/data/process/ad2.csv")
names(ad)[2]<-"time_stamp"
add<-read.csv("e:/text/data/process/ad32.csv")
ad<-rbind(ad,add)

remove(add)

behaviour_ad<-read.csv("e:/text/data/process/behaviour_ad_user.csv")

#=================================#
#======不区分广告的商品行为=======#
#=================================#
#把behaviour_ad中的广告点击user挑选出来
b<-match_df(behaviour_ad,ad,on="user")
#??????????????????????????#该商品有n多品牌  
#假定不受品牌影响

#降序排列
b$time_stamp<-as.numeric(as.POSIXct(b$time_stamp))
b<-arrange(b,desc(time_stamp))

head(b)
#每5分钟收集一次
b$time<-b$time_stamp/60-24900000

#取整数
b$label<-b$time%/%5

# #四种商品行为分别挑选出来进行统计
# b_simple<-b[,c(2,3,6)]

mytable<-xtabs(~label+btag,data = b)#%>%as.matrix()

#构建多个二维列联表
mytable_gender<-xtabs(~label+gender1,data = b)
mytable_age<-xtabs(~label+age1,data = b)
mytable_shopping<-xtabs(~label+shopping1,data = b)
mytable_occupation<-xtabs(~label+occupation1,data = b)

mytable1<-cbind(mytable,mytable_gender,mytable_age,
                mytable_occupation,mytable_shopping)
write.csv(mytable1,"e:/text/data/process/mytable.csv")

#初步输入数据
table_all<-read.csv("e:/text/data/process/mytable.csv")
table_all<-table_all[,c(-6,-9,-18)]
names(table_all)<-c("time","buy","cart","fav","pv",
                    "gender1","gender2",
                    "age1","age2","age3","age4","age5","age6",
                    "student0","student1",
                    "city1","city2","city3")


n<-as.data.frame(2303:0)
names(n)<-"time"

table_all1<-join(n,table_all,by="time")
table_all1[is.na(table_all1)]<-0

write.csv(table_all1,"e:/text/data/process/table_all_user_1665.csv")

#==============================#
#======点击广告+商品行为=======#
#==============================#

ad_clk<-ad[which(ad$clk==1),]

#区分广告位
ad_clk1<-ad_clk[which(ad_clk$pid=="430548_1007"),]
ad_clk2<-ad_clk[which(ad_clk$pid=="430539_1007"),]

#------------#
#---ad_clk---#
#------------#
#把behaviour_ad中的广告点击user挑选出来
b<-match_df(behaviour_ad,ad_clk,on="user")
#??????????????????????????#该商品有n多品牌  
#假定不受品牌影响

#降序排列
b$time_stamp<-as.numeric(as.POSIXct(b$time_stamp))
b<-arrange(b,desc(time_stamp))

head(b)
#每5分钟收集一次
b$time<-b$time_stamp/60-24900000

#取整数
b$label<-b$time%/%5

# #四种商品行为分别挑选出来进行统计
# b_simple<-b[,c(2,3,6)]

mytable<-xtabs(~label+btag,data = b)#%>%as.matrix()

#构建多个二维列联表
mytable_gender<-xtabs(~label+gender1,data = b)
mytable_age<-xtabs(~label+age1,data = b)
mytable_shopping<-xtabs(~label+shopping1,data = b)
mytable_occupation<-xtabs(~label+occupation1,data = b)

mytable1<-cbind(mytable,mytable_gender,mytable_age,
                mytable_occupation,mytable_shopping)
write.csv(mytable1,"e:/text/data/process/mytable.csv")

#初步输入数据
table_all<-read.csv("e:/text/data/process/mytable.csv")
table_all<-table_all[,c(-6,-9,-18)]
names(table_all)<-c("time","buy","cart","fav","pv",
                    "gender1","gender2",
                    "age1","age2","age3","age4","age5","age6",
                    "student0","student1",
                    "city1","city2","city3")


n<-as.data.frame(2303:0)
names(n)<-"time"

table_all1<-join(n,table_all,by="time")
table_all1[is.na(table_all1)]<-0

write.csv(table_all1,"e:/text/data/process/table_all_user.csv")
#连接客户商品行为+客户特征+商品特征

#-------------#
#---ad_clk1---#
#-------------#

#把behaviour_ad中点击广告位1的user挑选出来
b<-match_df(behaviour_ad,ad_clk1,on="user")

#降序排列
b$time_stamp<-as.numeric(as.POSIXct(b$time_stamp))
b<-arrange(b,desc(time_stamp))

#每5分钟收集一次
b$time<-b$time_stamp/60-24900000

#取整数
b$label<-b$time%/%5

mytable<-xtabs(~label+btag,data = b)
write.csv(mytable,"e:/text/data/process/mytable.csv")

table_1<-read.csv("e:/text/data/process/mytable.csv")
names(table_1)[1]<-"time"

n<-as.data.frame(2303:0)
names(n)<-"time"

table_1<-join(n,table_1,by="time")
table_1[is.na(table_1)]<-0

write.csv(table_1,"e:/text/data/process/table_1.csv")

#-------------#
#---ad_clk2---#
#-------------#

#把behaviour_ad中点击广告位1的user挑选出来
b<-match_df(behaviour_ad,ad_clk2,on="user")

#降序排列
b$time_stamp<-as.numeric(as.POSIXct(b$time_stamp))
b<-arrange(b,desc(time_stamp))

#每5分钟收集一次
b$time<-b$time_stamp/60-24900000

#取整数
b$label<-b$time%/%5

mytable<-xtabs(~label+btag,data = b)
write.csv(mytable,"e:/text/data/process/mytable.csv")

table_2<-read.csv("e:/text/data/process/mytable.csv")
names(table_2)[1]<-"time"

n<-as.data.frame(2303:0)
names(n)<-"time"

table_2<-join(n,table_2,by="time")
table_2[is.na(table_2)]<-0

write.csv(table_2,"e:/text/data/process/table_2.csv")



#================================#
#======未点击广告+商品行为=======#
#================================#
#假定：未点击广告，但浏览过广告，广告产生了有效宣传效应

ad_nonclk<-ad[which(ad$clk==0),]

#把behaviour_ad中点击广告位1的user挑选出来
b<-match_df(behaviour_ad,ad_nonclk,on="user")

#降序排列
b$time_stamp<-as.numeric(as.POSIXct(b$time_stamp))
b<-arrange(b,desc(time_stamp))

#每5分钟收集一次
b$time<-b$time_stamp/60-24900000

#取整数
b$label<-b$time%/%5

mytable<-xtabs(~label+btag,data = b)

#构建多个二维列联表
mytable_gender<-xtabs(~label+gender1,data = b)
mytable_age<-xtabs(~label+age1,data = b)
mytable_shopping<-xtabs(~label+shopping1,data = b)
mytable_occupation<-xtabs(~label+occupation1,data = b)

mytable1<-cbind(mytable,mytable_gender,mytable_age,
                mytable_occupation,mytable_shopping)
write.csv(mytable1,"e:/text/data/process/mytable.csv")

#初步输入数据
table_all<-read.csv("e:/text/data/process/mytable.csv")
table_all<-table_all[,c(-6,-9,-18)]
names(table_all)<-c("time","buy","cart","fav","pv",
                    "gender1","gender2",
                    "age1","age2","age3","age4","age5","age6",
                    "student0","student1",
                    "city1","city2","city3")


n<-as.data.frame(2303:0)
names(n)<-"time"

table_all1<-join(n,table_all,by="time")
table_all1[is.na(table_all1)]<-0

write.csv(table_all1,"e:/text/data/process/table_nonclk_user.csv")



#=============================================================================#

#====================================#
#====user_profile格式特征格式转换====#  106.2万数据
#====================================#

# user_id	用户id
                             # cms_seg_id	微群id
                             # cms_group_id	
# gender	1男2女
# age_lever	年龄层次
# pvalue_lever	消费档次
# 1低档2中档3高档
# shopping level	购物深度
# 1浅层用户
# 2 中度用户
# 3 深度用户
# occupation	是否大学生
# 1是0否
# new_user_	城市层级

user<-read.csv("e:text/data/raw/user_profile.csv")
sum(is.na(user))
#$pvalue_level  568734
#$city_level    340718

head(user1)

user1<-user[,c(1,4,5,7,8)]
names(user1)<-c("user_id","gender1","age1","shopping1","occupation1")

#修改性别变量
user1$gender2<-user1$gender1
user1$gender1[user1$gender1!=1]<-0

user1$gender2[user1$gender2!=2]<-0
user1$gender2[user1$gender2==2]<-1

#修改年龄变量
user1$age2<-0L
user1$age3<-0L
user1$age4<-0L
user1$age5<-0L
user1$age6<-0L

user1$age2[which(user1$age1==2)]<-1
user1$age3[which(user1$age1==3)]<-1
user1$age4[which(user1$age1==4)]<-1
user1$age5[which(user1$age1==5)]<-1
user1$age6[which(user1$age1==6)]<-1

user1$age1[user1$age1!=1]<-0

#购物深度变量
user1$shopping2<-0L
user1$shopping3<-0L

user1$shopping2[which(user1$shopping1==2)]<-1
user1$shopping3[which(user1$shopping1==3)]<-1

user1$shopping1[user1$shopping1!=1]<-0

#大学生与非大学生
user1$occupation2<-0L

user1$occupation2[which(user1$occupation1==0)]<-1

write.csv(user1,"e:text/data/process/user_profile1.csv",row.names = F)


#=============================================================================#

#====================================#
#====user_profiley与behaviour匹配====#
#====================================#


a6216<-read.csv("e:/text/data/goods1/behavour_user6216day23.csv")
head(a6216)
pv6216<-sum(a6216$pv)
cart6216<-sum(a6216$cart)
fav6216<-sum(a6216$fav)
buy6216<-sum(a6216$buy)
b1<-data.frame(pv6216,cart6216,fav6216,buy6216)
names(b1)<-c("pv","cart","fav","buy")

a6423<-read.csv("e:/text/data/goods2/b_user6423day23.csv")
head(a6423)
pv6423<-sum(a6423$pv)
cart6423<-sum(a6423$cart)
fav6423<-sum(a6423$fav)
buy6423<-sum(a6423$buy)
b2<-data.frame(pv6423,cart6423,fav6423,buy6423)
names(b2)<-c("pv","cart","fav","buy")


a4520<-read.csv("e:/text/data/goods3/b_user4520day23.csv")
head(a4520)
pv4520<-sum(a4520$pv)
cart4520<-sum(a4520$cart)
fav4520<-sum(a4520$fav)
buy4520<-sum(a4520$buy)
b3<-data.frame(pv4520,cart4520,fav4520,buy4520)
names(b3)<-c("pv","cart","fav","buy")



a6421<-read.csv("e:/text/data/goods4/b_user6421day23.csv")
head(a6421)
pv6421<-sum(a6421$pv)
cart6421<-sum(a6421$cart)
fav6421<-sum(a6421$fav)
buy6421<-sum(a6421$buy)
b4<-data.frame(pv6421,cart6421,fav6421,buy6421)
names(b4)<-c("pv","cart","fav","buy")

b<-rbind(b1,b3,b2,b4)
write.csv(b,"c:/users/lenovo/desktop/behaviour4sum.csv")





















