rm(list=ls())
library(rJava)
library(xlsx)    
library(plyr)
library(dplyr) 
library(dtplyr)
library(openxlsx)
#数据包络模型
library(lpSolveAPI)   
library(ucminf)
library(Benchmarking)
library(bootstrap)
library(TFDEA)

#=======================#
#=====先算17年效率======#
#=======================#
#all17中的时间和薪酬是压缩后的数据
all17<-read.csv("C:/Users/lenovo/Desktop/BALL/team-NBA/team17/all17.csv",1)
all17$team1<-gsub("[\u4e00-\u9fa5]+\\d+-\\d+","",all17$pk)#队伍的中文名称简写

#----------------------------------------------------------------------#
#比较个人效率，所以只截取个人有效数据  2376条                          #
#编译符好处：不生成中间变量                                            #
#f1<-filter(all17,all17$t15>0) #%>%mode()                       个人上场数据
#f1%>%as.matrix()%>%mode()                                             #
#f2<-do.call(cbind,f1)  #list格式转化成matrix然后可导出                #
#write.csv(f2,"C:/Users/lenovo/Desktop/BALL/team-NBA/team17/f1.csv")   #
#----------------------------------------------------------------------#


#考虑位点特征，即不上场，我也进行评价
f1<-all17
remove(all17)

minmaxscale<-function(a){
  center <- sweep(a, 2, apply(a, 2, min),'-') 
  R <- apply(a, 2, max) - apply(a,2,min)   
  #算出极差
  return(sweep(center, 2, R, "/")) 
}

#------------------------------#
#------第一步、处理时间--------#
#------------------------------#
#时间好取权重    先对原始数据取个人权重，再标准化计算
#时间：48*5=240
#data$e_f<-minmaxscale(as.data.frame(data$e_f))
w_t15<-f1$t15/240  
f1$t15<-minmaxscale(as.data.frame(f1$t15*w_t15))

#对标准化后的list格式进行转化
#f <- data.frame(matrix(unlist(f1$t15), nrow=2376, byrow=T),stringsAsFactors=FALSE)
f1$t15 = as.vector(unlist(f1$t15[1]))

#再对另外14个t加权，然后autoencoder
f1$t1<-f1$t1*w_t15
f1$t2<-f1$t2*w_t15
f1$t3<-f1$t3*w_t15
f1$t4<-f1$t4*w_t15
f1$t5<-f1$t5*w_t15
f1$t6<-f1$t6*w_t15
f1$t7<-f1$t7*w_t15
f1$t8<-f1$t8*w_t15
f1$t9<-f1$t9*w_t15
f1$t10<-f1$t10*w_t15
f1$t11<-f1$t11*w_t15
f1$t12<-f1$t12*w_t15
f1$t13<-f1$t13*w_t15
f1$t14<-f1$t14*w_t15

t14<-data.frame(f1$t1,f1$t2,f1$t3,f1$t4,f1$t5,f1$t6,f1$t7,f1$t8,f1$t9
                ,f1$t10,f1$t11,f1$t12,f1$t13,f1$t14)
write.csv(t14,"C:/Users/lenovo/Desktop/BALL/team-NBA/team17/t14.csv")

#------------------------------#
#------第二步、处理薪酬--------#
#------------------------------#
#1.先求17年15人的总薪酬

#2.截取30只队伍的总薪酬
# ss<-read.csv("C:/Users/lenovo/Desktop/BALL/team-NBA/salary_code16_17.csv",1)
# ss1<-filter(ss,ss$code16==1)
# ss2<-ss1[,c(1,9,10)]
# write.csv(ss2,"C:/Users/lenovo/Desktop/BALL/team-NBA/total_salary_30.csv")

#3.添加总薪酬变量
Tsalary<-read.csv("C:/Users/lenovo/Desktop/BALL/team-NBA/total_salary_30.csv")

for (i in 1:3936) {
  f1$total_salary[i]<-Tsalary$total_s17[Tsalary$team==f1$team1[i]]
}

#考虑薪酬的位点特征
#即不上场，但也考虑投入


#4.对15个人的薪酬取个人权重
w_s15<-f1$s15/f1$total_salary  #薪酬权重
f1$s15<-f1$s15*w_s15           #加权后的第15人薪酬

f1$s15<-minmaxscale(as.data.frame(f1$s15))
f1$s15<-as.vector(unlist(f1$s15[1]))



#5.对14人的薪酬取权后再压缩  *w_s15
f1$s1<-f1$s1*w_s15
f1$s2<-f1$s2*w_s15
f1$s3<-f1$s3*w_s15
f1$s4<-f1$s4*w_s15
f1$s5<-f1$s5*w_s15
f1$s6<-f1$s6*w_s15
f1$s7<-f1$s7*w_s15
f1$s8<-f1$s8*w_s15
f1$s9<-f1$s9*w_s15
f1$s10<-f1$s10*w_s15
f1$s11<-f1$s11*w_s15
f1$s12<-f1$s12*w_s15
f1$s13<-f1$s13*w_s15
f1$s14<-f1$s14*w_s15


#导出时间和薪酬进行autoencoder
s14<-data.frame(f1$s1,f1$s2,f1$s3,f1$s4,f1$s5,f1$s6,f1$s7,f1$s8,f1$s9
                ,f1$s10,f1$s11,f1$s12,f1$s13,f1$s14)
write.csv(s14,"C:/Users/lenovo/Desktop/BALL/team-NBA/team17/s14.csv")



#------------------------------------#
#------第三步、导入降维后的t、s------#
#------------------------------------#
#t14-t4 loss: 0.0124   0331120789051
t4<-read.csv("C:/Users/lenovo/Desktop/BALL/team-NBA/team17/t4.csv")
#s4    loss: 0.0041  77137743681669
s4<-read.csv("C:/Users/lenovo/Desktop/BALL/team-NBA/team17/s4.csv")

f1<-cbind(f1,t4)
f1<-cbind(f1,s4)

#--------------------------------#
#------第三步、中间变量预处理----#
#--------------------------------#
#即场上技术指标
#注意两点：1.分别取权重  2. 15人数据

# #前场篮板
# w_o_rebound<-f1$p_o_rebound/(2*f1$o_rebound)
# #15人的数据*第15人的权重  再乘1/2
# f1$o_rebound<-f1$o_rebound*w_o_rebound 
# #第15人的数据乘以自己的权重  再乘1/2
# f1$p_o_rebound<-f1$p_o_rebound*w_o_rebound
# 
# #后场篮板
# w_d_rebound<-f1$p_d_rebound/(2*f1$d_rebound)
# f1$d_rebound<-f1$d_rebound*w_d_rebound
# f1$p_d_rebound<-f1$p_d_rebound*w_d_rebound
# 
# #助攻
# w_assist<-f1$p_assist/(2*f1$assist)
# f1$assist<-f1$assist*w_assist
# f1$p_assist<-f1$p_assist*w_assist
# 
# #抢断
# w_steal<-f1$p_steal/(2*f1$steal)
# f1$steal<-f1$steal*w_steal
# f1$p_steal<-f1$p_steal*w_steal
# 
# #盖帽
# w_block<-f1$p_block/(2*f1$block)
# f1$block<-f1$block*w_block
# f1$p_block<-f1$p_block*w_block
# 
# #失误
# w_error<-f1$p_error/(2*f1$error)
# f1$error<-f1$error*w_error
# f1$p_error<-f1$p_error*w_error
# 
# #犯规
# w_foul<-f1$p_foul/(2*f1$foul)
# f1$foul<-f1$foul*w_foul
# f1$p_foul<-f1$p_foul*w_foul
# 
# #得分
# w_score<-f1$p_score/(2*f1$score)
# f1$score<-f1$score*w_score
# f1$p_score<-f1$p_score*w_score

#--------------------------------#
#------第三步、中间变量整合------#
#--------------------------------#

#进攻数据=得分+前场篮板*2+助攻*2    
f1$attack<-f1$score+f1$o_rebound*2+f1$assist*2    #15人
#which(is.na(f1$attack))  查看缺失值位置
f1$p_attack<-f1$p_score+f1$p_o_rebound*2+f1$p_assist*2  #第15人

w_a<-f1$p_attack/f1$attack  #进攻的权重

f1$attack<-minmaxscale(as.data.frame(f1$attack-f1$p_attack))*w_a  #14人
f1$attack<-as.vector(unlist(f1$attack[1]))        

f1$p_attack<-minmaxscale(as.data.frame(f1$p_attack))*w_a
f1$p_attack<-as.vector(unlist(f1$p_attack[1]))  

#防守数据=（抢断+盖帽+后场篮板）*2
f1$defend<-(f1$steal+f1$d_rebound+f1$block)*2
f1$p_defend<-(f1$p_steal+f1$p_block+f1$p_d_rebound)*2

w_d<-f1$p_defend/f1$defend

f1$defend<-minmaxscale(as.data.frame(f1$defend-f1$p_defend))*w_d
f1$defend<-as.vector(unlist(f1$defend[1]))

f1$p_defend<-minmaxscale(as.data.frame(f1$p_defend))*w_d
f1$p_defend<-as.vector(unlist(f1$p_defend[1]))

#失误和犯规=失误*2+犯规*1
f1$turnover<-f1$error*2+f1$foul
f1$p_turnover<-f1$p_error*2+f1$p_foul

w_tu<-f1$p_turnover/f1$turnover
  
f1$turnover<-minmaxscale(as.data.frame(f1$turnover-f1$p_turnover))*w_tu
f1$turnover<-as.vector(unlist(f1$turnover[1]))  

f1$p_turnover<-minmaxscale(as.data.frame(f1$p_turnover))*w_tu
f1$p_turnover<-as.vector(unlist(f1$p_turnover[1])) 

#--------------------------------#
#------第四步、产出变量处理------#
#--------------------------------#
f2<-f1
#分差，取1/15
#f2<-filter(f1,f1$outcome==1)     #1151条数据   只截取赢的场次
#remove(f1,s4,t4,w_assist,w_block,w_d_rebound,w_error,w_foul,
#       w_o_rebound,w_s15,w_score,w_steal)
#分差
f2$scoregap1<-minmaxscale(as.data.frame(f2$scoregap1))*w_t15
f2$scoregap1<-as.vector(unlist(f2$scoregap1[1]))

#对手进攻数据=得分+前场篮板*2+助攻*2
f2$vs_attack<-f2$vs_score+f2$vs_o_rebound*2+f2$vs_assist*2
f2$vs_attack<-minmaxscale(as.data.frame(f2$vs_attack))*w_t15
f2$vs_attack<-as.vector(unlist(f2$vs_attack[1]))

#对手防守数据=（抢断+盖帽+后场篮板）*2
f2$vs_defend<-(f2$vs_steal+f2$vs_block+f2$vs_d_rebound)*2
f2$vs_defend<-minmaxscale(as.data.frame(f2$vs_defend))*w_t15
f2$vs_defend<-as.vector(unlist(f2$vs_defend[1]))

#对手失误和犯规=失误*2+犯规*1
f2$vs_turnover<-f2$vs_error*2+f2$vs_foul
f2$vs_turnover<-minmaxscale(as.data.frame(f2$vs_turnover))*w_t15
f2$vs_turnover<-as.vector(unlist(f2$vs_turnover[1]))

# names(f2)
# aa<-f2[,67:77]
# aa<-as.data.frame(aa)
# aa<-do.call(cbind,aa)%>%as.data.frame()
# write.csv(aa,"C:/Users/lenovo/Desktop/BALL/team-NBA/team17/aa.csv")
#--------------------------------#
#------第五步、第一阶段效率------#
#--------------------------------#
#投入包括4个t、4个s，1个t，1个s，队伍的trunover 和 个人的turnover 12个变量
x<-data.frame(f2$sa,f2$sb,f2$sc,f2$sd,
              f2$ta,f2$tb,f2$tc,f2$td,
              f2$t15,f2$s15,
              f2$turnover,f2$p_turnover)
x<-as.matrix(x)
x[x==0]<-0.000001

z1<-data.frame(f2$attack,f2$p_attack,
               f2$defend,f2$p_defend)
z1<-as.matrix(z1)
z1[z1==0]<-0.000001

xz1<-data.frame(x,z1)   #第一阶段的投入产出的excel
write.csv(xz1,"C:/Users/lenovo/Desktop/BALL/team-NBA/team17/xz1.csv")

e1<-dea(x,z1,RTS="vrs",ORIENTATION="out")
e_a<-1/e1$eff
e_a
table(e_a==1) 


#---------------#
#---第二阶段----#
#---------------#
#投入包括自己的
z2<-data.frame(f2$attack,f2$p_attack,f2$defend,f2$p_defend,
               #f2$turnover,f2$p_turnover,
               f2$vs_attack,f2$vs_defend)
z2<-as.matrix(z2)
z2[z2==0]<-0.000001

y<-data.frame(f2$scoregap1,f2$vs_turnover)
y<-as.matrix(y)
y[y==0]<-0.000001

z2y<-data.frame(z2,y)     #第二阶段投入产出数据
write.csv(z2y,"C:/Users/lenovo/Desktop/BALL/team-NBA/team17/z2y.csv")

e2<-dea(z2,y,RTS="vrs", ORIENTATION="out")
# donation<-e2$lambda
# write.csv(donation,"C:/Users/lenovo/Desktop/BALL/au_dea/donation2.csv")

e_b<-1/e2$eff

e<-data.frame(f2$team,f2$time,f2$pk,e_a,e_b)




write.csv(e,"C:/Users/lenovo/Desktop/BALL/team-NBA/team17/e.csv")



#待做
#把球员对应到数据上面









#--------惩罚系数构造
# a<-seq(0.8,1,length.out = 30)
# write.csv(a,"C:/Users/lenovo/Desktop/BALL/au_dea/a.csv")

#----------要除的优化系数------------#
data$cost[data$team=="勇士"]<-1
data$cost[data$team=="马刺"]<-0.9931 
data$cost[data$team=="火箭"]<-0.9862 
data$cost[data$team=="凯尔特"]<-0.9793 
data$cost[data$team=="爵士"]<-0.9724 
data$cost[data$team=="猛龙"]<-0.9724 
data$cost[data$team=="骑士"]<-0.9724 
data$cost[data$team=="快船"]<-0.9724 
data$cost[data$team=="奇才"]<-0.9448 
data$cost[data$team=="雷霆"]<-0.9379 
data$cost[data$team=="灰熊"]<-0.9310 
data$cost[data$team=="老鹰"]<-0.9310 
data$cost[data$team=="步行者"]<-0.9172 
data$cost[data$team=="雄鹿"]<-0.9172 
data$cost[data$team=="公牛"]<-0.9034 
data$cost[data$team=="开拓者"]<-0.9034 
data$cost[data$team=="热火"]<-0.9034 
data$cost[data$team=="掘金"]<-0.8828 
data$cost[data$team=="活塞"]<-0.8759 
data$cost[data$team=="黄蜂"]<-0.8690 
data$cost[data$team=="鹈鹕"]<-0.8621 
data$cost[data$team=="小牛"]<-0.8552 
data$cost[data$team=="国王"]<-0.8483 
data$cost[data$team=="森林狼"]<-0.8414 
data$cost[data$team=="尼克斯"]<-0.8414 
data$cost[data$team=="魔术"]<-0.8276 
data$cost[data$team=="七六人"]<-0.8207 
data$cost[data$team=="湖人"]<-0.8138 
data$cost[data$team=="太阳"]<-0.8069 
data$cost[data$team=="篮网"]<-0.8000 

write.csv(data$cost,"C:/Users/lenovo/Desktop/BALL/au_dea/cost.csv")

#---截取选秀球员有效数据，求均值，除惩罚函数，画图
all<-read.csv("C:/Users/lenovo/Desktop/BALL/au_dea/all.csv")
names(all)
all<-all[1:1553,28:38]
all1<-filter(all,all$t15.1>0)

a<-rep(0,807)
for (i in 1:807) {
  if(all1$id[i]!=all1$id[i+1]){
    i<-i+1
    a[i]<-i
  }
}

all1<-cbind(all1,a)
write.csv(all1,"C:/Users/lenovo/Desktop/BALL/au_dea/all1.csv")
print(a)

apply(all1$e,2,mean)







#Sys.time() "2018-07-04 16:16:53 CST"
#--------------------------------------------------------------------------#
#-----------------------------对输的比赛的贡献-----------------------------#
#--------------------------------------------------------------------------#

data<-read.csv("C:/Users/lenovo/Desktop/BALL/au_dea/data.csv",1)

x1<-read.csv("C:/Users/lenovo/Desktop/BALL/au_dea/t4.csv",1)
names(x1)<-c("a","b",'c','d')
x2<-read.csv("C:/Users/lenovo/Desktop/BALL/au_dea/s4.csv",1)
names(x2)<-c('e','f','g','h')
data<-cbind(data,x1,x2)

data<-filter(data,data$outcome==0)
write.csv(data,"C:/Users/lenovo/Desktop/BALL/au_dea/data_lose.csv")

#--------惩罚系数构造
# a<-seq(0.8,1,length.out = 30)
# write.csv(a,"C:/Users/lenovo/Desktop/BALL/au_dea/a.csv")

#------------构造最小最大标准化函数-----------------#
data$e_f<-data$error*2+data$foul

minmaxscale<-function(a){
  center <- sweep(a, 2, apply(a, 2, min),'-') 
  R <- apply(a, 2, max) - apply(a,2,min)   
  #算出极差
  return(sweep(center, 2, R, "/")) 
}
#dim(as.data.frame(data$e_f))
data$e_f<-minmaxscale(as.data.frame(data$e_f))
data$t15<-minmaxscale(as.data.frame(data$t15))
data$s15<-minmaxscale(as.data.frame(data$s15)) 



x<-data.frame(data$a,data$b,data$c,data$d,data$t15,
              data$e,data$f,data$g,data$h,data$s15,data$e_f)
x[x==0]<-0.0001
x<-as.matrix(x)

data$attack<-data$score+data$assist*2
data$attack<-minmaxscale(as.data.frame(data$attack))

data$defend<-(data$steal+data$block+data$rebound)*2
data$defend<-minmaxscale(as.data.frame(data$defend))

z1<-data.frame(data$attack,data$defend)
z1[z1==0]<-0.0001
z1<-as.matrix(z1)    

xz1<-data.frame(x,z1)   #第一阶段的投入产出的excel
write.csv(xz1,"C:/Users/lenovo/Desktop/BALL/au_dea/xz1_lose.csv")

e1<-dea(x,z1,RTS="vrs",ORIENTATION="out")

e<-1/e1$eff
e
e<-as.data.frame(e)
table(e)  
table(e==1) 

write.csv(e,"C:/Users/lenovo/Desktop/BALL/au_dea/e1_lose.csv") 


#---------------#
#---第二阶段----#
#---------------#
data$vs_attack<-data$vs_score +data$vs_assist*2
data$vs_attack<-minmaxscale(as.data.frame(data$vs_attack))

data$vs_defend<-(data$vs_steal+data$vs_block+data$vs_rebound)*2
data$vs_defend<-minmaxscale(as.data.frame(data$vs_defend))

z2<-data.frame(data$attack,data$defend,data$e_f,data$vs_attack,data$vs_defend)
z2<-as.matrix(z2)
z2[z2==0]<-0.0001

data$vs_e_f<-data$vs_error*2+data$vs_foul
data$vs_e_f<-minmaxscale(as.data.frame(data$vs_e_f))

#对输场的分差进行处理
#data$scoregap<-data$scoregap-min(data$scoregap)
data$scoregap<-minmaxscale(as.data.frame(data$scoregap))

y<-data.frame(data$scoregap,data$vs_e_f)
y<-as.matrix(y)
y[y==0]<-0.0001

z2y<-data.frame(z2,y)     #第二阶段投入产出数据
write.csv(z2y,"C:/Users/lenovo/Desktop/BALL/au_dea/z2y_lose.csv")

e2<-dea(z2,y,RTS="vrs", ORIENTATION="out")


eff<-1/e2$eff
eff
write.csv(eff,"C:/Users/lenovo/Desktop/BALL/au_dea/e2_lose.csv")

#----------要除的惩罚系数------------#
data$cost[data$team=="勇士"]<-1
data$cost[data$team=="马刺"]<-0.9931 
data$cost[data$team=="火箭"]<-0.9862 
data$cost[data$team=="凯尔特"]<-0.9793 
data$cost[data$team=="爵士"]<-0.9724 
data$cost[data$team=="猛龙"]<-0.9724 
data$cost[data$team=="骑士"]<-0.9724 
data$cost[data$team=="快船"]<-0.9724 
data$cost[data$team=="奇才"]<-0.9448 
data$cost[data$team=="雷霆"]<-0.9379 
data$cost[data$team=="灰熊"]<-0.9310 
data$cost[data$team=="老鹰"]<-0.9310 
data$cost[data$team=="步行者"]<-0.9172 
data$cost[data$team=="雄鹿"]<-0.9172 
data$cost[data$team=="公牛"]<-0.9034 
data$cost[data$team=="开拓者"]<-0.9034 
data$cost[data$team=="热火"]<-0.9034 
data$cost[data$team=="掘金"]<-0.8828 
data$cost[data$team=="活塞"]<-0.8759 
data$cost[data$team=="黄蜂"]<-0.8690 
data$cost[data$team=="鹈鹕"]<-0.8621 
data$cost[data$team=="小牛"]<-0.8552 
data$cost[data$team=="国王"]<-0.8483 
data$cost[data$team=="森林狼"]<-0.8414 
data$cost[data$team=="尼克斯"]<-0.8414 
data$cost[data$team=="魔术"]<-0.8276 
data$cost[data$team=="七六人"]<-0.8207 
data$cost[data$team=="湖人"]<-0.8138 
data$cost[data$team=="太阳"]<-0.8069 
data$cost[data$team=="篮网"]<-0.8000 

write.csv(data$cost,"C:/Users/lenovo/Desktop/BALL/au_dea/cost_lose.csv")


#---生成all_lose1
all_lose<-read.xlsx("C:/Users/lenovo/Desktop/BALL/au_dea/all_lose.xlsx")
all_lose1<-all_lose[1:1646,]
all_lose1<-filter(all_lose1,all_lose1$t15>0)
write.csv(all_lose1,"C:/Users/lenovo/Desktop/BALL/au_dea/all_lose1.csv")




























































