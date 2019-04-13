library(Rlibeemd)
library(gtools)
library(xts)
library(plyr)
library(dplyr) 
library(dtplyr)
library(vmd)

se<-read.csv("C:/Users/lenovo/Desktop/stock/steel/super_e1.csv")
se[se==Inf]<-500

apply(se,2,max)

filenames<-list.files("C:/Users/lenovo/Desktop/stock/steel/data")


for (i in 1:29) {
url<-paste("C:/Users/lenovo/Desktop/stock/steel/data/",filenames[i],sep = "") 

a1<-read.csv(url)

a2<-cbind(a1,se[,i+1])
head(a2)
a3<-a2[,c(3:7,11)]
names(a3)[6]<-"e"
a4<-a3[!duplicated(a3[,c(1:5)]),]
a5<-a4[,-6]

url1<-paste("C:/Users/lenovo/Desktop/stock/steel/29data/","raw",filenames[i],sep = "") 
write.csv(a5,url1)
}























