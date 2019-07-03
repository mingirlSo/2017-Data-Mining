#####0526변수추가#####

rm(list=ls())
library(xlsx)
train_profiles<-read.xlsx("train_profiles.xlsx",1)
train_profiles$CUS_ID<-as.numeric(as.character(train_profiles$CUS_ID))
train_profiles<-train_profiles[order(train_profiles$CUS_ID,decreasing = F),]
tp<-train_profiles
library(data.table)
train_clickstreams<-fread("train_clickstreams.tab");train_clickstreams[,CUS_ID:= as.numeric(CUS_ID)]
tr<-train_clickstreams
test_clickstreams<-fread("test_clickstreams.tab");test_clickstreams[,CUS_ID:=as.numeric(CUS_ID)]
tr2<-test_clickstreams

train_final<-read.csv("train_final.csv")
test_final<-read.csv("test_final.csv")

library("dplyr")
library("reshape")
detach(package:plyr)
####TRAIN####
n0<-cast(tr,CUS_ID~MACT_NM,length,value="SITE_CNT")
n0[,-1]<-n0[,-1]/rowSums(n0[,-1])*100
colnames(n0)[-1]<-paste("CMP",1:(dim(n0)[2]-1),sep="")


n1<-cast(tr[tr$BACT_NM=="뉴스/미디어",],CUS_ID~ACT_NM,length,value="ST_TIME")
n1[,-1]<-n1[,-1]/rowSums(n1[,-1])*100
names(n1)[-1]<-paste("newA",1:(dim(n1)[2]-1),sep="")


n2<-cast(tr[tr$BACT_NM=="쇼핑",],CUS_ID~ACT_NM,length,value="ST_TIME")
n2[,-1]<-n2[,-1]/rowSums(n2[,-1])*100
names(n2)[-1]<-paste("shopA",1:(dim(n2)[2]-1),sep="")

train_final<- train_final %>% left_join(n0) %>% left_join(n1) %>% left_join(n2)
train_final[is.na(train_final)]<-0
write.csv(train_final,"train_final_170526.csv",row.names=F)

####TEST####
n0<-cast(tr2,CUS_ID~MACT_NM,length,value="SITE_CNT")
n0[,-1]<-n0[,-1]/rowSums(n0[,-1])*100
colnames(n0)[-1]<-paste("CMP",1:(dim(n0)[2]-1),sep="")


n1<-cast(tr2[tr2$BACT_NM=="뉴스/미디어",],CUS_ID~ACT_NM,length,value="ST_TIME")
n1[,-1]<-n1[,-1]/rowSums(n1[,-1])*100
names(n1)[-1]<-paste("newA",1:(dim(n1)[2]-1),sep="")


n2<-cast(tr2[tr2$BACT_NM=="쇼핑",],CUS_ID~ACT_NM,length,value="ST_TIME")
n2[,-1]<-n2[,-1]/rowSums(n2[,-1])*100
names(n2)[-1]<-paste("shopA",1:(dim(n2)[2]-1),sep="")

test_final<- test_final %>% left_join(n0) %>% left_join(n1) %>% left_join(n2)
test_final[is.na(test_final)]<-0
write.csv(test_final,"test_final_170526.csv",row.names=F)

######0528변수추가XXXXXXXXX#### 
rm(list=ls())
library(xlsx)
train_profiles<-read.xlsx("train_profiles.xlsx",1)
train_profiles$CUS_ID<-as.numeric(as.character(train_profiles$CUS_ID))
train_profiles<-train_profiles[order(train_profiles$CUS_ID,decreasing = F),]
tp<-train_profiles

library(data.table)
train_clickstreams<-fread("train_clickstreams.tab");train_clickstreams[,CUS_ID:= as.numeric(CUS_ID)]
tr<-train_clickstreams
test_clickstreams<-fread("test_clickstreams.tab");test_clickstreams[,CUS_ID:=as.numeric(CUS_ID)]
tr2<-test_clickstreams
mg<-merge(tp,tr,by="CUS_ID")

train_final<-read.csv("train_final_170526.csv")
test_final<-read.csv("test_final_170526.csv")

library(dplyr)
cate<-data.frame(CUS_ID=1:2500)
c1<- mg %>% filter(BACT_NM=="게임") %>% group_by(CUS_ID,SITE_NM) %>%  summarise(SITE5=n())
c11<-aggregate(c1[3],by=list(CUS_ID=c1$CUS_ID),max)
summary(c11$SITE5)
c12<-c11[!c11$SITE5<=mean(c11$SITE5),]
am<-merge(c12,c1,by.x=c("CUS_ID","SITE5"),by.y = c("CUS_ID","SITE5"),all=F,all.x=T,all.y=F)
dim(table(am$SITE_NM))
am<-am[order(am$CUS_ID),]
rownames(am)<-1:dim(am)[1]
am<-am[!duplicated(am$CUS_ID),]
am<-am[,-2]
b<-c("피망","넷마블","한게임","다음 게임","인벤","넥슨","네이버 야구9단","던전앤파이터","루리웹닷컴")
am<-am[am$SITE_NM %in% b,]
colnames(am)[2]<-"SITE5"
cate<-merge(cate,am,by="CUS_ID",all.x=T)
cate$SITE5<-ifelse(is.na(cate$SITE5),"NONE",cate$SITE5)
cate$SITE5<-factor(cate$SITE5,labels=1:10)
train_final<-merge(train_final,cate,by="CUS_ID",all.x=T)
write.csv(train_final,"train_final_170528.csv",row.names = F)

cate<-data.frame(CUS_ID=2501:5000)
c1<- tr2 %>% filter(BACT_NM=="게임") %>% group_by(CUS_ID,SITE_NM) %>%  summarise(SITE5=n())
c11<-aggregate(c1[3],by=list(CUS_ID=c1$CUS_ID),max)
summary(c11$SITE5)
c12<-c11[!c11$SITE5<=mean(c11$SITE5),]
am<-merge(c12,c1,by.x=c("CUS_ID","SITE5"),by.y = c("CUS_ID","SITE5"),all=F,all.x=T,all.y=F)
dim(table(am$SITE_NM))
am<-am[order(am$CUS_ID),]
rownames(am)<-1:dim(am)[1]
am<-am[!duplicated(am$CUS_ID),]
am<-am[,-2]
b<-c("피망","넷마블","한게임","다음 게임","인벤","넥슨","네이버 야구9단","던전앤파이터","루리웹닷컴")
am<-am[am$SITE_NM %in% b,]
colnames(am)[2]<-"SITE5"
cate<-merge(cate,am,by="CUS_ID",all.x=T)
cate$SITE5<-ifelse(is.na(cate$SITE5),"NONE",cate$SITE5)
cate$SITE5<-factor(cate$SITE5,labels=1:10)
test_final<-merge(test_final,cate,by="CUS_ID",all.x=T)
write.csv(test_final,"test_final_170528.csv",row.names = F)