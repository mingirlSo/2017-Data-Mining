#사용한 데이터
##7조 코드 6개
##8조 a ,b ,c ,d
##4주차 1,2,3등
##5주차 1,2,3등
##w2v-SITE (nnet,glmnet,gbm)
##w2v-search (xgbtree,rf)


###################################################
##7조
library(xlsx)
train_profiles<-read.xlsx("train_profiles.xlsx",1)
train_profiles$CUS_ID<-as.numeric(as.character(train_profiles$CUS_ID))
train_profiles<-train_profiles[order(train_profiles$CUS_ID,decreasing = F),]
tp<-train_profiles
library(data.table)

train_clickstreams<-fread("train_clickstreams.tab");train_clickstreams[,CUS_ID:= as.numeric(CUS_ID)]
tr<-train_clickstreams


###########파생필드 생성##############

tr<-na.omit(tr)
tr$wk<-weekdays(as.Date(as.character(tr$TIME_ID),"%Y%m%d%H"))
tr$wk<-factor(tr$wk,levels = c("월요일","화요일","수요일","목요일","금요일","토요일","일요일"))
tr$h<-as.numeric(substr(tr$TIME_ID,9,10))
tr$m<-as.numeric(substr(tr$TIME_ID,5,6))
tr$h2<-cut(tr$h,breaks=4,labels=c("0005","0611","1217","1823"))

library(dplyr)
cs.v1<- tr %>% group_by(CUS_ID) %>% summarise(PAGEVIEW=sum(SITE_CNT))
cs.v2<- tr %>% distinct(CUS_ID,TIME_ID) %>% group_by(CUS_ID) %>% summarise(VDAYS=n())
cs.v3<- tr %>% distinct(CUS_ID,BACT_NM) %>% group_by(CUS_ID) %>% summarise(COVERAGE=n()/22)

library(reshape)
##카테고리별 페이지뷰 비율
a<-cast(tr,CUS_ID~BACT_NM,sum,value="SITE_CNT")
a$sum<-rowSums(a[,2:23])
cs.v4<-a[,2:23]/a$sum*100
colnames(cs.v4)<-paste("CP",1:22,sep="")

##요일별 페이지뷰 비율
b<-cast(tr,CUS_ID~wk,sum,value="SITE_CNT")
b$sum<-rowSums(b[,2:8])
cs.v5<-b[,2:8]/b$sum*100
colnames(cs.v5)<-paste("DP",1:7,sep="")

##시간대별 페이지뷰 비율
c<-cast(tr,CUS_ID~h2,sum,value="SITE_CNT")
c$sum<-rowSums(c[,2:5])
cs.v6<-c[,2:5]/c$sum
colnames(cs.v6)<-paste("HP",1:4,sep="")

##월별 페이지뷰 비율
d<-cast(tr,CUS_ID~m,sum,value = "SITE_CNT")
d$sum<-rowSums(d[,2:13])
cs.v7<-d[,2:13]/d$sum*100
colnames(cs.v7)<-paste("MP",colnames(cs.v7),sep="")

###변동계수만들기####
COV<-data.frame(CUS_ID=1:2500)
##요일별 변동계수
DCOV<- tr %>% group_by(CUS_ID,wk) %>% summarise(sum=sum(SITE_CNT)) %>% group_by(CUS_ID) %>% summarise(DCOV=sd(sum)/mean(sum))
COV<-merge(COV,DCOV,by="CUS_ID",all.x = T)
##월별 변동계수
MCOV<- tr %>% group_by(CUS_ID,m) %>% summarise(sum=sum(SITE_CNT)) %>% group_by(CUS_ID) %>% summarise(MCOV=sd(sum)/mean(sum))
COV<-merge(COV,MCOV,by="CUS_ID",all.x = T)
##시간대별 변동계수
HCOV<- tr %>% group_by(CUS_ID,h2) %>% summarise(sum=sum(SITE_CNT)) %>% group_by(CUS_ID) %>% summarise(HCOV=sd(sum)/mean(sum))
COV<-merge(COV,HCOV,by="CUS_ID",all.x = T)
##카데고리별 변동계수
CCOV<- tr %>% group_by(CUS_ID,BACT_NM) %>% summarise(sum=sum(SITE_CNT)) %>% group_by(CUS_ID) %>% summarise(CCOV=sd(sum)/mean(sum))
COV<-merge(COV,CCOV,by="CUS_ID",all.x = T)

###############변수 추가###############
cs.v10<- tr %>% distinct(CUS_ID,SITE_NM) %>% group_by(CUS_ID) %>% summarise(NSITE=n())
cs.v11<- tr %>% group_by(CUS_ID,SITE_NM) %>% summarise(N=n()) %>% group_by(CUS_ID) %>% summarise(NSCOV=sd(N)/mean(N))


train_final<-cs.v1 %>% left_join(cs.v2) %>% left_join(cs.v3)
train_final<-cbind(train_final,cs.v4,cs.v5,cs.v6,cs.v7)
train_final<-merge(train_final,COV,by="CUS_ID",all.x=T)
train_final<-train_final %>% left_join(cs.v10) %>% left_join(cs.v11)
train_final<-merge(train_profiles,train_final,by="CUS_ID")
head(train_final)
#########변수 추가 2## 17 05 07
a<-tr %>% filter(BACT_NM=="쇼핑")%>%group_by(CUS_ID,MACT_NM) %>% summarise(N=n()) %>% group_by(CUS_ID) %>% summarise(cov=sd(N)/mean(N))

train_final<-left_join(train_final,a)

###카테고리별 자주가는 사이트 17 05 07###천천히....
cate<-data.frame(CUS_ID=1:2500)

c1<-tr %>% filter(BACT_NM=="쇼핑") %>% group_by(CUS_ID,ACT_NM) %>% summarise(N=n())
c11<-aggregate(c1[3],by=list(CUS_ID=c1$CUS_ID),max)
summary(c11$N)
c12<-c11[!c11$N<=mean(c11$N),]
am<-merge(c12,c1,by.x=c("CUS_ID","N"),by.y = c("CUS_ID","N"),all=F,all.x=T,all.y=F)
dim(table(am$ACT_NM))
am<-am[order(am$CUS_ID),]
rownames(am)<-1:dim(am)[1]
am<-am[!duplicated(am$CUS_ID),]
am<-am[,-2]
b<-c("오픈마켓","여성의류쇼핑몰","소셜커머스","종합쇼핑몰","남성의류쇼핑몰","중고차쇼핑몰","종합도서쇼핑몰")
am<-am[am$ACT_NM %in% b,]
colnames(am)[2]<-"SITE11"
cate<-merge(cate,am,by="CUS_ID",all.x=T)
cate$SITE11<-ifelse(is.na(cate$SITE11),"NONE",cate$SITE11)
cate$SITE11<-factor(cate$SITE11,labels=1:8)

train_final<-merge(train_final,cate,by="CUS_ID",all.x=T)

#######트레인 키워드 정리#######
train_searchkeywords<-read.delim("train_searchkeywords.tab", stringsAsFactors = F)
head(train_searchkeywords)
ts<-train_searchkeywords

a<-aggregate(ts[2],by=list(CUS_ID=ts$CUS_ID),length)
colnames(a)[2]<-"TOTS"
b<-aggregate(ts[3],by=list(CUS_ID=ts$CUS_ID),sd)
b$QRY_CNT<-ifelse(is.na(b$QRY_CNT),0,b$QRY_CNT)
c<-aggregate(ts[3],by=list(CUS_ID=ts$CUS_ID),mean)
d<-data.frame(CUS_ID=a$CUS_ID,SCOV=b$QRY_CNT/c$QRY_CNT)

train_final<-merge(train_final,a,by="CUS_ID",all=T)
train_final<-merge(train_final,d,by="CUS_ID",all=T)

library(mice)
d<-mice(train_final[,-1],print=F)
d<-merge(train_final[,1,drop=F],complete(d,2),by="row.names",all.x=T)
rownames(d)<-d$Row.names
head(d)
train_final<-d[,-1]
train_final<-train_final[order(train_final$CUS_ID),]
head(train_final)

########### 변수추가 05 09 ############
a<-tr %>% filter(BACT_NM=="쇼핑") %>% group_by(CUS_ID) %>% summarise(SH=sum(ST_TIME))
train_final<-left_join(train_final,a)
train_final$SH<-ifelse(is.na(train_final$SH),0,train_final$SH)


#######변수추가 0511######
v0<-tr %>% group_by(CUS_ID) %>% summarise(DWELLTIME=sum(ST_TIME))

#시간대별 체류시간 비율

v1<-cast(tr,CUS_ID~h,sum,value="ST_TIME")
v11<-v1[,2:25]/rowSums(v1[,2:25])*100
colnames(v11)<-paste("H",colnames(v11),sep="")

#카테고리별 체류시간 비율
v2<-cast(tr,CUS_ID~BACT_NM,sum,value="ST_TIME")
v21<-v2[,2:23]/rowSums(v2[,2:23])*100
colnames(v21)<-paste("CP",1:22,".2",sep="")


#시간대별 체류시간 변동계수
v3<-tr %>% group_by(CUS_ID,h) %>% summarise(Total=sum(ST_TIME)) %>% group_by(CUS_ID) %>% summarise(TCOV=sd(Total)/mean(Total))
v4<-tr %>% group_by(CUS_ID,BACT_NM) %>% summarise(Total=sum(ST_TIME)) %>% group_by(CUS_ID) %>% summarise(CCOV2=sd(Total)/mean(Total))

head(train_final)
train_final<-train_final%>%left_join(v0) %>% left_join(v3) %>% left_join(v4)
train_final<-cbind(train_final,v11,v21)


# Install & load word2vec package
if(!require(devtools)) install.packages("devtools"); library(devtools)
if(!require(wordVectors)) install_github("bmschmidt/wordVectors"); library(wordVectors)

# Install & load data.table package
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(randomForest)) install.packages("randomForest"); library(randomForest)
if(!require(caret)) install.packages("caret"); library(caret)

# list objects in word2vec package
ls("package:wordVectors")


###트레인 소분류 워투백####
###### Fast reading and combining several files using data.table (with fread): 5 times faster than read.csv()
cs.dt <- fread("train_profiles.csv")
tr.dt <- fread("train_clickstreams.tab"); tr.dt[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt, CUS_ID); setkey(tr.dt, CUS_ID) 
md.dt <- merge(cs.dt, tr.dt)


###### Make sites sentences ## 방문한 소분류를 모음
f <- function(x, t) {
  grp <- md.dt[CUS_ID==x, GROUP][1]
  itemfreq <- table(md.dt[CUS_ID==x,  ACT_NM])
  fitems <- itemfreq[itemfreq >= t]
  act <- names(fitems)
  #  
  sapply(act, function(x) gsub(" ", "_", x))
  set.seed(1)
  #
  as.vector((sapply(1:20, function(x) c(grp, sample(act)))))  ##방문한 사이트를 20번 샘플링 --> 5만개의 문장을 만들어서 학습시킴.
}
items <- unlist(sapply(cs.dt$CUS_ID, f, 2))
write.table(items, "items.txt", eol = " ", quote = F, row.names = F, col.names = F)


##### Train site2vec model
model = train_word2vec("items.txt","vec2.bin",vectors=300,threads=1,window=5,cbow=1,iter=5,negative_samples=10, force = T)
# model <- read.binary.vectors("vec2.bin") # reload the model. 
df<-data.frame()
for (i in 1:2500 ){
  itemfreq <- table(tr.dt[CUS_ID==i, ACT_NM])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,cosineSimilarity(model[[names(fitems), average=T]], model[[c("F20-","F30","F40+","M20-","M30","M40+"), average=F]]))
  sim1 <- sim[,c("CUS_ID","F20.","F30","F40.","M20.","M30","M40.")]
  df<-rbind(df,sim1)
}
colnames(df)<-c("CUS_ID","AF20","AF30","AF40","AM20","AM30","AM40")

train_final<-merge(train_final,df,by="CUS_ID")
head(train_final)

#######방문한 SITE_NM모음#####
###트레인 모델 생성 ####
###### Make sites sentences

f <- function(x, t) {
  grp <- md.dt[CUS_ID==x, GROUP][1]
  itemfreq <- table(md.dt[CUS_ID==x,  SITE_NM])
  fitems <- itemfreq[itemfreq >= t]
  act <- names(fitems)
  #  
  sapply(act, function(x) gsub(" ", "_", x))
  set.seed(1)
  #
  as.vector((sapply(1:20, function(x) c(grp, sample(act)))))  ##방문한 사이트를 20번 샘플링 --> 5만개의 문장을 만들어서 학습시킴.
}
items <- unlist(sapply(cs.dt$CUS_ID, f, 2))
write.table(items, "items-site_nm.txt", eol = " ", quote = F, row.names = F, col.names = F)


##### Train site2vec model
model = train_word2vec("items-site_nm.txt","vec-site_nm.bin",vectors=300,threads=1,window=5,cbow=1,iter=5,negative_samples=10, force = T)
# model <- read.binary.vectors("vec-site_nm.bin") # reload the model. 

df<-data.frame()
for (i in 1:2500 ){
  itemfreq <- table(tr.dt[CUS_ID==i, SITE_NM])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,cosineSimilarity(model[[names(fitems), average=T]], model[[c("F20-","F30","F40+","M20-","M30","M40+"), average=F]]))
  sim1 <- sim[,c("CUS_ID","F20.","F30","F40.","M20.","M30","M40.")]
  df<-rbind(df,sim1)
}
colnames(df)<-c("CUS_ID","SF20","SF30","SF40","SM20","SM30","SM40")

train_final<-merge(train_final,df,by="CUS_ID")
head(train_final)

sum(is.na(train_final)) ## 0인가...?
write.csv(train_final,"train_final_170513_2.csv",row.names = F)


################테스트 데이터##################
test_clickstreams<-read.table("test_clickstreams.tab",sep="\t",header = T,stringsAsFactors = F)
tr<-test_clickstreams

##파생필드 생성
tr<-na.omit(tr)
tr$wk<-weekdays(as.Date(as.character(tr$TIME_ID),"%Y%m%d%H"))
tr$wk<-factor(tr$wk,levels = c("월요일","화요일","수요일","목요일","금요일","토요일","일요일"))
tr$h<-as.numeric(substr(tr$TIME_ID,9,10))
tr$m<-as.numeric(substr(tr$TIME_ID,5,6))
tr$h2<-cut(tr$h,breaks=4,labels=c("0005","0611","1217","1823"))

library(dplyr)
cs.v1<- tr %>% group_by(CUS_ID) %>% summarise(PAGEVIEW=sum(SITE_CNT))
cs.v2<- tr %>% distinct(CUS_ID,TIME_ID) %>% group_by(CUS_ID) %>% summarise(VDAYS=n())
cs.v3<- tr %>% distinct(CUS_ID,BACT_NM) %>% group_by(CUS_ID) %>% summarise(COVERAGE=n()/22)

library(reshape)

##카테고리별 페이지뷰 비율
a<-cast(tr,CUS_ID~BACT_NM,sum,value="SITE_CNT")
a$sum<-rowSums(a[,2:23])
cs.v4<-a[,2:23]/a$sum*100
colnames(cs.v4)<-paste("CP",1:22,sep="")

##요일별 페이지뷰 비율
b<-cast(tr,CUS_ID~wk,sum,value="SITE_CNT")
b$sum<-rowSums(b[,2:8])
cs.v5<-b[,2:8]/b$sum*100
colnames(cs.v5)<-paste("DP",1:7,sep="")

##시간대별 페이지뷰 비율
c<-cast(tr,CUS_ID~h2,sum,value="SITE_CNT")
c$sum<-rowSums(c[,2:5])
cs.v6<-c[,2:5]/c$sum
colnames(cs.v6)<-paste("HP",1:4,sep="")

##월별 페이지뷰 비율
d<-cast(tr,CUS_ID~m,sum,value = "SITE_CNT")
d$sum<-rowSums(d[,2:13])
cs.v7<-d[,2:13]/d$sum*100
colnames(cs.v7)<-paste("MP",colnames(cs.v7),sep="")

######변동계수만들기############
COV<-data.frame(CUS_ID=2501:5000)
##요일별 변동계수
DCOV<- tr %>% group_by(CUS_ID,wk) %>% summarise(sum=sum(SITE_CNT)) %>% group_by(CUS_ID) %>% summarise(DCOV=sd(sum)/mean(sum))
COV<-merge(COV,DCOV,by="CUS_ID",all.x = T)
##월별 변동계수
MCOV<- tr %>% group_by(CUS_ID,m) %>% summarise(sum=sum(SITE_CNT)) %>% group_by(CUS_ID) %>% summarise(MCOV=sd(sum)/mean(sum))
COV<-merge(COV,MCOV,by="CUS_ID",all.x = T)
##시간대별 변동계수
HCOV<- tr %>% group_by(CUS_ID,h2) %>% summarise(sum=sum(SITE_CNT)) %>% group_by(CUS_ID) %>% summarise(HCOV=sd(sum)/mean(sum))
COV<-merge(COV,HCOV,by="CUS_ID",all.x = T)
##카데고리별 변동계수
CCOV<- tr %>% group_by(CUS_ID,BACT_NM) %>% summarise(sum=sum(SITE_CNT)) %>% group_by(CUS_ID) %>% summarise(CCOV=sd(sum)/mean(sum))
COV<-merge(COV,CCOV,by="CUS_ID",all.x = T)

###############변수 추가###############
cs.v10<- tr %>% distinct(CUS_ID,SITE_NM) %>% group_by(CUS_ID) %>% summarise(NSITE=n())
cs.v11<- tr %>% group_by(CUS_ID,SITE_NM) %>% summarise(N=n()) %>% group_by(CUS_ID) %>% summarise(NSCOV=sd(N)/mean(N))


test_final<-cs.v1 %>% left_join(cs.v2) %>% left_join(cs.v3)
test_final<-cbind(test_final,cs.v4,cs.v5,cs.v6,cs.v7)
test_final<-merge(test_final,COV,by="CUS_ID",all.x=T)
test_final<-test_final %>% left_join(cs.v10) %>% left_join(cs.v11)
head(test_final)

##변수추가 17 05 07 천천히...
a<-tr %>% filter(BACT_NM=="쇼핑")%>%group_by(CUS_ID,MACT_NM) %>% summarise(N=n()) %>% group_by(CUS_ID) %>% summarise(cov=sd(N)/mean(N))

test_final<-left_join(test_final,a)

cate<-data.frame(CUS_ID=2501:5000)

c1<-tr %>% filter(BACT_NM=="쇼핑") %>% group_by(CUS_ID,ACT_NM) %>% summarise(N=n())
c11<-aggregate(c1[3],by=list(CUS_ID=c1$CUS_ID),max)
summary(c11$N)
c12<-c11[!c11$N<=mean(c11$N),]
am<-merge(c12,c1,by.x=c("CUS_ID","N"),by.y = c("CUS_ID","N"),all=F,all.x=T,all.y=F)
dim(table(am$ACT_NM))
am<-am[order(am$CUS_ID),]
rownames(am)<-1:dim(am)[1]
am<-am[!duplicated(am$CUS_ID),]
am<-am[,-2]
b<-c("오픈마켓","여성의류쇼핑몰","소셜커머스","종합쇼핑몰","남성의류쇼핑몰","중고차쇼핑몰","종합도서쇼핑몰")
am<-am[am$ACT_NM %in% b,]
colnames(am)[2]<-"SITE11"
cate<-merge(cate,am,by="CUS_ID",all.x=T)
cate$SITE11<-ifelse(is.na(cate$SITE11),"NONE",cate$SITE11)
cate$SITE11<-factor(cate$SITE11,labels=1:8)


test_final<-merge(test_final,cate,by="CUS_ID",all.x=T)
head(test_final)

#############테스트 키워드 정리################
test_searchkeywords<-read.delim("test_searchkeywords.tab", stringsAsFactors = F)
ts1<-test_searchkeywords
a<-aggregate(ts1[2],by=list(CUS_ID=ts1$CUS_ID),length)
colnames(a)[2]<-"TOTS"

SCOV<- ts1 %>% group_by(CUS_ID) %>% summarise(SCOV=sd(QRY_CNT)/mean(QRY_CNT))

test_final<-merge(test_final,a,by="CUS_ID",all=T)
test_final<-merge(test_final,SCOV,by="CUS_ID",all.x=T)

library(mice)
d<-mice(test_final[,-1],print=F)
d<-merge(test_final[,1,drop=F],complete(d,2),by="row.names",all.x=T)
rownames(d)<-d$Row.names
head(d)
test_final<-d[,-1]
test_final<-test_final[order(test_final$CUS_ID),]
head(test_final)
sum(is.na(test_final)) ##0이어야함

########### 변수추가 05 09 ############
a<-tr %>% filter(BACT_NM=="쇼핑") %>% group_by(CUS_ID) %>% summarise(SH=sum(ST_TIME))
test_final<-left_join(test_final,a)
test_final$SH<-ifelse(is.na(test_final$SH),0,test_final$SH)


#######변수추가 0511######
v0<-tr %>% group_by(CUS_ID) %>% summarise(DWELLTIME=sum(ST_TIME))

#시간대별 체류시간 비율
v1<-cast(tr,CUS_ID~h,sum,value="ST_TIME")
v11<-v1[,2:25]/rowSums(v1[,2:25])*100
colnames(v11)<-paste("H",colnames(v11),sep="")

#카테고리별 체류시간 비율
v2<-cast(tr,CUS_ID~BACT_NM,sum,value="ST_TIME")
v21<-v2[,2:23]/rowSums(v2[,2:23])*100
colnames(v21)<-paste("CP",1:22,".2",sep="")


#시간대별 체류시간 변동계수
v3<-tr %>% group_by(CUS_ID,h) %>% summarise(Total=sum(ST_TIME)) %>% group_by(CUS_ID) %>% summarise(TCOV=sd(Total)/mean(Total))
v4<-tr %>% group_by(CUS_ID,BACT_NM) %>% summarise(Total=sum(ST_TIME)) %>% group_by(CUS_ID) %>% summarise(CCOV2=sd(Total)/mean(Total))


test_final<-test_final%>%left_join(v0) %>% left_join(v3) %>% left_join(v4)
test_final<-cbind(test_final,v11,v21)

####테스트 소분류 워투백 모델 적용#####
tr2<-fread("test_clickstreams.tab"); tr2[,CUS_ID:= as.numeric(CUS_ID)]
model <- read.binary.vectors("vec2.bin")
df<-data.frame()
for (i in 2501:5000 ){
  itemfreq <- table(tr2[CUS_ID==i, ACT_NM])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,cosineSimilarity(model[[names(fitems), average=T]], model[[c("F20-","F30","F40+","M20-","M30","M40+"), average=F]]))
  sim1 <- sim[,c("CUS_ID","F20.","F30","F40.","M20.","M30","M40.")]
  df<-rbind(df,sim1)
}
colnames(df)<-c("CUS_ID","AF20","AF30","AF40","AM20","AM30","AM40")
test_final<-merge(test_final,df,by="CUS_ID")
head(test_final)

sum(is.na(test_final))

####테스트 사이트 워투백 모델 적용####
tr2<-fread("test_clickstreams.tab"); tr2[,CUS_ID:= as.numeric(CUS_ID)]
model <- read.binary.vectors("vec-site_nm.bin")
df<-data.frame()
for (i in 2501:5000 ){
  itemfreq <- table(tr2[CUS_ID==i, SITE_NM])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,cosineSimilarity(model[[names(fitems), average=T]], model[[c("F20-","F30","F40+","M20-","M30","M40+"), average=F]]))
  sim1 <- sim[,c("CUS_ID","F20.","F30","F40.","M20.","M30","M40.")]
  df<-rbind(df,sim1)
}
colnames(df)<-c("CUS_ID","SF20","SF30","SF40","SM20","SM30","SM40")


test_final<-merge(test_final,df,by="CUS_ID")
head(test_final)

sum(is.na(test_final)) ###0인가요...

write.csv(test_final,"test_final_170513_2.csv",row.names = F)


if(!require(devtools)) install.packages("devtools"); library(devtools)
if(!require(wordVectors)) install_github("bmschmidt/wordVectors"); library(wordVectors)

# Install & load data.table package
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(randomForest)) install.packages("randomForest"); library(randomForest)
if(!require(caret)) install.packages("caret"); library(caret)

# list objects in word2vec package
ls("package:wordVectors")

####트레인 키워드 워투백####
# Install & load word2vec package
if(!require(devtools)) install.packages("devtools"); library(devtools)
if(!require(wordVectors)) install_github("bmschmidt/wordVectors"); library(wordVectors)

# Install & load data.table package
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(randomForest)) install.packages("randomForest"); library(randomForest)
if(!require(caret)) install.packages("caret"); library(caret)

setwd("C:\\Users\\Hyangsuk_Min\\Desktop\\데마팀플\\원데이터")
library(data.table)
cs.dt <- fread("train_profiles.csv")
ts<-fread("train_searchkeywords.tab", stringsAsFactors = F);ts[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt, CUS_ID); setkey(ts, CUS_ID) 
mg<-merge(cs.dt,ts)
mg$QRY_STR<-ifelse(regexpr("query",mg$QRY_STR)>0 ,
                   substr(mg$QRY_STR,regexpr("query",mg$QRY_STR)+6,500),
                   mg$QRY_STR)
mg$QRY_STR<-ifelse(regexpr("acq",mg$QRY_STR)>0 ,
                   substr(mg$QRY_STR,1,regexpr("&",mg$QRY_STR)-1),
                   mg$QRY_STR)

f <- function(x, t) {
  grp <- mg[CUS_ID==x, GROUP][1]
  itemfreq <- table(mg[CUS_ID==x,  QRY_STR])
  fitems <- itemfreq[itemfreq >= t]
  act <- names(fitems)
  #  
  sapply(act, function(x) gsub(" ", "_", x))
  set.seed(1)
  #
  as.vector((sapply(1:10, function(x) c(grp, sample(act)))))  ##방문한 사이트를 20번 샘플링 --> 5만개의 문장을 만들어서 학습시킴.
}
items_s <- unlist(sapply(cs.dt$CUS_ID, f, 1))
write.table(items_s, "items-ts.txt", eol = " ", quote = F, row.names = F, col.names = F)

model = train_word2vec("items-ts.txt","vec-ts.bin",vectors=300,threads=1,window=5,cbow=1,iter=5,negative_samples=10, force = T)
#model <- read.binary.vectors("vec-ts.bin") # reload the model.

b<-unique(mg$CUS_ID)

df<-data.frame()
for (i in b ){
  itemfreq <- table(mg[CUS_ID==i, QRY_STR])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,cosineSimilarity(model[[names(fitems), average=T]], model[[c("F20-","F30","F40+","M20-","M30","M40+"), average=F]]))
  sim1 <- sim[,c("CUS_ID","F20.","F30","F40.","M20.","M30","M40.")]
  df<-rbind(df,sim1)
}
colnames(df)<-c("CUS_ID","KF20","KF30","KF40","KM20","KM30","KM40")

library(dplyr)
train_final<-read.csv("train_final_170513_2.csv")
train_final2<-left_join(train_final,df)
head(train_final2)
sum(is.na(train_final2))

library(mice)
d<-mice(train_final2[,123:128],print=F)
d<-merge(train_final2[,1:122,drop=F],complete(d,2),by="row.names",all.x=T)
rownames(d)<-d$Row.names
head(d)
d<-d[,-1]

train_final2<-d[order(d$CUS_ID),]
write.csv(train_final2,"train_final.csv",row.names = F)

###테스트 키워드 모델 적용####
setwd("C:\\Users\\Hyangsuk_Min\\Desktop\\데마팀플\\원데이터")
ts2<-fread("test_searchkeywords.tab", stringsAsFactors = F);ts[,CUS_ID:= as.numeric(CUS_ID)]
setkey(ts2, CUS_ID) 
model <- read.binary.vectors("vec-ts.bin") # reload the model.
ts2$QRY_STR<-ifelse(regexpr("query",ts2$QRY_STR)>0 ,
                    substr(ts2$QRY_STR,regexpr("query",ts2$QRY_STR)+6,500),
                    ts2$QRY_STR)
ts2$QRY_STR<-ifelse(regexpr("acq",ts2$QRY_STR)>0 ,
                    substr(ts2$QRY_STR,1,regexpr("&",ts2$QRY_STR)-1),
                    ts2$QRY_STR)
df<-data.frame()
b<-unique(ts2$CUS_ID)
for (i in b ){
  itemfreq <- table(ts2[CUS_ID==i, QRY_STR])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,cosineSimilarity(model[[names(fitems), average=T]], model[[c("F20-","F30","F40+","M20-","M30","M40+"), average=F]]))
  sim1 <- sim[,c("CUS_ID","F20.","F30","F40.","M20.","M30","M40.")]
  df<-rbind(df,sim1)
}
colnames(df)<-c("CUS_ID","KF20","KF30","KF40","KM20","KM30","KM40")

library(dplyr)
test_final<-read.csv("test_final_170513_2.csv")
test_final2<-left_join(test_final,df)
head(test_final2)
sum(is.na(test_final2))

library(mice)
d<-mice(test_final2[,122:127],print=F)
d<-merge(test_final2[,1:121,drop=F],complete(d,2),by="row.names",all.x=T)
rownames(d)<-d$Row.names
head(d)
d<-d[,-1]

test_final2<-d[order(d$CUS_ID),]
write.csv(test_final2,"test_final.csv",row.names = F)


#####0526변수추가#####

rm(list=ls())
setwd("C:\\Users\\Hyangsuk_Min\\Desktop\\데마팀플\\원데이터")
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
setwd("C:\\Users\\Hyangsuk_Min\\Desktop\\데마팀플\\원데이터")
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


########테스트데이터 합치기#####
rm(list=ls())
train_final<-read.csv("train_final_170528.csv")
test_final<-read.csv("test_final_170528.csv")

test_pulic<-read.csv("test_public.csv")
test_pulic$GROUP<-ifelse(test_pulic$F20.==1,"F20-",
                         ifelse(test_pulic$F30==1,"F30",
                                ifelse(test_pulic$F40.==1,"F40+",
                                       ifelse(test_pulic$M20.,"M20-",
                                              ifelse(test_pulic$M30,"M30","M40+")))))

test_final2<-merge(test_pulic[,c(1,8)],test_final,by="CUS_ID",all.x=T)
train_final<-rbind(train_final,test_final2)
test_final<-test_final[!(test_final$CUS_ID %in% test_pulic$CUS_ID),]

write.csv(train_final,"train_final_170529.csv",row.names = F)
write.csv(test_final,"test_final_170529.csv",row.names = F)

#######워드투백 새로 생성하기####
rm(list=ls())
# Install & load word2vec package
if(!require(devtools)) install.packages("devtools"); library(devtools)
if(!require(wordVectors)) install_github("bmschmidt/wordVectors"); library(wordVectors)

# Install & load data.table package
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(randomForest)) install.packages("randomForest"); library(randomForest)
if(!require(caret)) install.packages("caret"); library(caret)

# list objects in word2vec package
ls("package:wordVectors")


###트레인 소분류 워투백####
###### Fast reading and combining several files using data.table (with fread): 5 times faster than read.csv()
cs.dt <- fread("train_profiles.csv")
tr.dt <- fread("train_clickstreams.tab"); tr.dt[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt, CUS_ID); setkey(tr.dt, CUS_ID) 
md.dt <- merge(cs.dt, tr.dt)

test_pulic<-fread("test_public.csv")
test_pulic$GROUP<-ifelse(test_pulic$`F20-`==1,"F20-",
                         ifelse(test_pulic$F30==1,"F30",
                                ifelse(test_pulic$`F40+`==1,"F40+",
                                       ifelse(test_pulic$`M20-`,"M20-",
                                              ifelse(test_pulic$M30,"M30","M40+")))))
tr.dt2<-fread("test_clickstreams.tab"); tr.dt2[,CUS_ID:=as.numeric(CUS_ID)]
setkey(test_pulic,CUS_ID);setkey(tr.dt2,CUS_ID)
md.dt2<-merge(test_pulic[,c(1,8)],tr.dt2)
md.dt<-rbind(md.dt,md.dt2)
cus<-unique(md.dt$CUS_ID)
###### Make sites sentences ## 방문한 소분류를 모음
f <- function(x, t) {
  grp <- md.dt[CUS_ID==x, GROUP][1]
  itemfreq <- table(md.dt[CUS_ID==x,  ACT_NM])
  fitems <- itemfreq[itemfreq >= t]
  act <- names(fitems)
  #  
  sapply(act, function(x) gsub(" ", "_", x))
  set.seed(1)
  #
  as.vector((sapply(1:20, function(x) c(grp, sample(act)))))  ##방문한 사이트를 20번 샘플링 --> 5만개의 문장을 만들어서 학습시킴.
}
items <- unlist(sapply(cus, f, 2))

write.table(items, "items-act_nm.txt", eol = " ", quote = F, row.names = F, col.names = F)


##### Train site2vec model
model = train_word2vec("items-act_nm.txt","vec-act_nm.bin",vectors=300,threads=1,window=5,cbow=1,iter=5,negative_samples=10, force = T)
# model <- read.binary.vectors("vec-act_nm.bin") # reload the model. 
df<-data.frame()
for (i in cus ){
  itemfreq <- table(md.dt[CUS_ID==i, ACT_NM])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,cosineSimilarity(model[[names(fitems), average=T]], model[[c("F20-","F30","F40+","M20-","M30","M40+"), average=F]]))
  sim1 <- sim[,c("CUS_ID","F20.","F30","F40.","M20.","M30","M40.")]
  df<-rbind(df,sim1)
}
colnames(df)<-c("CUS_ID","AF20","AF30","AF40","AM20","AM30","AM40")

train_final<-read.csv("train_final_170529.csv")
train_final[,111:116]<-NULL
train_final<-merge(train_final,df,by="CUS_ID")

#####테스트적용#####
tr.dt2<-tr.dt2[!(tr.dt2$CUS_ID %in% cus),]
cus2<-unique(tr.dt2$CUS_ID)
df<-data.frame()
for (i in cus2 ){
  itemfreq <- table(tr.dt2[CUS_ID==i, ACT_NM])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,cosineSimilarity(model[[names(fitems), average=T]], model[[c("F20-","F30","F40+","M20-","M30","M40+"), average=F]]))
  sim1 <- sim[,c("CUS_ID","F20.","F30","F40.","M20.","M30","M40.")]
  df<-rbind(df,sim1)
}
colnames(df)<-c("CUS_ID","AF20","AF30","AF40","AM20","AM30","AM40")

test_final<-read.csv("test_final_170529.csv")
test_final[,110:115]<-NULL
test_final<-merge(test_final,df,by="CUS_ID")



#######방문한 SITE_NM모음#####
###### 트레인 모델 생성

setwd("D:/r/데마")
if(!require(devtools)) install.packages("devtools"); library(devtools)
if(!require(wordVectors)) install_github("bmschmidt/wordVectors"); library(wordVectors)

# Install & load data.table package
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(randomForest)) install.packages("randomForest"); library(randomForest)
if(!require(caret)) install.packages("caret"); library(caret)

# list objects in word2vec package
ls("package:wordVectors")


###트레인 소분류 워투백####
###### Fast reading and combining several files using data.table (with fread): 5 times faster than read.csv()
cs.dt <- fread("train_profiles.csv")
tr.dt <- fread("train_clickstreams.tab"); tr.dt[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt, CUS_ID); setkey(tr.dt, CUS_ID) 
md.dt <- merge(cs.dt, tr.dt)

test_pulic<-fread("test_public.csv")
test_pulic$GROUP<-ifelse(test_pulic$`F20-`==1,"F20-",
                         ifelse(test_pulic$F30==1,"F30",
                                ifelse(test_pulic$`F40+`==1,"F40+",
                                       ifelse(test_pulic$`M20-`,"M20-",
                                              ifelse(test_pulic$M30,"M30","M40+")))))
tr.dt2<-fread("test_clickstreams.tab"); tr.dt2[,CUS_ID:=as.numeric(CUS_ID)]
setkey(test_pulic,CUS_ID);setkey(tr.dt2,CUS_ID)
md.dt2<-merge(test_pulic[,c(1,8)],tr.dt2)
md.dt<-rbind(md.dt,md.dt2)
cus<-unique(md.dt$CUS_ID)
###### Make sites sentences ## 방문한 소분류를 모음
f <- function(x, t) {
  grp <- md.dt[CUS_ID==x, GROUP][1]
  itemfreq <- table(md.dt[CUS_ID==x,  ACT_NM])
  fitems <- itemfreq[itemfreq >= t]
  act <- names(fitems)
  #  
  sapply(act, function(x) gsub(" ", "_", x))
  set.seed(1)
  #
  as.vector((sapply(1:20, function(x) c(grp, sample(act)))))  ##방문한 사이트를 20번 샘플링 --> 5만개의 문장을 만들어서 학습시킴.
}
items <- unlist(sapply(cus, f, 2))

write.table(items, "items-act_nm.txt", eol = " ", quote = F, row.names = F, col.names = F)


##### Train site2vec model
model = train_word2vec("items-act_nm.txt","vec-act_nm.bin",vectors=300,threads=1,window=5,cbow=1,iter=5,negative_samples=10, force = T)
# model <- read.binary.vectors("vec-act_nm.bin") # reload the model. 
df<-data.frame()
for (i in cus ){
  itemfreq <- table(md.dt[CUS_ID==i, ACT_NM])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,cosineSimilarity(model[[names(fitems), average=T]], model[[c("F20-","F30","F40+","M20-","M30","M40+"), average=F]]))
  sim1 <- sim[,c("CUS_ID","F20.","F30","F40.","M20.","M30","M40.")]
  df<-rbind(df,sim1)
}
colnames(df)<-c("CUS_ID","AF20","AF30","AF40","AM20","AM30","AM40")

train_final<-read.csv("train_final_170529.csv")
colnames(train_final[,111:116])
train_final[,111:116]<-NULL
train_final<-merge(train_final,df,by="CUS_ID")
write.csv(train_final,"train_final_170529_1.csv",row.names = F)

#####테스트적용#####
tr.dt2<-tr.dt2[!(tr.dt2$CUS_ID %in% cus),]
cus2<-unique(tr.dt2$CUS_ID)
df<-data.frame()
for (i in cus2 ){
  itemfreq <- table(tr.dt2[CUS_ID==i, ACT_NM])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,cosineSimilarity(model[[names(fitems), average=T]], model[[c("F20-","F30","F40+","M20-","M30","M40+"), average=F]]))
  sim1 <- sim[,c("CUS_ID","F20.","F30","F40.","M20.","M30","M40.")]
  df<-rbind(df,sim1)
}
colnames(df)<-c("CUS_ID","AF20","AF30","AF40","AM20","AM30","AM40")

test_final<-read.csv("test_final_170529.csv")
test_final[,110:115]<-NULL
test_final<-merge(test_final,df,by="CUS_ID")
write.csv(test_final,"test_final_170529_1.csv",row.names = F)


#######방문한 SITE_NM모음#####
###### 트레인 모델 생성
cs.dt <- fread("train_profiles.csv")
tr.dt <- fread("train_clickstreams.tab"); tr.dt[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt, CUS_ID); setkey(tr.dt, CUS_ID) 
md.dt <- merge(cs.dt, tr.dt)

test_pulic<-fread("test_public.csv")
test_pulic$GROUP<-ifelse(test_pulic$`F20-`==1,"F20-",
                         ifelse(test_pulic$F30==1,"F30",
                                ifelse(test_pulic$`F40+`==1,"F40+",
                                       ifelse(test_pulic$`M20-`,"M20-",
                                              ifelse(test_pulic$M30,"M30","M40+")))))
tr.dt2<-fread("test_clickstreams.tab"); tr.dt2[,CUS_ID:=as.numeric(CUS_ID)]
setkey(test_pulic,CUS_ID);setkey(tr.dt2,CUS_ID)
md.dt2<-merge(test_pulic[,c(1,8)],tr.dt2)
md.dt<-rbind(md.dt,md.dt2)
cus<-unique(md.dt$CUS_ID) 

f <- function(x, t) {
  grp <- md.dt[CUS_ID==x, GROUP][1]
  itemfreq <- table(md.dt[CUS_ID==x,  SITE_NM])
  fitems <- itemfreq[itemfreq >= t]
  act <- names(fitems)
  #  
  sapply(act, function(x) gsub(" ", "_", x))
  set.seed(1)
  #
  as.vector((sapply(1:20, function(x) c(grp, sample(act)))))  ##방문한 사이트를 20번 샘플링 --> 5만개의 문장을 만들어서 학습시킴.
}
items <- unlist(sapply(cus, f, 2))
write.table(items, "items-site_nm.txt", eol = " ", quote = F, row.names = F, col.names = F)


##### Train site2vec model
#model = train_word2vec("items-site_nm.txt","vec-site_nm.bin",vectors=300,threads=1,window=5,cbow=1,iter=5,negative_samples=10, force = T)
model <- read.binary.vectors("vec-site_nm.bin") # reload the model. 

df<-data.frame()
for (i in cus ){
  itemfreq <- table(md.dt[CUS_ID==i, SITE_NM])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,cosineSimilarity(model[[names(fitems), average=T]], model[[c("F20-","F30","F40+","M20-","M30","M40+"), average=F]]))
  sim1 <- sim[,c("CUS_ID","F20.","F30","F40.","M20.","M30","M40.")]
  df<-rbind(df,sim1)
}
colnames(df)<-c("CUS_ID","SF20","SF30","SF40","SM20","SM30","SM40")
train_final<-read.csv("train_final_170529_1.csv")
train_final[,111:116]<-NULL
train_final<-merge(train_final,df,by="CUS_ID")
head(train_final)

sum(is.na(train_final)) ## 0인가...?
write.csv(train_final,"train_final_170530.csv",row.names = F)


#####테스트적용#####
tr.dt2<-tr.dt2[!(tr.dt2$CUS_ID %in% test_pulic$CUS_ID),]
cus2<-unique(tr.dt2$CUS_ID)
df<-data.frame()
for (i in cus2 ){
  itemfreq <- table(tr.dt2[CUS_ID==i, SITE_NM])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,cosineSimilarity(model[[names(fitems), average=T]], model[[c("F20-","F30","F40+","M20-","M30","M40+"), average=F]]))
  sim1 <- sim[,c("CUS_ID","F20.","F30","F40.","M20.","M30","M40.")]
  df<-rbind(df,sim1)
}
colnames(df)<-c("CUS_ID","SF20","SF30","SF40","SM20","SM30","SM40")
test_final<-read.csv("test_final_170529_1.csv")
test_final[,110:115]<-NULL
test_final<-merge(test_final,df,by="CUS_ID")
sum(is.na(test_final)) ## 0인가...?
write.csv(test_final,"test_final_170530.csv",row.names = F)


#############키워드 워투백##################

rm(list=ls())
gc()
setwd("D:/r/데마")
if(!require(devtools)) install.packages("devtools"); library(devtools)
if(!require(wordVectors)) install_github("bmschmidt/wordVectors"); library(wordVectors)

# Install & load data.table package
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(randomForest)) install.packages("randomForest"); library(randomForest)
if(!require(caret)) install.packages("caret"); library(caret)

# list objects in word2vec package
ls("package:wordVectors")

####트레인 키워드 워투백####
# Install & load word2vec package
if(!require(devtools)) install.packages("devtools"); library(devtools)
if(!require(wordVectors)) install_github("bmschmidt/wordVectors"); library(wordVectors)

# Install & load data.table package
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(randomForest)) install.packages("randomForest"); library(randomForest)
if(!require(caret)) install.packages("caret"); library(caret)


library(data.table)
cs.dt <- fread("train_profiles.csv")
ts<-fread("train_searchkeywords.tab", stringsAsFactors = F);ts[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt, CUS_ID); setkey(ts, CUS_ID) 
mg<-merge(cs.dt,ts)

test_pulic<-fread("test_public.csv")
test_pulic$GROUP<-ifelse(test_pulic$`F20-`==1,"F20-",
                         ifelse(test_pulic$F30==1,"F30",
                                ifelse(test_pulic$`F40+`==1,"F40+",
                                       ifelse(test_pulic$`M20-`,"M20-",
                                              ifelse(test_pulic$M30,"M30","M40+")))))
ts2<-fread("test_searchkeywords.tab", stringsAsFactors = F);ts[,CUS_ID:= as.numeric(CUS_ID)]
setkey(test_pulic,CUS_ID);setkey(ts2, CUS_ID) 
mg2<-merge(test_pulic[,c(1,8)],ts2,by="CUS_ID")
mg<-rbind(mg,mg2)

mg$QRY_STR<-ifelse(regexpr("query",mg$QRY_STR)>0 ,
                   substr(mg$QRY_STR,regexpr("query",mg$QRY_STR)+6,500),
                   mg$QRY_STR)
mg$QRY_STR<-ifelse(regexpr("acq",mg$QRY_STR)>0 ,
                   substr(mg$QRY_STR,1,regexpr("&",mg$QRY_STR)-1),
                   mg$QRY_STR)
cus<-unique(mg$CUS_ID)

f <- function(x, t) {
  grp <- mg[CUS_ID==x, GROUP][1]
  itemfreq <- table(mg[CUS_ID==x,  QRY_STR])
  fitems <- itemfreq[itemfreq >= t]
  act <- names(fitems)
  #  
  sapply(act, function(x) gsub(" ", "_", x))
  set.seed(1)
  #
  as.vector((sapply(1:10, function(x) c(grp, sample(act)))))  ##방문한 사이트를 20번 샘플링 --> 5만개의 문장을 만들어서 학습시킴.
}
items_s <- unlist(sapply(cus, f, 1))
write.table(items_s, "items-ts-all.txt", eol = " ", quote = F, row.names = F, col.names = F)

#model = train_word2vec("items-ts-all.txt","vec-ts-all.bin",vectors=300,threads=1,window=5,cbow=1,iter=5,negative_samples=10, force = T)
model <- read.binary.vectors("vec-ts-all.bin") # reload the model.

b<-unique(mg$CUS_ID)

df<-data.frame()
for (i in b ){
  itemfreq <- table(mg[CUS_ID==i, QRY_STR])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,cosineSimilarity(model[[names(fitems), average=T]], model[[c("F20-","F30","F40+","M20-","M30","M40+"), average=F]]))
  sim1 <- sim[,c("CUS_ID","F20.","F30","F40.","M20.","M30","M40.")]
  df<-rbind(df,sim1)
}
colnames(df)<-c("CUS_ID","KF20","KF30","KF40","KM20","KM30","KM40")

library(dplyr)
train_final<-read.csv("train_final_170530.csv")
train_final[111:116]<-NULL
train_final2<-merge(train_final,df,by="CUS_ID",all.x=T)

library(mice)
d<-mice(train_final2[,554:559],print=F)
d<-merge(train_final2[,1:553,drop=F],complete(d,2),by="row.names",all.x=T)
rownames(d)<-d$Row.names
head(d)
d<-d[,-1]

train_final2<-d[order(d$CUS_ID),]
write.csv(train_final2,"train_final_170530_k.csv",row.names = F)

###테스트 키워드 모델 적용####
ts2<-ts2[!(ts2$CUS_ID %in% test_pulic$CUS_ID),]
ts2$QRY_STR<-ifelse(regexpr("query",ts2$QRY_STR)>0 ,
                    substr(ts2$QRY_STR,regexpr("query",ts2$QRY_STR)+6,500),
                    ts2$QRY_STR)
ts2$QRY_STR<-ifelse(regexpr("acq",ts2$QRY_STR)>0 ,
                    substr(ts2$QRY_STR,1,regexpr("&",ts2$QRY_STR)-1),
                    ts2$QRY_STR)
df<-data.frame()
b<-unique(ts2$CUS_ID)
for (i in b ){
  itemfreq <- table(ts2[CUS_ID==i, QRY_STR])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,cosineSimilarity(model[[names(fitems), average=T]], model[[c("F20-","F30","F40+","M20-","M30","M40+"), average=F]]))
  sim1 <- sim[,c("CUS_ID","F20.","F30","F40.","M20.","M30","M40.")]
  df<-rbind(df,sim1)
}
colnames(df)<-c("CUS_ID","KF20","KF30","KF40","KM20","KM30","KM40")

library(dplyr)
test_final<-read.csv("test_final_170530.csv")
test_final[110:115]<-NULL
test_final2<-merge(test_final,df,by="CUS_ID",all.x = T)
head(test_final2)
sum(is.na(test_final2))

library(mice)
d<-mice(test_final2[,553:558],print=F)
d<-merge(test_final2[,1:552,drop=F],complete(d,2),by="row.names",all.x=T)
rownames(d)<-d$Row.names
head(d)
d<-d[,-1]

test_final2<-d[order(d$CUS_ID),]
write.csv(test_final2,"test_final_170530_k.csv",row.names = F)


##### 변수 고르기 #####
rm(list=ls())

library(mlbench)
library(caret)
library(plyr)
library(dplyr)
library(data.table)
library(FSelector)
##### Data read #####
train <- read.csv("train_final_170530_k.csv")
test <- read.csv("test_final_170530_k.csv")
##### Modeling #####
train$GROUP<-factor(train$GROUP,labels =c("F20","F30","F40","M20","M30","M40"))

## another method 
weights <- chi.squared(GROUP~., train[,-1])
print(weights)

train_var = train[,-c(1,2)]
selvar = train_var[,weights$attr_importance !=0]

train_sel = cbind(train[,1:2], selvar)
train_sel$GROUP<-factor(train_sel$GROUP,labels = c("F20-","F30","F40+","M20-","M30","M40+"))

write.csv(train_sel, file = "train_final_sel.csv", row.names = F)

test_var = test[,-1]
selvar_test = test_var[,weights$attr_importance !=0]

test_sel = cbind(test[,1], selvar_test)
names(test_sel)[1]<-"CUS_ID"
write.csv(test_sel, file = "test_final_sel.csv", row.names = F)


########모델생성1 7조######
rm(list=ls())
library(caret)
setwd("C:\\Users\\Hyangsuk_Min\\Desktop\\데마팀플\\원데이터")
train_final<-read.csv("train_final_sel.csv")
test_final<-read.csv("test_final_sel.csv")
trf<-train_final
tef<-test_final

set.seed(1)

number<-5
library(caret)
skf<-createFolds(trf$GROUP,k=number,list=T)
methods<-c("wsrf","extraTrees","rf","nnet","gbm")
dataset_blend_train<-data.frame(CUS_ID=trf$CUS_ID)
dataset_blend_test<-data.frame(CUS_ID=tef$CUS_ID)

trf$GROUP<-factor(make.names(trf$GROUP,unique = F,allow_ = T))
y<-trf[,2,drop=F]
x<-trf[,-2]
dbt<-data.frame(CUS_ID=tef$CUS_ID)
a<-data.frame()


for (j in 1:length(methods)){
  print(methods[j])
  a<-data.frame()
  for ( i in 1:number){
    print(i)
    x_train<-x[-skf[[i]],]
    y_train<-y[-skf[[i]],1]
    x_test<-x[skf[[i]],]
    y_test<-y[skf[[i]],1]
    model<-train(x_train[,-1],y_train,method=methods[j],metric = "logLoss",preProcess = "scale",
                 trControl = trainControl(number=1,classProbs = T,summaryFunction = mnLogLoss,savePredictions = T))
    print(model)
    print(i)
    y_submission<-data.frame(CUS_ID=x_test$CUS_ID,predict(model,x_test,type="prob"))
    print(i)
    a<-rbind(a,y_submission)
    print(i)
    dbt[,(2+(i-1)*6):(7+(i-1)*6)]<-predict(model,tef[,-1],type="prob")
  }
  names(a)<-c("CUS_ID",paste(methods[j],1:6,sep=""))
  dataset_blend_train<-merge(dataset_blend_train,a,by="CUS_ID")
  for (z in 1:6){
    dbt[31+z]<-(dbt[1+z]+dbt[7+z]+dbt[13+z]+dbt[19+z]+dbt[25+z])/5}
  dataset_blend_test[(2+(j-1)*6):(7+(j-1)*6)]<-dbt[32:37]
}

head(dataset_blend_train)
head(dataset_blend_test)
names(dataset_blend_test)<-names(dataset_blend_train)

y<-trf[,2]

control <- trainControl(method="repeatedcv", number=5,repeats = 3, summaryFunction=mnLogLoss,classProbs=TRUE)
model2 <- train(dataset_blend_train[,-1],y,method="xgbTree", metric="logLoss", trControl=control)

summary(model2$results$logLoss)

y_submission<-predict(model2,dataset_blend_test,type="prob")

y_submission<-(y_submission - min(y_submission))/(max(y_submission)-min(y_submission))

a<-data.frame(CUS_ID=tef$CUS_ID,y_submission)
names(a)[2:7]<-c("F20-","F30","F40+","M20-","M30","M40+")
head(a)
write.csv(a,"7조-logloss-175030.csv",row.names=F)

#######

trf<-read.csv("train_final_sel.csv")
trf$GROUP<-factor(make.names(trf$GROUP,unique = F,allow_ = T))
tef<-read.csv("test_final_sel.csv")

xgb_grid_1 = expand.grid(
  nrounds = c(100,300,400,600),
  eta = 0.01,
  max_depth = c(4),
  gamma = c(0.1),
  subsample = 0.8,
  colsample_bytree = c(0.8) ,
  min_child_weight=c(1)
)

xgb_trcontrol_1 = trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "final",
  classProbs = TRUE,
  savePredictions = 'final',
  summaryFunction = mnLogLoss ,
  allowParallel = TRUE
)


xgbx1 = train(GROUP~.-CUS_ID,trf, trControl = xgb_trcontrol_1,  
              tuneGrid = xgb_grid_1,  method = "xgbTree",  
              metric = "logLoss",maximize=FALSE)


pred<-data.frame(CUS_ID=tef$CUS_ID,predict(xgbx1,tef,type='prob'))
names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred, "7조-변수추가-xgb.csv", row.names = F)


######MASTER1#######
setwd("C:\\Users\\Hyangsuk_Min\\Desktop\\데마팀플\\원데이터")
rm(list=ls())
#train
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape)
library(matrixStats)
if(!require(data.table)) install.packages("data.table"); library(data.table)

#data
cs.dt.main <- fread("train_profiles.csv")
tr.dt.main <- fread("train_clickstreams.tab"); tr.dt.main[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt.main, CUS_ID); setkey(tr.dt.main, CUS_ID)
md.dt.main <- merge(cs.dt.main, tr.dt.main)

## train_sub
cs.dt.sub <- fread("test_public.csv")
cs.dt.sub$GROUP<-ifelse(cs.dt.sub$`F20-`==1,"F20-",
                        ifelse(cs.dt.sub$F30==1,"F30",
                               ifelse(cs.dt.sub$`F40+`==1,"F40+",
                                      ifelse(cs.dt.sub$`M20-`,"M20-",
                                             ifelse(cs.dt.sub$M30,"M30","M40+")))))
tr.dt.sub <- fread("test_clickstreams.tab"); tr.dt.sub[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt.sub, CUS_ID); setkey(tr.dt.sub, CUS_ID) 
md.dt.sub <- merge(cs.dt.sub[,c(1,8)], tr.dt.sub)

##newtrain
md.dt=rbind(md.dt.main,md.dt.sub)

cstr <- md.dt %>% select(-GROUP)
prof <- md.dt %>% select(CUS_ID,GROUP) %>% distinct(CUS_ID, .keep_all=T)

#save memory
write.csv(cstr, "cstr.csv",row.names=F)
write.csv(prof, "prof.csv",row.names=F)
#rm(list=ls())

cstr = fread("cstr.csv")
#cstr = cstr[,-1]
prof = fread("prof.csv")
#prof = prof[,-1]


cs.t1 = cstr %>% melt(id.vars=c("CUS_ID","ACT_NM"), measure.vars=c("ST_TIME")) %>%
  cast(CUS_ID~ACT_NM, sum, subset=variable=="ST_TIME")
cs.t2 = cs.t1
cs.t2[,-1] = 100*(cs.t2[,-1]/rowSums(cs.t2[,-1]))

cs.c1 = cstr %>% melt(id.vars=c("CUS_ID","ACT_NM"), measure.vars=c("SITE_CNT")) %>%
  cast(CUS_ID~ACT_NM, sum, subset=variable=="SITE_CNT")
cs.c2 = cs.c1
cs.c2[,-1] = 100*(cs.c2[,-1]/rowSums(cs.c2))

pf = prof %>% 
  mutate(gr=ifelse(GROUP=="M20-",0,
                   ifelse(GROUP=="M30",1,
                          ifelse(GROUP=="M40+",2, 
                                 ifelse(GROUP=="F20-",3,
                                        ifelse(GROUP=="F30",4,5)))))) %>% select(-GROUP)

cs.v1= pf %>% left_join(cs.t1)%>% left_join(cs.c1, by="CUS_ID", suffix=c(".t1",".c1"))
cs.v2= pf %>% left_join(cs.t2) %>% left_join(cs.c2, by="CUS_ID", suffix=c(".t2",".c2"))

custsig1 = pf %>% left_join(cs.v1) %>% left_join(cs.v2)
rm(cs.t1);rm(cs.t2);rm(cs.v1);rm(cs.v2);rm(cs.c1);rm(cs.c2)


cs.tc = cstr %>% group_by(CUS_ID) %>% select(CUS_ID, SITE_CNT) %>% summarize(total_cnt=sum(SITE_CNT))
cs.tt = cstr %>% group_by(CUS_ID) %>% select(CUS_ID, ST_TIME) %>% summarize(total_time=sum(ST_TIME))


cs.npr = cstr %>% group_by(CUS_ID) %>% select(CUS_ID, SITE) %>% summarize(reppagenum=n())
cs.np = cstr %>% group_by(CUS_ID) %>% select(CUS_ID, SITE) %>% distinct(SITE) %>% summarize(pagenum=n())


cs.tm = cstr %>% select(CUS_ID,TIME_ID,SITE_CNT,ST_TIME)
cs.tm[,TIME_ID:= as.numeric(TIME_ID)]
cs.tm$TIME_ID <- cs.tm$TIME_ID %% 100
cs.tm.1= cs.tm %>% melt(id.vars=c("CUS_ID","TIME_ID"), measure.vars=c("SITE_CNT")) %>%
  cast(CUS_ID~TIME_ID, sum, subset=variable=="SITE_CNT")
colnames(cs.tm.1) <- c("CUS_ID","tm_1.0","tm_1.1","tm_1.2","tm_1.3","tm_1.4","tm_1.5","tm_1.6",
                       "tm_1.7","tm_1.8","tm_1.9","tm_1.10","tm_1.11","tm_1.12","tm_1.13",
                       "tm_1.14","tm_1.15","tm_1.16","tm_1.17","tm_1.18","tm_1.19",
                       "tm_1.20","tm_1.21","tm_1.22","tm_1.23")

cs.time.1=cs.tm.1 %>% 
  mutate(tm1.1_8=tm_1.0+tm_1.1+tm_1.2+tm_1.3+tm_1.4+tm_1.5+tm_1.6+tm_1.7+tm_1.8)%>%
  mutate(tm1.10_17=tm_1.10+tm_1.11+tm_1.13+tm_1.14+tm_1.15+tm_1.16+tm_1.17) %>%
  mutate(tm1.22_2=tm_1.22+tm_1.23+tm_1.0+tm_1.1+tm_1.2) %>%
  mutate(tm1.5_8=tm_1.5+tm_1.6+tm_1.7+tm_1.8) %>% mutate(tm_1.13_14=tm_1.13+tm_1.14) 
cs.tm.2 = cs.tm.1
cs.tm.2[,-1] = 100*(cs.tm.2[,-1]/rowSums(cs.tm.2))
colnames(cs.tm.2) <- c("CUS_ID","tm_2.0","tm_2.1","tm_2.2","tm_2.3","tm_2.4","tm_2.5","tm_2.6",
                       "tm_2.7","tm_2.8","tm_2.9","tm_2.10","tm_2.11","tm_2.12","tm_2.13",
                       "tm_2.14","tm_2.15","tm_2.16","tm_2.17","tm_2.18","tm_2.19",
                       "tm_2.20","tm_2.21","tm_2.22","tm_2.23")
cs.time.2=cs.tm.2 %>%
  mutate(tm2.1_8=tm_2.0+tm_2.1+tm_2.2+tm_2.3+tm_2.4+tm_2.5+tm_2.6+tm_2.7+tm_2.8)%>%
  mutate(tm2.10_17=tm_2.10+tm_2.11+tm_2.13+tm_2.14+tm_2.15+tm_2.16+tm_2.17) %>%
  mutate(tm2.22_2=tm_2.22+tm_2.23+tm_2.0+tm_2.1+tm_2.2) %>%
  mutate(tm2.5_8=tm_2.5+tm_2.6+tm_2.7+tm_2.8) %>% mutate(tm_2.13_14=tm_2.13+tm_2.14) 


cs.tm.3= cs.tm %>% melt(id.vars=c("CUS_ID","TIME_ID"), measure.vars=c("ST_TIME")) %>%
  cast(CUS_ID~TIME_ID, sum, subset=variable=="ST_TIME")
cs.tm.4 = cs.tm.3
colnames(cs.tm.3) <- c("CUS_ID","tm_3.0","tm_3.1","tm_3.2","tm_3.3","tm_3.4","tm_3.5","tm_3.6",
                       "tm_3.7","tm_3.8","tm_3.9","tm_3.10","tm_3.11","tm_3.12","tm_3.13",
                       "tm_3.14","tm_3.15","tm_3.16","tm_3.17","tm_3.18","tm_3.19",
                       "tm_3.20","tm_3.21","tm_3.22","tm_3.23")
cs.time.3=cs.tm.3 %>%
  mutate(tm3.1_8=tm_3.0+tm_3.1+tm_3.2+tm_3.3+tm_3.4+tm_3.5+tm_3.6+tm_3.7+tm_3.8)%>%
  mutate(tm3.10_17=tm_3.10+tm_3.11+tm_3.13+tm_3.14+tm_3.15+tm_3.16+tm_3.17) %>%
  mutate(tm3.22_2=tm_3.22+tm_3.23+tm_3.0+tm_3.1+tm_3.2) %>%
  mutate(tm3.5_8=tm_3.5+tm_3.6+tm_3.7+tm_3.8) %>% mutate(tm_3.13_14=tm_3.13+tm_3.14) 
cs.tm.4[,-1] = 100*(cs.tm.4[,-1]/rowSums(cs.tm.4))
cs.tm.4[is.na(cs.tm.4)] <- 0
colnames(cs.tm.4) <- c("CUS_ID","tm_4.0","tm_4.1","tm_4.2","tm_4.3","tm_4.4","tm_4.5","tm_4.6",
                       "tm_4.7","tm_4.8","tm_4.9","tm_4.10","tm_4.11","tm_4.12","tm_4.13",
                       "tm_4.14","tm_4.15","tm_4.16","tm_4.17","tm_4.18","tm_4.19",
                       "tm_4.20","tm_4.21","tm_4.22","tm_4.23")
cs.time.4=cs.tm.4 %>%
  mutate(tm4.1_8=tm_4.0+tm_4.1+tm_4.2+tm_4.3+tm_4.4+tm_4.5+tm_4.6+tm_4.7+tm_4.8)%>%
  mutate(tm4.10_17=tm_4.10+tm_4.11+tm_4.13+tm_4.14+tm_4.15+tm_4.16+tm_4.17) %>%
  mutate(tm4.22_2=tm_4.22+tm_4.23+tm_4.0+tm_4.1+tm_4.2) %>%
  mutate(tm4.5_8=tm_4.5+tm_4.6+tm_4.7+tm_4.8) %>% mutate(tm_4.13_14=tm_4.13+tm_4.14) 


cstr$TIME_ID <- ymd_h(cstr$TIME_ID)
cstr$date <- date(cstr$TIME_ID)
cstr$year <- year(cstr$TIME_ID)
cstr$month <- month(cstr$TIME_ID)
cstr$day <- day(cstr$TIME_ID)
cstr$time <- hour(cstr$TIME_ID)
cstr$wkday <- wday(cstr$TIME_ID)

cs.V1 <- cstr %>% 
  group_by(CUS_ID) %>%
  summarize(ttl_pv = sum(SITE_CNT))


cs.V4 <- cstr %>%
  group_by(CUS_ID, date) %>%
  summarize(cstr = n()) %>%
  group_by(CUS_ID) %>%
  summarize(ttl_vis_day = n())


cs.V5.day <- cstr %>%
  group_by(CUS_ID, wkday) %>%
  summarize(pv_day = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ wkday, sum) %>%
  mutate(ttl_pv = cs.V1$ttl_pv)

cs.V5.day[,2:8] <- cs.V5.day[,2:8]/cs.V5.day[,9]
cs.V5.day <- cs.V5.day[,-9]
names(cs.V5.day) <- c("CUS_ID", "sun", "mon", "tue", "wed", "thu", "fri", "sat")


cs.V6.day <- cstr %>%
  group_by(CUS_ID, wkday) %>%
  summarize(pv_day = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ wkday, sum) %>%
  mutate(avg_day = rowMeans(.[2:8]), sd_day = rowSds(as.matrix(.[2:8]))) %>%
  mutate(coef_var_day = sd_day/avg_day) %>%
  select(CUS_ID, coef_var_day)

#custsig
custsig <- pf %>% 
  left_join(custsig1) %>%
  left_join(cs.tc) %>%
  left_join(cs.tt) %>%
  left_join(cs.npr) %>%
  left_join(cs.np) %>%
  left_join(cs.time.1) %>%
  left_join(cs.time.2) %>%
  left_join(cs.V1) %>%
  left_join(cs.V4) %>%
  left_join(cs.V5.day) %>%
  left_join(cs.V6.day)


names(custsig) <- gsub(" ", "", names(custsig))
names(custsig) <- gsub("/", "", names(custsig))

custsig[is.na(custsig)] <- 0
write.csv(custsig,"custsig.csv")
#change column names & NA to 0

#####
#test set
rm(list=ls())

#data
## train_sub
tr.dt.sub <- fread("test_clickstreams.tab"); tr.dt.sub[,CUS_ID:= as.numeric(CUS_ID)]
setkey(tr.dt.sub, CUS_ID) 

profsub =read.csv("test_public.csv", stringsAsFactors=F)

profsubtest= profsub %>%
  mutate(GROUP=ifelse(M20.==1,"M20-",
                      ifelse(M30==1,"M30",
                             ifelse(M40.==1,"M40+", 
                                    ifelse(F20.==1,"F20-",
                                           ifelse(F30==1,"F30",
                                                  ifelse(F40.==1,"F40+",NA))))))) %>% select(-F20.:-M40.)

# test data
tr.t.dt <- tr.dt.sub
tr.t.dt <- tr.t.dt[!(tr.t.dt$CUS_ID %in% profsubtest$CUS_ID)]
setkey(tr.t.dt, CUS_ID)

cstr <- tr.t.dt
rm(tr.dt.sub);rm(tr.t.dt);rm(profsub);rm(profsubtest)


cs.t1 = cstr %>% melt(id.vars=c("CUS_ID","ACT_NM"), measure.vars=c("ST_TIME")) %>%
  cast(CUS_ID~ACT_NM, sum, subset=variable=="ST_TIME")
cs.t2 = cs.t1
cs.t2[,-1] = 100*(cs.t2[,-1]/rowSums(cs.t2[,-1]))

cs.c1 = cstr %>% melt(id.vars=c("CUS_ID","ACT_NM"), measure.vars=c("SITE_CNT")) %>%
  cast(CUS_ID~ACT_NM, sum, subset=variable=="SITE_CNT")
cs.c2 = cs.c1
cs.c2[,-1] = 100*(cs.c2[,-1]/rowSums(cs.c2))

cs.v1= cs.t1 %>% left_join(cs.c1, by="CUS_ID", suffix=c(".t1",".c1"))
cs.v2= cs.t2 %>% left_join(cs.c2, by="CUS_ID", suffix=c(".t2",".c2"))

custsig1 = cs.v1 %>% left_join(cs.v2)
rm(cs.t1);rm(cs.t2);rm(cs.v1);rm(cs.v2);rm(cs.c1);rm(cs.c2)


cs.tc = cstr %>% group_by(CUS_ID) %>% select(CUS_ID, SITE_CNT) %>% summarize(total_cnt=sum(SITE_CNT))
cs.tt = cstr %>% group_by(CUS_ID) %>% select(CUS_ID, ST_TIME) %>% summarize(total_time=sum(ST_TIME))


cs.npr = cstr %>% group_by(CUS_ID) %>% select(CUS_ID, SITE) %>% summarize(reppagenum=n())
cs.np = cstr %>% group_by(CUS_ID) %>% select(CUS_ID, SITE) %>% distinct(SITE) %>% summarize(pagenum=n())


cs.tm = cstr %>% select(CUS_ID,TIME_ID,SITE_CNT,ST_TIME)
cs.tm[,TIME_ID:= as.numeric(TIME_ID)]
cs.tm$TIME_ID <- cs.tm$TIME_ID %% 100
cs.tm.1= cs.tm %>% melt(id.vars=c("CUS_ID","TIME_ID"), measure.vars=c("SITE_CNT")) %>%
  cast(CUS_ID~TIME_ID, sum, subset=variable=="SITE_CNT")
colnames(cs.tm.1) <- c("CUS_ID","tm_1.0","tm_1.1","tm_1.2","tm_1.3","tm_1.4","tm_1.5","tm_1.6",
                       "tm_1.7","tm_1.8","tm_1.9","tm_1.10","tm_1.11","tm_1.12","tm_1.13",
                       "tm_1.14","tm_1.15","tm_1.16","tm_1.17","tm_1.18","tm_1.19",
                       "tm_1.20","tm_1.21","tm_1.22","tm_1.23")

cs.time.1=cs.tm.1 %>% 
  mutate(tm1.1_8=tm_1.0+tm_1.1+tm_1.2+tm_1.3+tm_1.4+tm_1.5+tm_1.6+tm_1.7+tm_1.8)%>%
  mutate(tm1.10_17=tm_1.10+tm_1.11+tm_1.13+tm_1.14+tm_1.15+tm_1.16+tm_1.17) %>%
  mutate(tm1.22_2=tm_1.22+tm_1.23+tm_1.0+tm_1.1+tm_1.2) %>%
  mutate(tm1.5_8=tm_1.5+tm_1.6+tm_1.7+tm_1.8) %>% mutate(tm_1.13_14=tm_1.13+tm_1.14) 
cs.tm.2 = cs.tm.1
cs.tm.2[,-1] = 100*(cs.tm.2[,-1]/rowSums(cs.tm.2))
colnames(cs.tm.2) <- c("CUS_ID","tm_2.0","tm_2.1","tm_2.2","tm_2.3","tm_2.4","tm_2.5","tm_2.6",
                       "tm_2.7","tm_2.8","tm_2.9","tm_2.10","tm_2.11","tm_2.12","tm_2.13",
                       "tm_2.14","tm_2.15","tm_2.16","tm_2.17","tm_2.18","tm_2.19",
                       "tm_2.20","tm_2.21","tm_2.22","tm_2.23")
cs.time.2=cs.tm.2 %>%
  mutate(tm2.1_8=tm_2.0+tm_2.1+tm_2.2+tm_2.3+tm_2.4+tm_2.5+tm_2.6+tm_2.7+tm_2.8)%>%
  mutate(tm2.10_17=tm_2.10+tm_2.11+tm_2.13+tm_2.14+tm_2.15+tm_2.16+tm_2.17) %>%
  mutate(tm2.22_2=tm_2.22+tm_2.23+tm_2.0+tm_2.1+tm_2.2) %>%
  mutate(tm2.5_8=tm_2.5+tm_2.6+tm_2.7+tm_2.8) %>% mutate(tm_2.13_14=tm_2.13+tm_2.14) 


cs.tm.3= cs.tm %>% melt(id.vars=c("CUS_ID","TIME_ID"), measure.vars=c("ST_TIME")) %>%
  cast(CUS_ID~TIME_ID, sum, subset=variable=="ST_TIME")
cs.tm.4 = cs.tm.3
colnames(cs.tm.3) <- c("CUS_ID","tm_3.0","tm_3.1","tm_3.2","tm_3.3","tm_3.4","tm_3.5","tm_3.6",
                       "tm_3.7","tm_3.8","tm_3.9","tm_3.10","tm_3.11","tm_3.12","tm_3.13",
                       "tm_3.14","tm_3.15","tm_3.16","tm_3.17","tm_3.18","tm_3.19",
                       "tm_3.20","tm_3.21","tm_3.22","tm_3.23")
cs.time.3=cs.tm.3 %>%
  mutate(tm3.1_8=tm_3.0+tm_3.1+tm_3.2+tm_3.3+tm_3.4+tm_3.5+tm_3.6+tm_3.7+tm_3.8)%>%
  mutate(tm3.10_17=tm_3.10+tm_3.11+tm_3.13+tm_3.14+tm_3.15+tm_3.16+tm_3.17) %>%
  mutate(tm3.22_2=tm_3.22+tm_3.23+tm_3.0+tm_3.1+tm_3.2) %>%
  mutate(tm3.5_8=tm_3.5+tm_3.6+tm_3.7+tm_3.8) %>% mutate(tm_3.13_14=tm_3.13+tm_3.14) 
cs.tm.4[,-1] = 100*(cs.tm.4[,-1]/rowSums(cs.tm.4))
cs.tm.4[is.na(cs.tm.4)] <- 0
colnames(cs.tm.4) <- c("CUS_ID","tm_4.0","tm_4.1","tm_4.2","tm_4.3","tm_4.4","tm_4.5","tm_4.6",
                       "tm_4.7","tm_4.8","tm_4.9","tm_4.10","tm_4.11","tm_4.12","tm_4.13",
                       "tm_4.14","tm_4.15","tm_4.16","tm_4.17","tm_4.18","tm_4.19",
                       "tm_4.20","tm_4.21","tm_4.22","tm_4.23")
cs.time.4=cs.tm.4 %>%
  mutate(tm4.1_8=tm_4.0+tm_4.1+tm_4.2+tm_4.3+tm_4.4+tm_4.5+tm_4.6+tm_4.7+tm_4.8)%>%
  mutate(tm4.10_17=tm_4.10+tm_4.11+tm_4.13+tm_4.14+tm_4.15+tm_4.16+tm_4.17) %>%
  mutate(tm4.22_2=tm_4.22+tm_4.23+tm_4.0+tm_4.1+tm_4.2) %>%
  mutate(tm4.5_8=tm_4.5+tm_4.6+tm_4.7+tm_4.8) %>% mutate(tm_4.13_14=tm_4.13+tm_4.14) 


cstr$TIME_ID <- ymd_h(cstr$TIME_ID)
cstr$date <- date(cstr$TIME_ID)
cstr$year <- year(cstr$TIME_ID)
cstr$month <- month(cstr$TIME_ID)
cstr$day <- day(cstr$TIME_ID)
cstr$time <- hour(cstr$TIME_ID)
cstr$wkday <- wday(cstr$TIME_ID)

cs.V1 <- cstr %>% 
  group_by(CUS_ID) %>%
  summarize(ttl_pv = sum(SITE_CNT))


cs.V4 <- cstr %>%
  group_by(CUS_ID, date) %>%
  summarize(cstr = n()) %>%
  group_by(CUS_ID) %>%
  summarize(ttl_vis_day = n())


cs.V5.day <- cstr %>%
  group_by(CUS_ID, wkday) %>%
  summarize(pv_day = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ wkday, sum) %>%
  mutate(ttl_pv = cs.V1$ttl_pv)

cs.V5.day[,2:8] <- cs.V5.day[,2:8]/cs.V5.day[,9]
cs.V5.day <- cs.V5.day[,-9]
names(cs.V5.day) <- c("CUS_ID", "sun", "mon", "tue", "wed", "thu", "fri", "sat")


cs.V6.day <- cstr %>%
  group_by(CUS_ID, wkday) %>%
  summarize(pv_day = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ wkday, sum) %>%
  mutate(avg_day = rowMeans(.[2:8]), sd_day = rowSds(as.matrix(.[2:8]))) %>%
  mutate(coef_var_day = sd_day/avg_day) %>%
  select(CUS_ID, coef_var_day)

#custsig
custsig <- custsig1 %>%
  left_join(cs.tc) %>%
  left_join(cs.tt) %>%
  left_join(cs.npr) %>%
  left_join(cs.np) %>%
  left_join(cs.time.1) %>%
  left_join(cs.time.2) %>%
  left_join(cs.V1) %>%
  left_join(cs.V4) %>%
  left_join(cs.V5.day) %>%
  left_join(cs.V6.day)


names(custsig) <- gsub(" ", "", names(custsig))
names(custsig) <- gsub("/", "", names(custsig))

custsig[is.na(custsig)] <- 0
write.csv(custsig,"custsig_test.csv")

#####
#feature selection
rm(list=ls())
library(caret)
library(dplyr)
library(xgboost)
library(plyr)
library(randomForest)

sig <- read.csv("custsig.csv", stringsAsFactors=F)
sig = sig %>% mutate(weekend=sun+sat)
sig = sig[,-1]
sigsubmit <- read.csv("custsig_test.csv", stringsAsFactors=F)
sigsubmit = sigsubmit %>% mutate(weekend=sun+sat)
sigsubmit = sigsubmit[,-1]

cb.train=sig
cb.test= sigsubmit

#####
#model train
cb.train$gr <- factor(cb.train$gr)
cb.train$gr = paste("C",cb.train$gr,sep="")

cb3.train = cb.train %>%
  select(CUS_ID,gr,#sex
         IT뉴스.c2,자동차정보.t2,컨텐츠공유.P2P..c2,자동차제조.c2,외국신문잡지.c2,
         자동차제조.t2,내비게이션GPS.c2,tm2.5_8,종합인터넷신문.t2,weekend,종합일간지.t2,
         종합가격비교.t2,방송뉴스.t2,경제신문.c2,tm2.1_8,tm_2.7,자동차브랜드.c2,대통령선거.c2,
         중고차쇼핑몰.c2,포털뉴스.t2,게임웹진.t2,기계장비B2B.c2,
         여성의류쇼핑몰.c2,tm_2.13_14,연예인의류쇼핑몰.c2,여성화전문몰.c2,커뮤니티포털.t2,
         포털블로그.t2,종합쇼핑몰.t2,coef_var_day,tm2.10_17,브랜드여성의류쇼핑몰.c2,
         화장품브랜드쇼핑몰.t2,종합의류쇼핑몰.c2,출산육아정보.t2,화장품브랜드업체.t2,온라인사진인화.c2,
         소셜허브.t2,커피전문점.c2,포털게시판.t2,패션의류전문지.t2,tue,패밀리레스토랑.t2,유아용품제조.t2,
         종합일간지.t1,방송뉴스.t1,스포츠신문.c1,대통령선거.c1,자동차브랜드.t1,컴퓨터장치주변기기.c1,
         경제신문.c1,게임웹진.t1,기계장비B2B.c1,자동차전문지.c1,출산육아정보.t1,의약제조.c1,패밀리레스토랑.c1,
         홈인테리어용품쇼핑몰.t1,
         #age 20_30
         여성의류쇼핑몰.t1,장학재단.c1,군대.t1,SNS.c1,남성의류쇼핑몰.c1,캐주얼웨어브랜드.c1,
         연예인의류쇼핑몰.c1,속옷쇼핑몰.c1,스포츠의류쇼핑몰.c1,선글라스쇼핑몰.t1,콘텍트렌즈.c1,
         고시자격증학원.c1,콘텍트렌즈.t1,메신저.c1,선물디자인소품쇼핑몰.c1,속옷제조.t1,쇼핑몰솔루션.c1,
         화장품브랜드업체.t1,시험자격증교육.종합..c1,행정민원.c1,시중은행.t1,인터넷납부.c1,
         대형마트쇼핑몰.c1,포털쇼핑.t1,전기가스석유유통.t1,생명보험.c1,포털어린이.c1,공공질서안전.t1,
         고객지원센터.c1,자동차보험.c1,관광농원휴양지.t1,전자결제전자화폐.c1,홈인테리어용품쇼핑몰.t1,
         SNS.c2,군대.c2,남성의류쇼핑몰.c2,장학재단.c2,포털만화.t2,고시자격증학원.t2,
         메신저.c2,캐주얼웨어브랜드.c2,패션몰링크검색.t2,영화평리뷰.c2,액션슈팅.c2,종합대학교.국내..c2,
         종합신발쇼핑몰.c2,시험자격증교육.종합..c2,속옷쇼핑몰.t2,오픈마켓.c2,시중은행.c2,행정민원.c2,
         대형마트쇼핑몰.t2,인터넷납부.c2,신용카드.c2,자동차보험.t2,포털어린이.c2,종합포털.t2,
         전자결제전자화폐.c2,공단.c2,tm1.5_8,포털지도지역정보.c2,아동복쇼핑몰.c2,
         포털쇼핑.t2,포털부동산.c2,대형마트SSM.t2,
         #age20_40
         종합신발쇼핑몰.c1,커뮤니티포털.t1,브랜드여성의류쇼핑몰.t1,포털검색.c1,종합의류쇼핑몰.c1,
         여성화전문몰.c1,배달음식쇼핑몰.t1,미용실.t1,게임커뮤니티.c1,SPA브랜드.c1,
         헤어미용쇼핑몰.c1,빅사이즈의류쇼핑몰.t1,폰트서체.t1,광고대행사.c1,
         웹디자인그래픽.t1,패션미용정보.t1,기타패션잡화쇼핑몰.c1,비만클리닉.c1,
         화장품브랜드쇼핑몰.t1,아이스크림프랜차이즈.c1,그룹사기업채용.c1,명품쇼핑몰.c1,모바일게임.t1,
         여성의류브랜드.c1,패션쇼핑타운.c1,대기업그룹사.c1,스키스노우보드.c1,웹에이전시.c1,성형외과.t1,
         커피음료프랜차이즈.c1,포털블로그.c1,영화평리뷰.c1,시험자격증교육.단과..c1,포털게시판.t1,
         이미지클립아트.t1,인터넷광고대행사미디어랩.c1,피자스파게티.t1,패스트푸드프랜차이즈.c1,커플쇼핑몰.t1,
         게임인터넷방송.t1,브랜드남성의류쇼핑몰.t1,로그분석카운터.t1,섬유패션B2B.t1,포털만화.t1,
         공연정보.c1,SPA브랜드쇼핑몰.c1,체형피부관리.c1,기타교육기관.c1,수능대학입시.t1,
         모바일솔루션개발.c1,해외쇼핑대행.t1,쇼핑몰구축.c1,음악감상.c1,패션의류전문지.t1,
         브랜드청바지쇼핑몰.c1,화장품제조.t1,여행용품쇼핑몰.t1,외국신문잡지.c1,종합인터넷신문.t1,
         포털뉴스.t1,종합포털.t1,내비게이션GPS.t1,종합가격비교.c1,IT뉴스.c1,공단.t1,
         사법기관.c1,언론사블로그.t1,광역단체.t1,포털부동산.c1,대형마트쇼핑몰.t1,
         AS센터.t1,부동산종합정보.t1,부동산경매.t1,지역뉴스.t1,전문부동산정보.c1,pagenum,
         포털검색.c2,배달음식쇼핑몰.t2,스포츠의류쇼핑몰.c2,기능화장품쇼핑몰.t2,소셜커머스.t2,
         선글라스쇼핑몰.t2,선물디자인소품쇼핑몰.c2,미용실.c2,게임커뮤니티.t2,헤어미용쇼핑몰.c2,
         쇼핑몰솔루션.c2,tm2.22_2,빅사이즈의류쇼핑몰.t2,SPA브랜드.c2,아이템거래.t2,
         그룹사기업채용.c2,명품쇼핑몰.c2,로그분석카운터.t2,패션미용정보.c2,인터넷광고대행사미디어랩.t2,
         상품권쇼핑몰.t2,토렌트정보.t2,tm_2.2,SPA브랜드쇼핑몰.t2,수능대학입시.c2,면세점.t2,MMORPG.t2,
         coef_var_day,브랜드남성의류쇼핑몰.c2,패스트푸드프랜차이즈.c2,아이스크림프랜차이즈.c2,
         기타교육기관.c2,안과.c2,이미지클립아트.t2,게임인터넷방송.t2,기타쇼핑몰.t2,쇼핑몰구축.c2,
         스키스노우보드.t2,음식점프랜차이즈.c2,호텔.c2,폰트서체.t2,광고대행사.t2,
         패션쇼핑타운.c2,피자스파게티.t2,섬유패션B2B.t2,휴대폰악세사리쇼핑몰.c1,
         여행용품쇼핑몰.t2,게임정보.t2,방송뉴스.c2,언론사블로그.t2,위성케이블채널.c2,스포츠신문.c2,
         ttl_vis_day,사법기관.c2,부동산경매.c2,생명보험.t2,전기가스석유유통.t2,기초단체.t2,지역뉴스.t2,통신사.t2,
         #age 30_40
         오픈마켓.c1,종합신발쇼핑몰.t1,임부복쇼핑몰.c1,소셜커머스.c1,우유유제품제조.c1,백화점.c1,
         온라인사진인화.t1,가방쇼핑몰.c1,신용카드.c1,사무용품쇼핑몰.c1,유아용품쇼핑몰.t1,전자결제솔루션.t1,
         유아용품제조.t1,해외배송대행.c1,사진관스튜디오.c1,치킨프랜차이즈.t1,커피전문점.c1,
         기능화장품쇼핑몰.t1,파티용품쇼핑몰.c1,통신사.t1)

cb3.test = cb.test %>%
  select(CUS_ID, #sex
         IT뉴스.c2,자동차정보.t2,컨텐츠공유.P2P..c2,자동차제조.c2,외국신문잡지.c2,
         자동차제조.t2,내비게이션GPS.c2,tm2.5_8,종합인터넷신문.t2,weekend,종합일간지.t2,
         종합가격비교.t2,방송뉴스.t2,경제신문.c2,tm2.1_8,tm_2.7,자동차브랜드.c2,대통령선거.c2,
         중고차쇼핑몰.c2,포털뉴스.t2,게임웹진.t2,기계장비B2B.c2,
         여성의류쇼핑몰.c2,tm_2.13_14,연예인의류쇼핑몰.c2,여성화전문몰.c2,커뮤니티포털.t2,
         포털블로그.t2,종합쇼핑몰.t2,coef_var_day,tm2.10_17,브랜드여성의류쇼핑몰.c2,
         화장품브랜드쇼핑몰.t2,종합의류쇼핑몰.c2,출산육아정보.t2,화장품브랜드업체.t2,온라인사진인화.c2,
         소셜허브.t2,커피전문점.c2,포털게시판.t2,패션의류전문지.t2,tue,패밀리레스토랑.t2,유아용품제조.t2,
         종합일간지.t1,방송뉴스.t1,스포츠신문.c1,대통령선거.c1,자동차브랜드.t1,컴퓨터장치주변기기.c1,
         경제신문.c1,게임웹진.t1,기계장비B2B.c1,자동차전문지.c1,출산육아정보.t1,의약제조.c1,패밀리레스토랑.c1,
         홈인테리어용품쇼핑몰.t1,
         #age 20_30
         여성의류쇼핑몰.t1,장학재단.c1,군대.t1,SNS.c1,남성의류쇼핑몰.c1,캐주얼웨어브랜드.c1,
         연예인의류쇼핑몰.c1,속옷쇼핑몰.c1,스포츠의류쇼핑몰.c1,선글라스쇼핑몰.t1,콘텍트렌즈.c1,
         고시자격증학원.c1,콘텍트렌즈.t1,메신저.c1,선물디자인소품쇼핑몰.c1,속옷제조.t1,쇼핑몰솔루션.c1,
         화장품브랜드업체.t1,시험자격증교육.종합..c1,행정민원.c1,시중은행.t1,인터넷납부.c1,
         대형마트쇼핑몰.c1,포털쇼핑.t1,전기가스석유유통.t1,생명보험.c1,포털어린이.c1,공공질서안전.t1,
         고객지원센터.c1,자동차보험.c1,관광농원휴양지.t1,전자결제전자화폐.c1,홈인테리어용품쇼핑몰.t1,
         SNS.c2,군대.c2,남성의류쇼핑몰.c2,장학재단.c2,포털만화.t2,고시자격증학원.t2,
         메신저.c2,캐주얼웨어브랜드.c2,패션몰링크검색.t2,영화평리뷰.c2,액션슈팅.c2,종합대학교.국내..c2,
         종합신발쇼핑몰.c2,시험자격증교육.종합..c2,속옷쇼핑몰.t2,오픈마켓.c2,시중은행.c2,행정민원.c2,
         대형마트쇼핑몰.t2,인터넷납부.c2,신용카드.c2,자동차보험.t2,포털어린이.c2,종합포털.t2,
         전자결제전자화폐.c2,공단.c2,tm1.5_8,포털지도지역정보.c2,아동복쇼핑몰.c2,
         포털쇼핑.t2,포털부동산.c2,대형마트SSM.t2,
         #age20_40
         종합신발쇼핑몰.c1,커뮤니티포털.t1,브랜드여성의류쇼핑몰.t1,포털검색.c1,종합의류쇼핑몰.c1,
         여성화전문몰.c1,배달음식쇼핑몰.t1,미용실.t1,게임커뮤니티.c1,SPA브랜드.c1,
         헤어미용쇼핑몰.c1,빅사이즈의류쇼핑몰.t1,폰트서체.t1,광고대행사.c1,
         웹디자인그래픽.t1,패션미용정보.t1,기타패션잡화쇼핑몰.c1,비만클리닉.c1,
         화장품브랜드쇼핑몰.t1,아이스크림프랜차이즈.c1,그룹사기업채용.c1,명품쇼핑몰.c1,모바일게임.t1,
         여성의류브랜드.c1,패션쇼핑타운.c1,대기업그룹사.c1,스키스노우보드.c1,웹에이전시.c1,성형외과.t1,
         커피음료프랜차이즈.c1,포털블로그.c1,영화평리뷰.c1,시험자격증교육.단과..c1,포털게시판.t1,
         이미지클립아트.t1,인터넷광고대행사미디어랩.c1,피자스파게티.t1,패스트푸드프랜차이즈.c1,커플쇼핑몰.t1,
         게임인터넷방송.t1,브랜드남성의류쇼핑몰.t1,로그분석카운터.t1,섬유패션B2B.t1,포털만화.t1,
         공연정보.c1,SPA브랜드쇼핑몰.c1,체형피부관리.c1,기타교육기관.c1,수능대학입시.t1,
         모바일솔루션개발.c1,해외쇼핑대행.t1,쇼핑몰구축.c1,음악감상.c1,패션의류전문지.t1,
         브랜드청바지쇼핑몰.c1,화장품제조.t1,여행용품쇼핑몰.t1,외국신문잡지.c1,종합인터넷신문.t1,
         포털뉴스.t1,종합포털.t1,내비게이션GPS.t1,종합가격비교.c1,IT뉴스.c1,공단.t1,
         사법기관.c1,언론사블로그.t1,광역단체.t1,포털부동산.c1,대형마트쇼핑몰.t1,
         AS센터.t1,부동산종합정보.t1,부동산경매.t1,지역뉴스.t1,전문부동산정보.c1,pagenum,
         포털검색.c2,배달음식쇼핑몰.t2,스포츠의류쇼핑몰.c2,기능화장품쇼핑몰.t2,소셜커머스.t2,
         선글라스쇼핑몰.t2,선물디자인소품쇼핑몰.c2,미용실.c2,게임커뮤니티.t2,헤어미용쇼핑몰.c2,
         쇼핑몰솔루션.c2,tm2.22_2,빅사이즈의류쇼핑몰.t2,SPA브랜드.c2,아이템거래.t2,
         그룹사기업채용.c2,명품쇼핑몰.c2,로그분석카운터.t2,패션미용정보.c2,인터넷광고대행사미디어랩.t2,
         상품권쇼핑몰.t2,토렌트정보.t2,tm_2.2,SPA브랜드쇼핑몰.t2,수능대학입시.c2,면세점.t2,MMORPG.t2,
         coef_var_day,브랜드남성의류쇼핑몰.c2,패스트푸드프랜차이즈.c2,아이스크림프랜차이즈.c2,
         기타교육기관.c2,안과.c2,이미지클립아트.t2,게임인터넷방송.t2,기타쇼핑몰.t2,쇼핑몰구축.c2,
         스키스노우보드.t2,음식점프랜차이즈.c2,호텔.c2,폰트서체.t2,광고대행사.t2,
         패션쇼핑타운.c2,피자스파게티.t2,섬유패션B2B.t2,휴대폰악세사리쇼핑몰.c1,
         여행용품쇼핑몰.t2,게임정보.t2,방송뉴스.c2,언론사블로그.t2,위성케이블채널.c2,스포츠신문.c2,
         ttl_vis_day,사법기관.c2,부동산경매.c2,생명보험.t2,전기가스석유유통.t2,기초단체.t2,지역뉴스.t2,통신사.t2,
         #age 30_40
         오픈마켓.c1,종합신발쇼핑몰.t1,임부복쇼핑몰.c1,소셜커머스.c1,우유유제품제조.c1,백화점.c1,
         온라인사진인화.t1,가방쇼핑몰.c1,신용카드.c1,사무용품쇼핑몰.c1,유아용품쇼핑몰.t1,전자결제솔루션.t1,
         유아용품제조.t1,해외배송대행.c1,사진관스튜디오.c1,치킨프랜차이즈.t1,커피전문점.c1,
         기능화장품쇼핑몰.t1,파티용품쇼핑몰.c1,통신사.t1)
names(cb3.train)
group<-fread("train_profiles.csv")
test_pulic<-fread("test_public.csv")
test_pulic$GROUP<-ifelse(test_pulic$`F20-`==1,"F20-",
                         ifelse(test_pulic$F30==1,"F30",
                                ifelse(test_pulic$`F40+`==1,"F40+",
                                       ifelse(test_pulic$`M20-`,"M20-",
                                              ifelse(test_pulic$M30,"M30","M40+")))))
group<-rbind(group,test_pulic[,c(1,8)])
cb3.train<-merge(group,cb3.train,by="CUS_ID")
cb3.train<-cb3.train[,-3]
write.csv(cb3.train,"master1_train.csv",row.names = F)
write.csv(cb3.test,"maste1_test.csv",row.names = F)

####모델생성####
rm(list=ls())
library(caret)
cb3.train<-read.csv("master1_train.csv")
cb3.test<-read.csv("maste1_test.csv")

cb3.train$GROUP<-make.names(cb3.train$GROUP)
model_xgb_null3 <-train(GROUP ~ .-CUS_ID,
                        data=cb3.train,
                        method="xgbTree",
                        metric = "logLoss",
                        trControl = trainControl(method = "repeatedcv", number = 3, 
                                                 repeats = 3, classProbs=T, 
                                                 summaryFunction=mnLogLoss, 
                                                 verboseIter = FALSE))

summary(model_xgb_null3$results$logLoss)

pred<-data.frame(CUS_ID=cb3.test$CUS_ID,predict(model_xgb_null3,cb3.test,type="prob"))
names(pred)<-c("CUS_ID","F20-","F30","F40+","M20-","M30","M40+")
write.csv(pred,"master1_submission_xgb_1.318.csv",row.names = F)



#######w2v 새로 생성########
#####워드투백 이용하기######
setwd("C:\\Users\\Hyangsuk_Min\\Desktop\\데마팀플\\원데이터")
rm(list=ls())
# Install & load word2vec package
if(!require(devtools)) install.packages("devtools"); library(devtools)
if(!require(wordVectors)) install_github("bmschmidt/wordVectors"); library(wordVectors)

# Install & load data.table package
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(randomForest)) install.packages("randomForest"); library(randomForest)
if(!require(caret)) install.packages("caret"); library(caret)

# list objects in word2vec package
ls("package:wordVectors")


###소분류 워투백####
####트레이
###### Fast reading and combining several files using data.table (with fread): 5 times faster than read.csv()
cs.dt <- fread("train_profiles.csv")
tr.dt <- fread("train_clickstreams.tab"); tr.dt[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt, CUS_ID); setkey(tr.dt, CUS_ID) 
md.dt <- merge(cs.dt, tr.dt)

test_pulic<-fread("test_public.csv")
test_pulic$GROUP<-ifelse(test_pulic$`F20-`==1,"F20-",
                         ifelse(test_pulic$F30==1,"F30",
                                ifelse(test_pulic$`F40+`==1,"F40+",
                                       ifelse(test_pulic$`M20-`,"M20-",
                                              ifelse(test_pulic$M30,"M30","M40+")))))
tr.dt2<-fread("test_clickstreams.tab"); tr.dt2[,CUS_ID:=as.numeric(CUS_ID)]
setkey(test_pulic,CUS_ID);setkey(tr.dt2,CUS_ID)
md.dt2<-merge(test_pulic[,c(1,8)],tr.dt2)
md.dt<-rbind(md.dt,md.dt2)

cus<-unique(md.dt$CUS_ID)

####벡터생성
model <- read.binary.vectors("vec-act_nm.bin") # reload the model.7조 변수 생성시 사용했던 모델 그대로 사용. 
df<-data.frame()
for (i in cus ){
  itemfreq <- table(md.dt[CUS_ID==i, ACT_NM])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,model[[names(fitems), average=T]])
  df<-rbind(df,sim)
}
cus_gender<-rbind(cs.dt,test_pulic[,c(1,8)])
w2v_act<-df
w2v_act_train<-merge(cus_gender,by="CUS_ID",w2v_act)

write.csv(w2v_act_train,"w2v_act_mean_train.csv",row.names = F)

#####테스트적용#####
tr.dt2<-tr.dt2[!(tr.dt2$CUS_ID %in% cus),]
cus2<-unique(tr.dt2$CUS_ID)
df<-data.frame()
for (i in cus2 ){
  itemfreq <- table(tr.dt2[CUS_ID==i, ACT_NM])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,model[[names(fitems), average=T]])
  df<-rbind(df,sim)
}

w2v_act_test<-df

write.csv(w2v_act_test,"w2v_act_mean_test.csv",row.names = F)

#####
#####워드투백 이용하기######

rm(list=ls())
# Install & load word2vec package
if(!require(devtools)) install.packages("devtools"); library(devtools)
if(!require(wordVectors)) install_github("bmschmidt/wordVectors"); library(wordVectors)

# Install & load data.table package
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(randomForest)) install.packages("randomForest"); library(randomForest)
if(!require(caret)) install.packages("caret"); library(caret)

# list objects in word2vec package
ls("package:wordVectors")


###사이트 워투백####
####트레이
###### Fast reading and combining several files using data.table (with fread): 5 times faster than read.csv()
cs.dt <- fread("train_profiles.csv")
tr.dt <- fread("train_clickstreams.tab"); tr.dt[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt, CUS_ID); setkey(tr.dt, CUS_ID) 
md.dt <- merge(cs.dt, tr.dt)

test_pulic<-fread("test_public.csv")
test_pulic$GROUP<-ifelse(test_pulic$`F20-`==1,"F20-",
                         ifelse(test_pulic$F30==1,"F30",
                                ifelse(test_pulic$`F40+`==1,"F40+",
                                       ifelse(test_pulic$`M20-`,"M20-",
                                              ifelse(test_pulic$M30,"M30","M40+")))))
tr.dt2<-fread("test_clickstreams.tab"); tr.dt2[,CUS_ID:=as.numeric(CUS_ID)]
setkey(test_pulic,CUS_ID);setkey(tr.dt2,CUS_ID)
md.dt2<-merge(test_pulic[,c(1,8)],tr.dt2)
md.dt<-rbind(md.dt,md.dt2)

cus<-unique(md.dt$CUS_ID)

####벡터생성
model <- read.binary.vectors("vec-site_nm.bin") # reload the model. 
df<-data.frame()
for (i in cus ){
  itemfreq <- table(md.dt[CUS_ID==i, SITE_NM])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,model[[names(fitems), average=T]])
  df<-rbind(df,sim)
}
cus_gender<-rbind(cs.dt,test_pulic[,c(1,8)])

w2v_site<-df

w2v_site_train<-merge(cus_gender,by="CUS_ID",w2v_site)

write.csv(w2v_site_train,"w2v_site_mean_train.csv",row.names = F)

#####테스트적용#####
tr.dt2<-tr.dt2[!(tr.dt2$CUS_ID %in% cus),]
cus2<-unique(tr.dt2$CUS_ID)
df<-data.frame()
for (i in cus2 ){
  itemfreq <- table(tr.dt2[CUS_ID==i, SITE_NM])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,model[[names(fitems), average=T]])
  df<-rbind(df,sim)
}

w2v_site_test<-df

write.csv(w2v_site_test,"w2v_site_mean_test.csv",row.names = F)

trf1<-read.csv("w2v_act_mean_train.csv")
trf2<-read.csv("w2v_site_mean_train.csv")
names(trf2)[-1:-2]<-paste("X",301:600,sep="")
tef1<-read.csv("w2v_act_mean_test.csv")
tef2<-read.csv("w2v_site_mean_test.csv")
names(tef2)[-1]<-paste("X",301:600,sep="")

trf<-merge(trf1,trf2,by="CUS_ID")
tef<-merge(tef1,tef2,by="CUS_ID")
trf[,303]<-NULL
names(trf)[2]<-"GROUP"
write.csv(trf,"train_final_w2v_all.csv",row.names = F)
write.csv(tef,"test_final_w2v_all.csv",row.names = F)


#### Training & Prediction####
# Control parameters for model training
rm(list=ls())
gc()
train<-read.csv("train_final_w2v_all.csv")
test<-read.csv("test_final_w2v_all.csv")

str(train)
str(test)
train$GROUP<-make.names(train$GROUP)
control <- trainControl(method="cv", number=5, repeats=1, classProbs=TRUE, summaryFunction=mnLogLoss)

# Used models
methods <- c("gbm", "nnet") # add methods such as xgbTree, rf, svmRadious, etc.

models <- list()
for (i in 1:length(methods)) {
  set.seed(123)
  model <- train(GROUP ~ .,
                 data = subset(train, select=-CUS_ID),
                 method = methods[i],
                 preProcess = NULL,
                 metric = "logLoss",
                 trControl = control)
  models[[i]] <- model
}
names(models) <- methods
# Saving and loading models: 
saveRDS(models, "models.rds") 
# models <- readRDS("models.rds")

# Model comparison
#results <- resamples(models)
#summary(results)
#xyplot(results)
#modelCor(results)
#splom(results)

# prediction & submission
for (i in 1:length(methods)) {
  pred <- predict(models[i], test, type="prob")
  fname <- paste("submission_st+act", methods[i], ".csv", sep="")
  write.csv(cbind(CUS_ID=test$CUS_ID,pred[[1]]), fname, row.names = F)
}

#########MASTER2#####
####Candidate Features####
setwd("D:/r/데마")
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape)
library(caret)
library(ROCR)
library(matrixStats)
rm(list=ls())
cs <- read.delim("train_clickstreams.tab", stringsAsFactors = F)
sk <- read.delim("train_searchkeywords.tab", stringsAsFactors = F)
profile <- read.csv("train_profiles.csv", stringsAsFactors = F, header = T)

cs_t <- read.delim("test_clickstreams.tab", stringsAsFactors = F)
sk_t <- read.delim("test_searchkeywords.tab", stringsAsFactors = F)

#########Module 1 : Create Training Set#########

####총 페이지뷰 횟수
cs.V1 <- cs %>% 
  group_by(CUS_ID) %>%
  summarize(ttl_pv = sum(SITE_CNT))

####웹사이트 카테고리별 페이지뷰 비율
cs.V2.1 <- cs %>%
  group_by(CUS_ID, BACT_NM) %>%
  summarize(cs = n()) %>%
  cast(CUS_ID ~ BACT_NM, sum) %>%
  mutate(ttl_pv = cs.V1$ttl_pv)

cs.V2.1[,2:23] <- cs.V2.1[,2:23]/cs.V2.1[,24]
cs.V2.1 <- cs.V2.1[,-24]
names(cs.V2.1)[-1] <- paste("pv", names(cs.V2.1)[-1], sep="_")

cs.V2.2 <- cs %>%
  group_by(CUS_ID, MACT_NM) %>%
  summarize(cs = n()) %>%
  cast(CUS_ID ~ MACT_NM, sum) %>%
  mutate(ttl_pv = cs.V1$ttl_pv)

cs.V2.2[,2:208] <- cs.V2.2[,2:208]/cs.V2.2[,209]
cs.V2.2 <- cs.V2.2[,-209]
names(cs.V2.2)[-1] <- paste("mact", names(cs.V2.2)[-1], sep="_")

cs.V2 <- inner_join(cs.V2.1, cs.V2.2, by=("CUS_ID"))

####카테고리에 대한 변동계수
cs.V3 <- cs %>%
  group_by(CUS_ID, BACT_NM) %>%
  summarize(cs = n()) %>%
  cast(CUS_ID ~ BACT_NM, sum) %>%
  mutate(avg_pv = rowMeans(.[2:23]), sd_pv = rowSds(as.matrix(.[2:23]))) %>%
  mutate(coef_var_cat = sd_pv/avg_pv) %>%
  select(CUS_ID, coef_var_cat)

####날짜계산용 전처리
cs$TIME_ID <- ymd_h(cs$TIME_ID)
cs$date <- date(cs$TIME_ID)
cs$year <- year(cs$TIME_ID)
cs$month <- month(cs$TIME_ID)
cs$day <- day(cs$TIME_ID)
cs$time <- hour(cs$TIME_ID)
cs$wkday <- wday(cs$TIME_ID)

####총 방문 일수
cs.V4 <- cs %>%
  group_by(CUS_ID, date) %>%
  summarize(cs = n()) %>%
  group_by(CUS_ID) %>%
  summarize(ttl_vis_day = n())

####월별, 요일별, 시간대별 페이지뷰 비율 
#월별
cs.V5.mth <- cs %>%
  group_by(CUS_ID, month) %>%
  summarize(pv_mth = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ month, sum) %>%
  mutate(ttl_pv = cs.V1$ttl_pv)

cs.V5.mth[,2:13] <- cs.V5.mth[,2:13]/cs.V5.mth[,14]
cs.V5.mth <- cs.V5.mth[,-14]
names(cs.V5.mth)[-1] <- paste("mth", names(cs.V5.mth)[-1], sep="_")

#요일별
cs.V5.day <- cs %>%
  group_by(CUS_ID, wkday) %>%
  summarize(pv_day = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ wkday, sum) %>%
  mutate(ttl_pv = cs.V1$ttl_pv)

cs.V5.day[,2:8] <- cs.V5.day[,2:8]/cs.V5.day[,9]
cs.V5.day <- cs.V5.day[,-9]
names(cs.V5.day) <- c("CUS_ID", "sun", "mon", "tue", "wed", "thu", "fri", "sat")

#시간대별
cs.V5.time <- cs %>%
  group_by(CUS_ID, time) %>%
  summarize(pv_time = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ time, sum) %>%
  mutate(ttl_pv = cs.V1$ttl_pv)

cs.V5.time[,2:25] <- cs.V5.time[,2:25]/cs.V5.time[,26]
cs.V5.time <- cs.V5.time[,-26]
names(cs.V5.time)[-1] <- paste("time", names(cs.V5.time)[-1], sep="_")

#merge
cs.V5 <- inner_join(cs.V5.mth, cs.V5.day, by=("CUS_ID")) %>%
  left_join(cs.V5.time, by="CUS_ID")

####월, 요일, 시간대에 대한 변동계수
#월별
cs.V6.mth <- cs %>%
  group_by(CUS_ID, month) %>%
  summarize(pv_mth = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ month, sum) %>%
  mutate(avg_mth = rowMeans(.[2:13]), sd_mth = rowSds(as.matrix(.[2:13]))) %>%
  mutate(coef_var_mth = sd_mth/avg_mth) %>%
  select(CUS_ID, coef_var_mth)

#요일별
cs.V6.day <- cs %>%
  group_by(CUS_ID, wkday) %>%
  summarize(pv_day = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ wkday, sum) %>%
  mutate(avg_day = rowMeans(.[2:8]), sd_day = rowSds(as.matrix(.[2:8]))) %>%
  mutate(coef_var_day = sd_day/avg_day) %>%
  select(CUS_ID, coef_var_day)

#시간대별
cs.V6.time <- cs %>%
  group_by(CUS_ID, time) %>%
  summarize(pv_time = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ time, sum) %>%
  mutate(avg_time = rowMeans(.[2:25]), sd_time = rowSds(as.matrix(.[2:25]))) %>%
  mutate(coef_var_time = sd_time/avg_time) %>%
  select(CUS_ID, coef_var_time)

#merge
cs.V6 <- inner_join(cs.V6.mth, cs.V6.day, by=("CUS_ID")) %>%
  left_join(cs.V6.time, by="CUS_ID")

####뉴스사이트 카테고리별 페이지뷰 비율
cs.V7 <- cs %>% subset(BACT_NM=="뉴스/미디어") %>%
  group_by(CUS_ID, MACT_NM) %>%
  summarize(cs = n()) %>%
  cast(CUS_ID ~ MACT_NM, sum) %>%
  mutate(ttl_pv_news = rowSums(.[2:10]))

cs.V7[,2:10] <- cs.V7[,2:10]/cs.V7[,11]
cs.V7 <- cs.V7[,-11]
names(cs.V7)[-1] <- paste("pv_news", names(cs.V7)[-1], sep="_")

####NA점검
anyNA(cs.V1)
anyNA(cs.V2)
anyNA(cs.V3)
anyNA(cs.V4)
anyNA(cs.V5)
anyNA(cs.V6)
anyNA(cs.V7)

#######최종merge
custsig.train <- profile %>%
  left_join (cs.V1) %>%
  left_join (cs.V2) %>%
  left_join (cs.V3) %>%
  left_join (cs.V4) %>%
  left_join (cs.V5) %>%
  left_join (cs.V6) %>%
  left_join (cs.V7) 

custsig.train[is.na(custsig.train)] <- 0

####변수명 조정
names(custsig.train) <- gsub(" ", "", names(custsig.train))
names(custsig.train) <- gsub("/", "", names(custsig.train))

names(custsig.train)[4:25]<-paste("pv",1:22,sep="")
names(custsig.train)[26:232]<-paste("mact",1:207,sep="")
names(custsig.train)[281:289]<-paste("pv_news",1:9,sep="")
write.csv(custsig.train, file="train_master_v.csv",row.names = F)

#########Module 2 : Create Testing Set#########
####총 페이지뷰 횟수
cs_t.V1 <- cs_t %>% 
  group_by(CUS_ID) %>%
  summarize(ttl_pv = sum(SITE_CNT))

####웹사이트 카테고리별 페이지뷰 비율
cs_t.V2.1 <- cs_t %>%
  group_by(CUS_ID, BACT_NM) %>%
  summarize(cs_t = n()) %>%
  cast(CUS_ID ~ BACT_NM, sum) %>%
  mutate(ttl_pv = cs_t.V1$ttl_pv)

cs_t.V2.1[,2:23] <- cs_t.V2.1[,2:23]/cs_t.V2.1[,24]
cs_t.V2.1 <- cs_t.V2.1[,-24]
names(cs_t.V2.1)[-1] <- paste("pv", names(cs_t.V2.1)[-1], sep="_")

cs_t.V2.2 <- cs_t %>%
  group_by(CUS_ID, MACT_NM) %>%
  summarize(cs_t = n()) %>%
  cast(CUS_ID ~ MACT_NM, sum) %>%
  mutate(ttl_pv = cs_t.V1$ttl_pv)

cs_t.V2.2[,2:208] <- cs_t.V2.2[,2:208]/cs_t.V2.2[,209]
cs_t.V2.2 <- cs_t.V2.2[,-209]
names(cs_t.V2.2)[-1] <- paste("mact", names(cs_t.V2.2)[-1], sep="_")

cs_t.V2 <- inner_join(cs_t.V2.1, cs_t.V2.2, by=("CUS_ID"))

####카테고리에 대한 변동계수
cs_t.V3 <- cs_t %>%
  group_by(CUS_ID, BACT_NM) %>%
  summarize(cs_t = n()) %>%
  cast(CUS_ID ~ BACT_NM, sum) %>%
  mutate(avg_pv = rowMeans(.[2:23]), sd_pv = rowSds(as.matrix(.[2:23]))) %>%
  mutate(coef_var_cat = sd_pv/avg_pv) %>%
  select(CUS_ID, coef_var_cat)

####날짜계산용 전처리
cs_t$TIME_ID <- ymd_h(cs_t$TIME_ID)
cs_t$date <- date(cs_t$TIME_ID)
cs_t$year <- year(cs_t$TIME_ID)
cs_t$month <- month(cs_t$TIME_ID)
cs_t$day <- day(cs_t$TIME_ID)
cs_t$time <- hour(cs_t$TIME_ID)
cs_t$wkday <- wday(cs_t$TIME_ID)

####총 방문 일수
cs_t.V4 <- cs_t %>%
  group_by(CUS_ID, date) %>%
  summarize(cs_t = n()) %>%
  group_by(CUS_ID) %>%
  summarize(ttl_vis_day = n())

####월별, 요일별, 시간대별 페이지뷰 비율 
#월별
cs_t.V5.mth <- cs_t %>%
  group_by(CUS_ID, month) %>%
  summarize(pv_mth = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ month, sum) %>%
  mutate(ttl_pv = cs_t.V1$ttl_pv)

cs_t.V5.mth[,2:13] <- cs_t.V5.mth[,2:13]/cs_t.V5.mth[,14]
cs_t.V5.mth <- cs_t.V5.mth[,-14]
names(cs_t.V5.mth)[-1] <- paste("mth", names(cs_t.V5.mth)[-1], sep="_")

#요일별
cs_t.V5.day <- cs_t %>%
  group_by(CUS_ID, wkday) %>%
  summarize(pv_day = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ wkday, sum) %>%
  mutate(ttl_pv = cs_t.V1$ttl_pv)

cs_t.V5.day[,2:8] <- cs_t.V5.day[,2:8]/cs_t.V5.day[,9]
cs_t.V5.day <- cs_t.V5.day[,-9]
names(cs_t.V5.day) <- c("CUS_ID", "sun", "mon", "tue", "wed", "thu", "fri", "sat")

#시간대별
cs_t.V5.time <- cs_t %>%
  group_by(CUS_ID, time) %>%
  summarize(pv_time = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ time, sum) %>%
  mutate(ttl_pv = cs_t.V1$ttl_pv)

cs_t.V5.time[,2:25] <- cs_t.V5.time[,2:25]/cs_t.V5.time[,26]
cs_t.V5.time <- cs_t.V5.time[,-26]
names(cs_t.V5.time)[-1] <- paste("time", names(cs_t.V5.time)[-1], sep="_")

#merge
cs_t.V5 <- inner_join(cs_t.V5.mth, cs_t.V5.day, by=("CUS_ID")) %>%
  left_join(cs_t.V5.time, by="CUS_ID")

####월, 요일, 시간대에 대한 변동계수
#월별
cs_t.V6.mth <- cs_t %>%
  group_by(CUS_ID, month) %>%
  summarize(pv_mth = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ month, sum) %>%
  mutate(avg_mth = rowMeans(.[2:13]), sd_mth = rowSds(as.matrix(.[2:13]))) %>%
  mutate(coef_var_mth = sd_mth/avg_mth) %>%
  select(CUS_ID, coef_var_mth)

#요일별
cs_t.V6.day <- cs_t %>%
  group_by(CUS_ID, wkday) %>%
  summarize(pv_day = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ wkday, sum) %>%
  mutate(avg_day = rowMeans(.[2:8]), sd_day = rowSds(as.matrix(.[2:8]))) %>%
  mutate(coef_var_day = sd_day/avg_day) %>%
  select(CUS_ID, coef_var_day)

#시간대별
cs_t.V6.time <- cs_t %>%
  group_by(CUS_ID, time) %>%
  summarize(pv_time = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ time, sum) %>%
  mutate(avg_time = rowMeans(.[2:25]), sd_time = rowSds(as.matrix(.[2:25]))) %>%
  mutate(coef_var_time = sd_time/avg_time) %>%
  select(CUS_ID, coef_var_time)

#merge
cs_t.V6 <- inner_join(cs_t.V6.mth, cs_t.V6.day, by=("CUS_ID")) %>%
  left_join(cs_t.V6.time, by="CUS_ID")


####뉴스사이트 카테고리별 페이지뷰 비율
cs_t.V7 <- cs_t %>% subset(BACT_NM=="뉴스/미디어") %>%
  group_by(CUS_ID, MACT_NM) %>%
  summarize(cs_t = n()) %>%
  cast(CUS_ID ~ MACT_NM, sum) %>%
  mutate(ttl_pv_news = rowSums(.[2:10]))

cs_t.V7[,2:10] <- cs_t.V7[,2:10]/cs_t.V7[,11]
cs_t.V7 <- cs_t.V7[,-11]
names(cs_t.V7)[-1] <- paste("pv_news", names(cs_t.V7)[-1], sep="_")


####NA점검
anyNA(cs_t.V1)
anyNA(cs_t.V2)
anyNA(cs_t.V3)
anyNA(cs_t.V4)
anyNA(cs_t.V5)
anyNA(cs_t.V6)
anyNA(cs_t.V7)

custsig.test <- cs_t.V1 %>%
  left_join (cs_t.V2) %>%
  left_join (cs_t.V3) %>%
  left_join (cs_t.V4) %>%
  left_join (cs_t.V5) %>%
  left_join (cs_t.V6) %>%
  left_join (cs_t.V7) 

custsig.test[is.na(custsig.test)] <- 0

####변수명 조정
names(custsig.test) <- gsub(" ", "", names(custsig.test))
names(custsig.test) <- gsub("/", "", names(custsig.test))

names(custsig.test)[3:24]<-paste("pv",1:22,sep="")
names(custsig.test)[25:231]<-paste("mact",1:207,sep="")
names(custsig.test)[280:288]<-paste("pv_news",1:9,sep="")
write.csv(custsig.test, file="test_master_v.csv",row.names = F)
#####테스트 합치기####
trf<-read.csv("train_master_v.csv")
tef<-read.csv("test_master_v.csv")
test_pulic<-fread("test_public.csv")
test_pulic$GROUP<-ifelse(test_pulic$`F20-`==1,"F20-",
                         ifelse(test_pulic$F30==1,"F30",
                                ifelse(test_pulic$`F40+`==1,"F40+",
                                       ifelse(test_pulic$`M20-`,"M20-",
                                              ifelse(test_pulic$M30,"M30","M40+")))))

tef<-tef[tef$CUS_ID %in% test_pulic$CUS_ID,]
tef<-merge(test_pulic[,c(1,8)],tef,by="CUS_ID")
trf2<-rbind(trf,tef)
write.csv(trf2,"train_master_all.csv",row.names = F)

tef<-read.csv("test_master_v.csv")
tef<-tef[!(tef$CUS_ID %in% test_pulic$CUS_ID),]
write.csv(tef,"test_master_all.csv",row.names = F)

####w2v 추가####
trf1<-read.csv("train_master_all.csv")
tef1<-read.csv("test_master_all.csv")
trf2<-read.csv("train_final_sel.csv")
tef2<-read.csv("test_final_sel.csv")
trf2<-trf2[,c(1,220:237)]
tef2<-tef2[,c(1,219:236)]
trf<-merge(trf1,trf2,by="CUS_ID")
names(trf)
tef<-merge(tef1,tef2,by="CUS_ID")
names(tef)
write.csv(trf,"train_master+w2v_all.csv",row.names = F)
write.csv(tef,"test_master+w2v_all.csv",row.names = F)
####
rm(list=ls())
gc()
trf<-read.csv("train_master+w2v_all.csv")
trf$GROUP<-factor(make.names(trf$GROUP,unique = F,allow_ = T))
tef<-read.csv("test_master+w2v_all.csv")

xgb_grid_1 = expand.grid(
  nrounds = c(100,300,400,600),
  eta = 0.01,
  max_depth = c(4),
  gamma = c(0.1),
  subsample = 0.8,
  colsample_bytree = c(0.8) ,
  min_child_weight=c(1)
)

xgb_trcontrol_1 = trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "final",
  classProbs = TRUE,
  savePredictions = 'final',
  summaryFunction = mnLogLoss ,
  allowParallel = TRUE
)


xgbx1 = train(GROUP~.-CUS_ID,trf, trControl = xgb_trcontrol_1,  
              tuneGrid = xgb_grid_1,  method = "xgbTree",  
              metric = "logLoss",maximize=FALSE)


pred<-data.frame(CUS_ID=tef$CUS_ID,predict(xgbx1,tef,type='prob'))
names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred, "ex-master-xgb-1.21.csv", row.names = F)

############################################################################
##8조 a,b,c,d

setwd("D:/r/데마")
library(data.table)
library(xlsx)
library(reshape)
library(caret)
library(xgboost)
library(plyr)

## a
pf <- read.xlsx("train_profiles.xlsx",header = T,1)
ck.train <- fread("train_clickstreams.tab")
ck.test <- fread("test_clickstreams.tab")
pft <- data.frame(CUS_ID=unique(ck.test$CUS_ID))
pft$GROUP <- NA
all <- rbind(ck.train,ck.test)
pf$CUS_ID <- as.character(pf$CUS_ID)
pft$CUS_ID <- as.character(pft$CUS_ID)
pf_all <- rbind(pf,pft)

#ACT_NM, BACT_NM, MACT_NM의 총 ST_TIME 횟수  
ACTNM3 <- cast(all, CUS_ID ~ ACT_NM, sum, value = "ST_TIME")
ACTNM4=as.data.frame.cast_matrix(prop.table(as.table(as.matrix(ACTNM3)),1))

BACTNM3 <- cast(all, CUS_ID ~ BACT_NM, sum, value = "ST_TIME")
BACTNM4=as.data.frame.cast_matrix(prop.table(as.table(as.matrix(BACTNM3)),1))

MACTNM3 <- cast(all, CUS_ID ~ MACT_NM, sum, value = "ST_TIME")
MACTNM4=as.data.frame.cast_matrix(prop.table(as.table(as.matrix(MACTNM3)),1))

#시간대별 ST_TIEM
all$HOUR <- substr(all$TIME_ID,9,10)
all$TIME=as.numeric(all$HOUR)
TIMETIME <- cast(all, CUS_ID ~ TIME, sum, value = "ST_TIME")
TIMETIME2=as.data.frame.cast_matrix(prop.table(as.table(as.matrix(TIMETIME)),1))
#평일, 주말
all$DATE <- substr(all$TIME_ID,1,8)
all$DATE <- as.Date(as.character(all$DATE),"%Y%m%d")

day_levels <- c("월요일", "화요일", "수요일", "목요일", "금요일", "토요일","일요일")
all$WDAY = as.numeric(factor(weekdays((all$DATE)), levels=day_levels, ordered=TRUE))

wd <- aggregate(all$ST_TIME,by=list(CUS_ID=all$CUS_ID,WD=all$WDAY),sum)
wd1 <- cast(data = wd,CUS_ID~WD,value="x",sum)
wd2=as.data.frame.cast_matrix(prop.table(as.table(as.matrix(wd1)),1))

colnames(wd2) <- c("CUS_ID","MON","TUE","WED","TUR","FRI","SAT","SUN")

wd2$weekday<-wd2$MON+wd2$TUE+wd2$WED+wd2$TUR+wd2$FRI
wd2$weekend<-wd2$SAT+wd2$SUN

wd3<-wd2[,-8:-2]



allD=merge(pf_all,BACTNM4,by="CUS_ID")
allD <- merge(allD,MACTNM4,by="CUS_ID")
allD <- merge(allD,ACTNM4,by="CUS_ID")
allD <- merge(allD,TIMETIME2,by="CUS_ID")
allD <- merge(allD,wd3,by="CUS_ID")

allD$CUS_ID <- as.numeric(allD$CUS_ID)
allD1 <- arrange(allD,CUS_ID)

write.csv(allD1,"allD1_st_prop.csv",row.names=F,quote=F)
rm(list=ls())
gc()

gun1 <- read.csv("allD1_st_prop.csv")
str(gun1)
train5 <- gun1[1:2500,]
test3 <- gun1[2501:5000,]
train5$GROUP<-substr(train5$GROUP,1,3)
test3<-test3[,-2]
train5$GROUP<-factor(make.names(train5$GROUP,unique = F,allow_ = T))
train5[is.na(train5)] <-0
test3[is.na(test3)]<-0
control <- trainControl(method="repeatedcv", number=5, repeats=3, classProbs=TRUE, summaryFunction=mnLogLoss)
set.seed(123)

xgb <- caret::train(GROUP ~ .,
                    data = subset(train5, select=-CUS_ID),
                    method = "xgbTree",
                    preProcess = NULL,
                    metric = "logLoss",
                    trControl = control)


predicted <- predict(xgb, subset(test3, select=-CUS_ID), type="prob")
pred <- cbind(test3$CUS_ID,predicted)
colnames(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred,"a.csv",row.names=F,quote=F)

rm(list=ls())
gc()
##b
pf <- read.xlsx("train_profiles.xlsx",header = T,1)
ck.train <- fread("train_clickstreams.tab")
ck.test <- fread("test_clickstreams.tab")
pft <- data.frame(CUS_ID=unique(ck.test$CUS_ID))
pft$GROUP <- NA
all <- rbind(ck.train,ck.test)
pf$CUS_ID <- as.character(pf$CUS_ID)
pft$CUS_ID <- as.character(pft$CUS_ID)
pf_all <- rbind(pf,pft)

all$count <- 1

#ACT_NM, BACT_NM, MACT_NM의 총 count 횟수  

#ACTNM3 <- cast(all, CUS_ID ~ ACT_NM, sum, value = "count")
#save(ACTNM3,file = "gun_so")
#BACTNM3 <- cast(all, CUS_ID ~ BACT_NM, sum, value = "count")
#save(BACTNM3,file = "gun_so2")
#MACTNM3 <- cast(all, CUS_ID ~ MACT_NM, sum, value = "count")
#save(MACTNM3,file = "gun_so3")

#시간대별 ST_TIEM
all$HOUR <- substr(all$TIME_ID,9,10)
all$TIME=as.numeric(all$HOUR)
TIMETIME <- cast(all, CUS_ID ~ TIME, sum, value = "count")
TIMETIME2=as.data.frame.cast_matrix(prop.table(as.table(as.matrix(TIMETIME)),1))

#평일, 주말
all$DATE <- substr(all$TIME_ID,1,8)
all$DATE <- as.Date(as.character(all$DATE),"%Y%m%d")

day_levels <- c("월요일", "화요일", "수요일", "목요일", "금요일", "토요일","일요일")
all$WDAY = as.numeric(factor(weekdays((all$DATE)), levels=day_levels, ordered=TRUE))

wd <- aggregate(all$count,by=list(CUS_ID=all$CUS_ID,WD=all$WDAY),sum)
wd1 <- cast(data = wd,CUS_ID~WD,value="x",sum)

colnames(wd1) <- c("CUS_ID","MON","TUE","WED","TUR","FRI","SAT","SUN")

wd1$weekday<-wd1$MON+wd1$TUE+wd1$WED+wd1$TUR+wd1$FRI
wd1$weekend<-wd1$SAT+wd1$SUN

wd3<-wd1[,-8:-2]

load("gun_so")
load("gun_so2")
load("gun_so3")

allD=merge(pf_all,BACTNM3,by="CUS_ID")
allD <- merge(allD,MACTNM3,by="CUS_ID")
allD <- merge(allD,ACTNM3,by="CUS_ID")
allD <- merge(allD,TIMETIME,by="CUS_ID")
allD <- merge(allD,wd3,by="CUS_ID")

allD$CUS_ID <- as.numeric(allD$CUS_ID)
allD1 <- arrange(allD,CUS_ID)
write.csv(allD1,"allD2_st_prop.csv",row.names=F,quote=F)
rm(list=ls())
gc()

gun2 <- read.csv("allD2_st_prop.csv")

train5 <- gun2[1:2500,]
test3 <- gun2[2501:5000,]
train5$GROUP<-substr(train5$GROUP,1,3)
test3<-test3[,-2]
train5$GROUP<-factor(make.names(train5$GROUP,unique = F,allow_ = T))
train5[is.na(train5)] <-0
test3[is.na(test3)]<-0
control <- trainControl(method="repeatedcv", number=5, repeats=3, classProbs=TRUE, summaryFunction=mnLogLoss)
set.seed(123)

xgb <- caret::train(GROUP ~ .,
                    data = subset(train5, select=-CUS_ID),
                    method = "xgbTree",
                    preProcess = NULL,
                    metric = "logLoss",
                    trControl = control)


predicted <- predict(xgb, subset(test3, select=-CUS_ID), type="prob")
pred <- cbind(test3$CUS_ID,predicted)
colnames(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred,"b.csv",row.names=F,quote=F)
rm(list=ls())
gc()

##c

pf <- read.xlsx("train_profiles.xlsx",header = T,1)
ck.train <- fread("train_clickstreams.tab")
ck.test <- fread("test_clickstreams.tab")
pft <- data.frame(CUS_ID=unique(ck.test$CUS_ID))
pft$GROUP <- NA
all <- rbind(ck.train,ck.test)
pf$CUS_ID <- as.character(pf$CUS_ID)
pft$CUS_ID <- as.character(pft$CUS_ID)
pf_all <- rbind(pf,pft)

#ACT_NM, BACT_NM, MACT_NM의 총 SITE_CNT 횟수  
ACTNM3 <- cast(all, CUS_ID ~ ACT_NM, sum, value = "SITE_CNT")
ACTNM4=as.data.frame.cast_matrix(prop.table(as.table(as.matrix(ACTNM3)),1))

BACTNM3 <- cast(all, CUS_ID ~ BACT_NM, sum, value = "SITE_CNT")
BACTNM4=as.data.frame.cast_matrix(prop.table(as.table(as.matrix(BACTNM3)),1))

MACTNM3 <- cast(all, CUS_ID ~ MACT_NM, sum, value = "SITE_CNT")
MACTNM4=as.data.frame.cast_matrix(prop.table(as.table(as.matrix(MACTNM3)),1))

save(ACTNM4,file = "gun_31")
save(BACTNM4,file = "gun_32")
save(MACTNM4,file = "gun_33")
rm(list=ls())
gc()
#--
pf <- read.xlsx("train_profiles.xlsx",header = T,1)
ck.train <- fread("train_clickstreams.tab")
ck.test <- fread("test_clickstreams.tab")
pft <- data.frame(CUS_ID=unique(ck.test$CUS_ID))
pft$GROUP <- NA
all <- rbind(ck.train,ck.test)
pf$CUS_ID <- as.character(pf$CUS_ID)
pft$CUS_ID <- as.character(pft$CUS_ID)
pf_all <- rbind(pf,pft)
#시간대별 ST_TIEM
all$HOUR <- substr(all$TIME_ID,9,10)
all$TIME=as.numeric(all$HOUR)
TIMETIME <- cast(all, CUS_ID ~ TIME, sum, value = "SITE_CNT")
TIMETIME2=as.data.frame.cast_matrix(prop.table(as.table(as.matrix(TIMETIME)),1))

#평일, 주말
all$DATE <- substr(all$TIME_ID,1,8)
all$DATE <- as.Date(as.character(all$DATE),"%Y%m%d")

day_levels <- c("월요일", "화요일", "수요일", "목요일", "금요일", "토요일","일요일")
all$WDAY = as.numeric(factor(weekdays((all$DATE)), levels=day_levels, ordered=TRUE))

wd <- aggregate(all$SITE_CNT,by=list(CUS_ID=all$CUS_ID,WD=all$WDAY),sum)
wd1 <- cast(data = wd,CUS_ID~WD,value="x",sum)
wd2=as.data.frame.cast_matrix(prop.table(as.table(as.matrix(wd1)),1))

colnames(wd2) <- c("CUS_ID","MON","TUE","WED","TUR","FRI","SAT","SUN")

wd2$weekday<-wd2$MON+wd2$TUE+wd2$WED+wd2$TUR+wd2$FRI
wd2$weekend<-wd2$SAT+wd2$SUN

wd3<-wd2[,-8:-2]

load("gun_31")
load("gun_32")
load("gun_33")

allD=merge(pf_all,BACTNM4,by="CUS_ID")
allD <- merge(allD,MACTNM4,by="CUS_ID")
allD <- merge(allD,ACTNM4,by="CUS_ID")
allD <- merge(allD,TIMETIME2,by="CUS_ID")
allD <- merge(allD,wd3,by="CUS_ID")

allD$CUS_ID <- as.numeric(allD$CUS_ID)
allD1 <- arrange(allD,CUS_ID)
allD$CUS_ID <- as.numeric(allD$CUS_ID)
allD1 <- arrange(allD,CUS_ID)
write.csv(allD1,"allD3_st_prop.csv",row.names=F,quote=F)
rm(list=ls())
gc()

gun3<-read.csv("allD3_st_prop.csv")

train5 <- gun3[1:2500,]
test3 <- gun3[2501:5000,]
train5$GROUP<-substr(train5$GROUP,1,3)
test3<-test3[,-2]
train5$GROUP<-factor(make.names(train5$GROUP,unique = F,allow_ = T))
train5[is.na(train5)] <-0
test3[is.na(test3)]<-0

control <- trainControl(method="repeatedcv", number=5, repeats=3, classProbs=TRUE, summaryFunction=mnLogLoss)
set.seed(123)

xgb <- caret::train(GROUP ~ .,
                    data = subset(train5, select=-CUS_ID),
                    method = "xgbTree",
                    preProcess = NULL,
                    metric = "logLoss",
                    trControl = control)


predicted <- predict(xgb, subset(test3, select=-CUS_ID), type="prob")
pred <- cbind(test3$CUS_ID,predicted)
colnames(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred,"c.csv",row.names=F,quote=F)
rm(list=ls())
gc()
##d
pf <- read.xlsx("train_profiles.xlsx",header = T,1)
ck.train <- fread("train_clickstreams.tab")
ck.test <- fread("test_clickstreams.tab")
pft <- data.frame(CUS_ID=unique(ck.test$CUS_ID))
pft$GROUP <- NA
all <- rbind(ck.train,ck.test)
pf$CUS_ID <- as.character(pf$CUS_ID)
pft$CUS_ID <- as.character(pft$CUS_ID)
pf_all <- rbind(pf,pft)

#ACT_NM, BACT_NM, MACT_NM 
ACTNM3 <- cast(all, CUS_ID ~ ACT_NM, sum, value = "SITE_CNT")

BACTNM3 <- cast(all, CUS_ID ~ BACT_NM, sum, value = "SITE_CNT")

MACTNM3 <- cast(all, CUS_ID ~ MACT_NM, sum, value = "SITE_CNT")
save(ACTNM3,file = "gun_41")
save(BACTNM3,file = "gun_42")
save(MACTNM3,file = "gun_43")
rm(list=ls())
gc()

pf <- read.xlsx("train_profiles.xlsx",header = T,1)
ck.train <- fread("train_clickstreams.tab")
ck.test <- fread("test_clickstreams.tab")
pft <- data.frame(CUS_ID=unique(ck.test$CUS_ID))
pft$GROUP <- NA
all <- rbind(ck.train,ck.test)
pf$CUS_ID <- as.character(pf$CUS_ID)
pft$CUS_ID <- as.character(pft$CUS_ID)
pf_all <- rbind(pf,pft)
#시간대별 
all$HOUR <- substr(all$TIME_ID,9,10)
all$TIME=as.numeric(all$HOUR)
TIMETIME <- cast(all, CUS_ID ~ TIME, sum, value = "SITE_CNT")

#평일, 주말
all$DATE <- substr(all$TIME_ID,1,8)
all$DATE <- as.Date(as.character(all$DATE),"%Y%m%d")

day_levels <- c("월요일", "화요일", "수요일", "목요일", "금요일", "토요일","일요일")
all$WDAY = as.numeric(factor(weekdays((all$DATE)), levels=day_levels, ordered=TRUE))

wd <- aggregate(all$SITE_CNT,by=list(CUS_ID=all$CUS_ID,WD=all$WDAY),sum)
wd1 <- cast(data = wd,CUS_ID~WD,value="x",sum)
wd2=as.data.frame.cast_matrix(prop.table(as.table(as.matrix(wd1)),1))

colnames(wd2) <- c("CUS_ID","MON","TUE","WED","TUR","FRI","SAT","SUN")

wd2$weekday<-wd2$MON+wd2$TUE+wd2$WED+wd2$TUR+wd2$FRI
wd2$weekend<-wd2$SAT+wd2$SUN

wd3<-wd2[,-8:-2]

load("gun_41")
load("gun_42")
load("gun_43")

allD=merge(pf_all,BACTNM3,by="CUS_ID")
allD <- merge(allD,MACTNM3,by="CUS_ID")
allD <- merge(allD,ACTNM3,by="CUS_ID")
allD <- merge(allD,TIMETIME,by="CUS_ID")
allD <- merge(allD,wd3,by="CUS_ID")

allD$CUS_ID <- as.numeric(allD$CUS_ID)
allD1 <- arrange(allD,CUS_ID)
write.csv(allD1,"allD4_st_prop.csv",row.names=F,quote=F)
rm(list=ls())
gc()

gun4 <- read.csv("allD4_st_prop.csv")

train5 <- gun4[1:2500,]
test3 <- gun4[2501:5000,]
train5$GROUP<-substr(train5$GROUP,1,3)
test3<-test3[,-2]

train5<-na.omit(train5)
test3[is.na(test3)] <- 0
test3<-na.omit(test3)
train5$GROUP<-factor(make.names(train5$GROUP,unique = F,allow_ = T))
train5[is.na(train5)] <-0
test3[is.na(test3)]<-0

control <- trainControl(method="repeatedcv", number=5, repeats=3, classProbs=TRUE, summaryFunction=mnLogLoss)
set.seed(123)

xgb <- caret::train(GROUP ~ .,
                    data = subset(train5, select=-CUS_ID),
                    method = "xgbTree",
                    preProcess = NULL,
                    metric = "logLoss",
                    trControl = control)

predicted <- predict(xgb, subset(test3, select=-CUS_ID), type="prob")
pred <- cbind(test3$CUS_ID,predicted)
colnames(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred,"d.csv",row.names=F,quote=F)


############################################################################
###w2v-QRY300
#train
setwd("D:/r/데마")
if(!require(devtools)) install.packages("devtools"); library(devtools)
if(!require(wordVectors)) install_github("bmschmidt/wordVectors"); library(wordVectors)

# Install & load data.table package
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(randomForest)) install.packages("randomForest"); library(randomForest)
if(!require(caret)) install.packages("caret"); library(caret)
library(xlsx)
library(data.table)
cs.dt <- fread("train_profiles.csv")
tr.dt <- fread("train_searchkeywords.tab"); tr.dt[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt, CUS_ID); setkey(tr.dt, CUS_ID) 
md.dt <- merge(cs.dt, tr.dt)

test_pulic<-fread("test_public.csv")
test_pulic$GROUP<-ifelse(test_pulic$`F20-`==1,"F20-",
                         ifelse(test_pulic$F30==1,"F30",
                                ifelse(test_pulic$`F40+`==1,"F40+",
                                       ifelse(test_pulic$`M20-`,"M20-",
                                              ifelse(test_pulic$M30,"M30","M40+")))))
tr.dt2<-fread("test_searchkeywords.tab"); tr.dt2[,CUS_ID:=as.numeric(CUS_ID)]
setkey(test_pulic,CUS_ID);setkey(tr.dt2,CUS_ID)
md.dt2<-merge(test_pulic[,c(1,8)],tr.dt2)
md.dt<-rbind(md.dt,md.dt2)
cus<-unique(md.dt$CUS_ID) 
so<-md.dt

so$QRY_STR<-ifelse(regexpr("query",so$QRY_STR)>0 ,
                   substr(so$QRY_STR,regexpr("query",so$QRY_STR)+6,500),
                   so$QRY_STR)
so$QRY_STR<-ifelse(regexpr("acq",so$QRY_STR)>0 ,
                   substr(so$QRY_STR,1,regexpr("&",so$QRY_STR)-1),
                   so$QRY_STR)

#
model <- read.binary.vectors("vec-ts-all.bin")
df<-data.frame()
for (i in cus ){
  itemfreq <- table(md.dt[CUS_ID==i, QRY_STR])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,model[[names(fitems), average=T]])
  df<-rbind(df,sim)
}
cus_gender<-rbind(cs.dt,test_pulic[,c(1,8)])
w2v_act<-df
w2v_QRY_train<-merge(cus_gender,by="CUS_ID",w2v_act)

write.csv(QRY_train,"w2v_QRY_train.csv",row.names = F)


######test
tr.dt2<-tr.dt2[!(tr.dt2$CUS_ID %in% test_pulic$CUS_ID),]

sso<-tr.dt2

sso$QRY_STR<-ifelse(regexpr("query",sso$QRY_STR)>0 ,
                    substr(sso$QRY_STR,regexpr("query",sso$QRY_STR)+6,500),
                    sso$QRY_STR)
sso$QRY_STR<-ifelse(regexpr("acq",sso$QRY_STR)>0 ,
                    substr(sso$QRY_STR,1,regexpr("&",sso$QRY_STR)-1),
                    sso$QRY_STR)
cus2<-unique(sso$CUS_ID)

df<-data.frame()
for (i in cus2 ){
  itemfreq <- table(tr.dt2[CUS_ID==i, QRY_STR])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,model[[names(fitems), average=T]])
  df<-rbind(df,sim)
}

w2v_QRY_test<-df

write.csv(w2v_QRY_test,"w2v_QRY_test.csv",row.names = F)

######################모델링
trf<-read.csv("w2v_QRY_train.csv")
tef<-read.csv("w2v_QRY_test.csv")
tef2<-read.csv("w2v_site_test.csv")
train<-trf

test<-merge(tef2[1],tef,by="CUS_ID",all=T)
a<-mice(test[,-1])
test<-cbind(test[,1],complete(a,2))
test$CUS_ID<-test$`test[, 1]`
test<-test[,-1]

train$GROUP<-make.names(train$GROUP)
control <- trainControl(method="cv", number=5, repeats=1, classProbs=TRUE, summaryFunction=mnLogLoss)
str(train)
str(test)
# Used models
methods <- c("xgbTree","rf","wsrf") # add methods such as xgbTree, rf, svmRadious, etc.
library(gbm)
library(nnet)
library(glmnet)

models <- list()
for (i in 1:length(methods)) {
  set.seed(123)
  model <- train(GROUP ~ .,
                 data = subset(train, select=-CUS_ID),
                 method = methods[i],
                 preProcess = NULL,
                 metric = "logLoss",
                 trControl = control)
  models[[i]] <- model
}
names(models) <- methods
# Saving and loading models: 
#saveRDS(models, "models.rds") 
# models <- readRDS("models.rds")


# prediction & submission
for (i in 1:length(methods)) {
  pred <- predict(models[i], test, type="prob")
  fname <- paste("submission_qry_", methods[i], ".csv", sep="")
  write.csv(cbind(CUS_ID=test$CUS_ID,pred[[1]]), fname, row.names = F)
}

####################################################################
#SITE
setwd("D:/r/데마")
if(!require(devtools)) install.packages("devtools"); library(devtools)
if(!require(wordVectors)) install_github("bmschmidt/wordVectors"); library(wordVectors)

# Install & load data.table package
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(randomForest)) install.packages("randomForest"); library(randomForest)
if(!require(caret)) install.packages("caret"); library(caret)
#
cs.dt <- fread("train_profiles.csv")
tr.dt <- fread("train_clickstreams.tab"); tr.dt[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt, CUS_ID); setkey(tr.dt, CUS_ID) 
md.dt <- merge(cs.dt, tr.dt)

test_pulic<-fread("test_public.csv")
test_pulic$GROUP<-ifelse(test_pulic$`F20-`==1,"F20-",
                         ifelse(test_pulic$F30==1,"F30",
                                ifelse(test_pulic$`F40+`==1,"F40+",
                                       ifelse(test_pulic$`M20-`,"M20-",
                                              ifelse(test_pulic$M30,"M30","M40+")))))
tr.dt2<-fread("test_clickstreams.tab"); tr.dt2[,CUS_ID:=as.numeric(CUS_ID)]
setkey(test_pulic,CUS_ID);setkey(tr.dt2,CUS_ID)
md.dt2<-merge(test_pulic[,c(1,8)],tr.dt2)
md.dt<-rbind(md.dt,md.dt2)

cus<-unique(md.dt$CUS_ID)

####벡터생성

model <- read.binary.vectors("vec_site.bin") # reload the model.7조 변수 생성시 사용했던 모델 그대로 사용. 
df<-data.frame()
for (i in cus ){
  itemfreq <- table(md.dt[CUS_ID==i, SITE])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,model[[names(fitems), average=T]])
  df<-rbind(df,sim)
}
cus_gender<-rbind(cs.dt,test_pulic[,c(1,8)])
w2v_act<-df
w2v_act_train<-merge(cus_gender,by="CUS_ID",w2v_act)

write.csv(w2v_act_train,"w2v_site_train.csv",row.names = F)

#####테스트적용#####
tr.dt2<-tr.dt2[!(tr.dt2$CUS_ID %in% cus),]
cus2<-unique(tr.dt2$CUS_ID)
df<-data.frame()
for (i in cus2 ){
  itemfreq <- table(tr.dt2[CUS_ID==i, SITE])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,model[[names(fitems), average=T]])
  df<-rbind(df,sim)
}

w2v_act_test<-df

write.csv(w2v_act_test,"w2v_site_test.csv",row.names = F)
pred <- predict(models[i], test_SITE400, type="prob")
fname <- paste("w2v_SITE_", methods[i], ".csv", sep="")
write.csv(cbind(CUS_ID=test$CUS_ID,pred[[1]]), fname, row.names = F)
}

###모델링
trf2<-read.csv("w2v_site_train.csv")
tef2<-read.csv("w2v_site_test.csv")
train<-trf2
test<-tef2
train$GROUP<-make.names(train$GROUP)
control <- trainControl(method="cv", number=5, repeats=1, classProbs=TRUE, summaryFunction=mnLogLoss)
str(train)
str(test)
# Used models
methods <- c("gbm") # add methods such as xgbTree, rf, svmRadious, etc.
library(gbm)
library(nnet)
library(glmnet)

models <- list()
for (i in 1:length(methods)) {
  set.seed(123)
  model <- train(GROUP ~ .,
                 data = subset(train, select=-CUS_ID),
                 method = methods[i],
                 preProcess = NULL,
                 metric = "logLoss",
                 trControl = control)
  models[[i]] <- model
}
names(models) <- methods
# Saving and loading models: 
#saveRDS(models, "models.rds") 
# models <- readRDS("models.rds")


# prediction & submission
for (i in 1:length(methods)) {
  pred <- predict(models[i], test, type="prob")
  fname <- paste("submission_site_", methods[i], ".csv", sep="")
  write.csv(cbind(CUS_ID=test$CUS_ID,pred[[1]]), fname, row.names = F)
}
##########scale nnet
train1<-scale(train[,-c(1,2)])
train<-cbind(train[,c(1,2)],train1)

methods <- c("nnet") # add methods such as xgbTree, rf, svmRadious, etc.
library(gbm)
library(nnet)
library(glmnet)

models <- list()
for (i in 1:length(methods)) {
  set.seed(123)
  model <- train(GROUP ~ .,
                 data = subset(train, select=-CUS_ID),
                 method = methods[i],
                 preProcess = NULL,
                 metric = "logLoss",
                 trControl = control)
  models[[i]] <- model
}
names(models) <- methods
# Saving and loading models: 
#saveRDS(models, "models.rds") 
# models <- readRDS("models.rds")


# prediction & submission
for (i in 1:length(methods)) {
  pred <- predict(models[i], test, type="prob")
  fname <- paste("submission_site_scale_", methods[i], ".csv", sep="")
  write.csv(cbind(CUS_ID=test$CUS_ID,pred[[1]]), fname, row.names = F)
}

#################################



#############################################
###########nnet 파일 scale
train<-read.csv("train_final_w2v_all.csv")
test<-read.csv("test_final_w2v_all.csv")

a<-data.frame(scale(train[,-c(1,2)]))
b<-data.frame(scale(test[,-1]))
train<-cbind(train[1:2],a)
test<-cbind(test[1],b)



train$GROUP<-make.names(train$GROUP)
control <- trainControl(method="cv", number=5, repeats=1, classProbs=TRUE, summaryFunction=mnLogLoss)

# Used models
methods <- c("nnet") # add methods such as xgbTree, rf, svmRadious, etc.

models <- list()
for (i in 1:length(methods)) {
  set.seed(123)
  model <- train(GROUP ~ .,
                 data = subset(train, select=-CUS_ID),
                 method = methods[i],
                 preProcess = NULL,
                 metric = "logLoss",
                 trControl = control)
  models[[i]] <- model
}
names(models) <- methods


# prediction & submission
for (i in 1:length(methods)) {
  pred <- predict(models[i], test, type="prob")
  fname <- paste("submission_st+act_scale", methods[i], ".csv", sep="")
  write.csv(cbind(CUS_ID=test$CUS_ID,pred[[1]]), fname, row.names = F)
}





앙상블 데이터 불러오기

###################################################################
if(!require(caret)) install.packages("caret"); library(caret)
if(!require(caretEnsemble)) install.packages("caretEnsemble"); library(caretEnsemble)


setwd("D:/r/데마/최종")
test_pulic <- read.csv("test_public.csv", stringsAsFactors = F)
en1<-read.csv("A.csv")
en1<-en1[!(en1$CUS_ID %in% test_pulic$CUS_ID),]
names(en1)[-1]<-paste(names(en1)[-1],"_en1",sep="")
en2<-read.csv("B.csv")
en2<-en2[!(en2$CUS_ID %in% test_pulic$CUS_ID),]
names(en2)[-1]<-paste(names(en2)[-1],"_en2",sep="")
en3<-read.csv("C.csv")
en3<-en3[!(en3$CUS_ID %in% test_pulic$CUS_ID),]
names(en3)[-1]<-paste(names(en3)[-1],"_en3",sep="")
en4<-read.csv("D.csv")
en4<-en4[!(en4$CUS_ID %in% test_pulic$CUS_ID),]
names(en4)[-1]<-paste(names(en4)[-1],"_en4",sep="")

en5<-read.csv("5-1조.csv")
en5<-en5[!(en5$CUS_ID %in% test_pulic$CUS_ID),]
names(en5)[-1]<-paste(names(en5)[-1],"_en5",sep="")
en6<-read.csv("5-7조.csv")
en6<-en6[!(en6$CUS_ID %in% test_pulic$CUS_ID),]
names(en6)[-1]<-paste(names(en6)[-1],"_en6",sep="")
en7<-read.csv("5-8조.csv")
en7<-en7[!(en7$CUS_ID %in% test_pulic$CUS_ID),]
names(en7)[-1]<-paste(names(en7)[-1],"_en7",sep="")
en8<-read.csv("4-3조.csv")
en8<-en8[!(en8$CUS_ID %in% test_pulic$CUS_ID),]
names(en8)[-1]<-paste(names(en8)[-1],"_en8",sep="")

en9<-read.csv("7조-logloss-175030.csv")
names(en9)[-1]<-paste(names(en9)[-1],"_en9",sep="")
en10<-read.csv("7조-변수추가-xgb.csv")
names(en10)[-1]<-paste(names(en10)[-1],"_en10",sep="")
en11<-read.csv("ex-master-xgb-1.21.csv")
names(en11)[-1]<-paste(names(en11)[-1],"_en11",sep="")
en12<-read.csv("master1_submission_xgb_1.318.csv")
names(en12)[-1]<-paste(names(en12)[-1],"_en12",sep="")

##w2v site+act
en13<-read.csv("submission_st+actgbm.csv")
names(en13)[-1]<-paste(names(en13)[-1],"_en13",sep="")
#en14<-read.csv("submission_st+actnnet.csv")
#names(en14)[-1]<-paste(names(en14)[-1],"_en14",sep="")
en14<-read.csv("submission_st+act_scalennet.csv")
names(en14)[-1]<-paste(names(en14)[-1],"_en14",sep="")

##w2v site
en15<-read.csv("submission_site_scale_nnet.csv")
names(en15)[-1]<-paste(names(en15)[-1],"_en15",sep="")
en16<-read.csv("submission_site_gbm.csv")
names(en16)[-1]<-paste(names(en16)[-1],"_en16",sep="")
en17<-read.csv("submission_site_glmnet.csv")
names(en17)[-1]<-paste(names(en17)[-1],"_en17",sep="")
#4조 추가 
en18<-read.csv("4-9조.csv")
en18<-en18[!(en18$CUS_ID %in% test_pulic$CUS_ID),]
names(en18)[-1]<-paste(names(en18)[-1],"_en8",sep="")
en19<-read.csv("4-7조.csv")
en19<-en19[!(en19$CUS_ID %in% test_pulic$CUS_ID),]
names(en19)[-1]<-paste(names(en19)[-1],"_en8",sep="")
#qry
en20<-read.csv("submission_qry_rf.csv")
names(en20)[-1]<-paste(names(en20)[-1],"_en20",sep="")
en21<-read.csv("submission_qry_xgbTree.csv")
names(en21)[-1]<-paste(names(en21)[-1],"_en21",sep="")

###################################################
#앙상블
#앙상블 1 (a+b+c+d)
pred<-merge(merge(en1,merge(en2,en3,by="CUS_ID"),by="CUS_ID"),en4,by="CUS_ID")
nc <- ncol(pred)
nf <- 4
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred,"ensemble1.csv", row.names = F)

#앙상블2 (4주차 1,2,3)
pred<-merge(en8,merge(en18,en19,by="CUS_ID"),by="CUS_ID")
nc <- ncol(pred)
nf <- 3
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred,"ensemble2.csv", row.names = F)

#앙상블3 (w2v SITE gbm+nnet)
pred<-merge(en15,en16,by="CUS_ID")
nc <- ncol(pred)
nf <- 2
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred,"ensemble3.csv", row.names = F)

#
ensemble1<-read.csv("ensemble1.csv")
ensemble2<-read.csv("ensemble2.csv")
ensemble3<-read.csv("ensemble3.csv")
#앙상블4 (ensemble3+actsite600nnet+gbm)
pred<-merge(ensemble3,merge(en13,en14,by="CUS_ID"),by="CUS_ID")
nc <- ncol(pred)
nf <- 3
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred,"ensemble4.csv", row.names = F)

ensemble4<-read.csv("ensemble4.csv")
#앙상블 8(w2v qry rf+xgb)
pred<-merge(en20,en21,by="CUS_ID")
nc <- ncol(pred)
nf <- 2
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred,"ensemble8.csv", row.names = F)

#앙상블 a(2+4+8)
pred<-merge(ensemble2,merge(ensemble4,ensemble8,by="CUS_ID"),by="CUS_ID")
nc <- ncol(pred)
nf <- 3
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred,"ensemble_a.csv", row.names = F)

ensemble_a<-read.csv("ensemble_a.csv")

#앙상블 b-
pred<-merge(ensemble_a,merge(en9,en10,by="CUS_ID"),by="CUS_ID")
nc <- ncol(pred)
nf <- 3
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred,"ensemble_b.csv", row.names = F)

ensemble_b<-read.csv("ensemble_b.csv")

#최종 submission
pred<-merge(ensemble_b,merge(en11,en12,by="CUS_ID"),by="CUS_ID")
nc <- ncol(pred)
nf <- 3
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred,"5조_final3_re.csv", row.names = F)
