
##### Test 데이터

test_clickstreams<-read.table("test_clickstreams.tab",sep="\t",header = T,stringsAsFactors = F)
tr<-test_clickstreams

##### 파생필드 생성
tr<-na.omit(tr)
tr$wk<-weekdays(as.Date(as.character(tr$TIME_ID),"%Y%m%d%H"))
tr$wk<-factor(tr$wk,levels = c("월요일","화요일","수요일","목요일","금요일","토요일","일요일"))
tr$h<-as.numeric(substr(tr$TIME_ID,9,10))
tr$m<-as.numeric(substr(tr$TIME_ID,5,6))
tr$h2<-cut(tr$h,breaks=4,labels=c("0005","0611","1217","1823"))

cs.v1<- tr %>% group_by(CUS_ID) %>% summarise(PAGEVIEW=sum(SITE_CNT))
cs.v2<- tr %>% distinct(CUS_ID,TIME_ID) %>% group_by(CUS_ID) %>% summarise(VDAYS=n())
cs.v3<- tr %>% distinct(CUS_ID,BACT_NM) %>% group_by(CUS_ID) %>% summarise(COVERAGE=n()/22)


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

##변수추가
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
sum(is.na(test_final)) 

########### 변수추가 ############
a<-tr %>% filter(BACT_NM=="쇼핑") %>% group_by(CUS_ID) %>% summarise(SH=sum(ST_TIME))
test_final<-left_join(test_final,a)
test_final$SH<-ifelse(is.na(test_final$SH),0,test_final$SH)


#######변수추가 ######
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

#Test 소분류(ACT_NM) 워투백 모델 적용#####
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

####Test 사이트(SITE_NM) 워투백 모델 적용####
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

sum(is.na(test_final)) 
write.csv(test_final,"test_final_170513_2.csv",row.names = F)
#
train_final<-read.csv("train_final_170513_2.csv")
test_final<-read.csv("test_final_170513_2.csv")
