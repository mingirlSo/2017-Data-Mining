##### 텍스트 데이터 word2vec 적용 

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