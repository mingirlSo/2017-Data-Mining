##########################################################################
# trans2vec: 
# This predictive modeling uses only word2vec without feature engineering
##########################################################################

##### Required packages

# Install & load word2vec package
rm(list = ls())
version
if(!require(devtools)) install.packages("devtools"); library(devtools)
if(!require(wordVectors)) install_github("bmschmidt/wordVectors"); library(wordVectors)

# Install & load data munging packages
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(randomForest)) install.packages("randomForest"); library(randomForest)
if(!require(caret)) install.packages("caret"); library(caret)

# list objects in word2vec package
ls("package:wordVectors")

setwd("C:/Users/Admin/Desktop/simsim")
###### Fast reading and combining several files using data.table (with fread): 5 times faster than read.csv()
library(xlsx)

# train data
cs.dt <- read.xlsx("train_profiles.xlsx",1)
str(cs.dt)
cs.dt <-as.data.table(cs.dt)
cs.dt$CUS_ID<-as.numeric(cs.dt$CUS_ID)
tr.dt <- fread("train_clickstreams.tab"); tr.dt[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt, CUS_ID); setkey(tr.dt, CUS_ID) 
md.dt <- merge(cs.dt, tr.dt)


# test data
tr.t.dt <- fread("test_clickstreams.tab"); tr.t.dt[,CUS_ID:= as.numeric(CUS_ID)]
setkey(tr.t.dt, CUS_ID)


### SITE_NM 10샘플링 100차원 ##################################
f <- function(x, t) {
# Select sites accessed min times and more  
  grp <- md.dt[CUS_ID==x, GROUP] #data.table에서만 됨 , 해당 고객의 GROUP을 추출 
  itemfreq <- table(md.dt[CUS_ID==x, SITE_NM]) #특정 고객의 site별 접속수
  fitems <- itemfreq[itemfreq >= t] # t= 1, 있으면 다 필터링 
  act <- names(fitems) #site 이름
# Replace blanks in 카테고리 with underscore
  act<-sapply(act, function(x) gsub(" ", "_", x)) #공백처리 
  set.seed(1)
# Boost transactions 
  as.vector((sapply(1:10, function(x) c(grp, sample(act))))) #고객별 site 이름을 20번 샘플링
  #as.vector((sapply(1:10, function(x) c(sample(c(grp,act))))))
} 
#grp과 act를 묶은후 sample하는게 맞지않나  ?
start<-Sys.time()
items <- unlist(sapply(cs.dt$CUS_ID, f, 1)) # best performed when min = 1
write.table(items, "site10.txt", eol = " ", quote = F, row.names = F, col.names = F)

#1,2,3,4,5,6 -> F20-,F30,F40+,M20-,M30,M40+
end=Sys.time();end-start

##### Build trans2vec model
start<-Sys.time()
set.seed(12345)
model = train_word2vec("site10.txt","site10_100.bin",vectors=100,threads=1,window=5,cbow=1,negative_samples=10,iter=5,force = T)
end=Sys.time();end-start
#model <- read.binary.vectors("vec1119_2.bin") # reload the pre-trained word2vec model 

#vectors: 많을수록 좋지만 오랜 시간.걸림 
#threads: Number of threads to run training process on
#window: The size of the window (in words) to use in training
#cbows :If 1, use a continuous-bag-of-words model instead of skip-grams. 
#             Defaults to false (recommended for newcomers).
#iter :Number of passes to make over the corpus in training.
#negative_samples:Number of negative samples to take in skip-gram training. 0 means full sampling, 
#                 while lower numbers give faster training. For large corpora 2-5 may work; for smaller 
#                 corpora, 5-15 is reasonable.
#force: Whether to overwrite existing model file

" 
(word2vec performance) The main choices to make are:
- architecture: skip-gram (slower, better for infrequent words) vs CBOW (fast)
- the training algorithm: hierarchical softmax (better for infrequent words) vs negative sampling (better for frequent words, better with low dimensional vectors)
- sub-sampling of frequent words: can improve both accuracy and speed for large data sets (useful values are in range 1e-3 to 1e-5)
- dimensionality of the word vectors: usually more is better, but not always
- context (window) size: for skip-gram usually around 10, for CBOW around 5
"
### SITE_NM 20샘플링 100차원 ################
f <- function(x, t) {
  # Select sites accessed min times and more  
  grp <- md.dt[CUS_ID==x, GROUP] #data.table에서만 됨 , 해당 고객의 GROUP을 추출 
  itemfreq <- table(md.dt[CUS_ID==x, SITE_NM]) #특정 고객의 site별 접속수
  fitems <- itemfreq[itemfreq >= t] # t= 1, 있으면 다 필터링 
  act <- names(fitems) #site 이름
  # Replace blanks in 카테고리 with underscore
  act<-sapply(act, function(x) gsub(" ", "_", x)) #공백처리 
  set.seed(1)
  # Boost transactions 
  as.vector((sapply(1:20, function(x) c(grp, sample(act))))) #고객별 site 이름을 20번 샘플링
  #as.vector((sapply(1:10, function(x) c(sample(c(grp,act))))))
} 
#grp과 act를 묶은후 sample하는게 맞지않나  ?
start<-Sys.time()
items <- unlist(sapply(cs.dt$CUS_ID, f, 1)) # best performed when min = 1
write.table(items, "site20.txt", eol = " ", quote = F, row.names = F, col.names = F)

#1,2,3,4,5,6 -> F20-,F30,F40+,M20-,M30,M40+
end=Sys.time();end-start

##### Build trans2vec model
start<-Sys.time()
set.seed(12345)
model = train_word2vec("site20.txt","site20_100.bin",vectors=100,threads=1,window=5,cbow=1,negative_samples=10,iter=5,force = T)
end=Sys.time();end-start
### SITE_NM 10샘플링 /200차원 #############
start<-Sys.time()
model = train_word2vec("site10.txt","site10_100.bin",vectors=200,threads=1,window=5,cbow=1,negative_samples=10,iter=5,force = T)
end=Sys.time();end-start
##비교해보기 

### MACT ####################
### ACT #####################

##### Prediction using trans2vec + classifaction methods

### train / vector features (mean vector)############
# Get mean vector 벡터차원의 변수화 
g <- function(x, dt) {
  items <- dt[CUS_ID==x, SITE_NM]
  mvector <- model[[items, average=T]] # 각 사이트를 차원에 매핑 
  #mvector1<- as.vector(mvector)
  return(mvector)
}

fv <- t(sapply(cs.dt$CUS_ID, g, md.dt))
train <- cbind(data.frame(CUS_ID=cs.dt$CUS_ID), as.data.frame(fv))
#colnames(train)<-c("CUS_ID","F20-","F30","F40+","M20-","M30","M40+")

#각각의 열이 numeric이 아님. 

### test / vector features
### tartet과의 코사인유사도를 변수화  -- 일단 안되는걸로 pass######
# calculate the cosine similarity between items and target classes
cosi<-data.frame()
g <- function(x, dt, min) {
  itemfreq <- table(dt[CUS_ID==x, SITE_NM])
  fitems <- itemfreq[itemfreq >= min]
  #sim <- cosineSimilarity(model[[names(fitems), average=T]], model[[c("F20-","F30","F40+","M20-","M30","M40+"), average=F]])
  sim <- cosineSimilarity(model[[names(fitems), average=T]], model[[c("1","2","3","4","5","6"), average=F]])
  #사이트의 평균 벡터와 Y(label)의 벡터의 코사인유사도 
  #return(names(which.max(sim[1,])))
  #rbind(cosi,sim)
}
sapply(cs.dt$CUS_ID, g, md.dt,1)

# for train data 
fv <- t(sapply(cs.dt$CUS_ID, g, md.dt,1))
train <- cbind(data.frame(CUS_ID=cs.dt$CUS_ID), as.data.frame(fv))
head(train)

# for test data
test.CUS_ID <- unique(tr.t.dt$CUS_ID)
fv <- t(sapply(test.CUS_ID, g, tr.t.dt))
test <- cbind(data.frame(CUS_ID=test.CUS_ID), as.data.frame(fv))

### train 코사인유사도 (for 문써서 만드는 방식)################
df<-data.frame()
for (i in 1:2500 ){
  itemfreq <- table(md.dt[CUS_ID==i, SITE_NM])
  fitems <- itemfreq[itemfreq >= 1]
  #sim <- data.frame(CUS_ID=i,cosineSimilarity(model[[names(fitems), average=T]], model[[c("F20-","F30","F40+","M20-","M30","M40+"), average=F]]))
  sim <- data.frame(CUS_ID=i,cosineSimilarity(model[[names(fitems), average=T]], model[[c("1","2","3","4","5","6"), average=F]]))
  #sim1 <- sim[,c("CUS_ID","F20.","F30","F40.","M20.","M30","M40.")]
  df<-rbind(df,sim)
}
head(df)
colnames(df)<-c("CUS_ID","F20-","F30","F40+","M20-","M30","M40+")


### test 코사인유사도 ############################
####tr2<-fread("test_clickstreams.tab"); tr2[,CUS_ID:= as.numeric(CUS_ID)]
#model <- read.binary.vectors("vec2.bin")
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

### Training & Prediction #######################################
# Control parameters for model training
control <- trainControl(method="cv", number=5, repeats=1, classProbs=TRUE, summaryFunction=mnLogLoss)
# Used models
methods <- c("rf", "nnet","xgbTree") # add methods such as xgbTree, rf, svmRadious, etc.
str(train)
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
# saveRDS(models, "models.rds") 
# models <- readRDS("models.rds")

# Model comparison
results <- resamples(models)
summary(results)
xyplot(results)
modelCor(results)
splom(results)


# prediction & submission
for (i in 1:length(methods)) {
  pred <- predict(models[i], test, type="prob")
  fname <- paste("520_2submission_", methods[i], ".csv", sep="")
  write.csv(cbind(CUS_ID=test.CUS_ID,pred[[1]]), fname, row.names = F)
}
##
train_s<-train
test_s<-test
save(train_s,file="train_s.csv")
save(test_s,file="test_s.csv")

##

save(train_p,file="train_p.csv")
save(test_p,file="test_p.csv")
load("train_p.csv")
load("test_p.csv")
