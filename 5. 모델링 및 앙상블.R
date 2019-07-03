##### 모델링  (간략화)

library(caret)

control <- trainControl(method="cv", number=5, repeats=3, classProbs=TRUE, summaryFunction=mnLogLoss)
# Used models
methods <- c("xgbTree","rf","svmRadial","nnet","gbm","glmnet","fda","pda") # add methods such as xgbTree, rf, svmRadious, etc.

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
  fname <- paste("act200+site200_", methods[i], ".csv", sep="")
  write.csv(cbind(CUS_ID=test200$CUS_ID,pred[[1]]), fname, row.names = F)
}



##### nnet 의 경우 scale 할 것. 
rain<-read.csv("train_final_w2v_all.csv")
test<-read.csv("test_final_w2v_all.csv")

a<-data.frame(scale(train[,-c(1,2)]))
b<-data.frame(scale(test[,-1]))
train<-cbind(train[1:2],a)
test<-cbind(test[1],b)

train$GROUP<-make.names(train$GROUP)

##### 앙상블 기법

#########################################################################
# Creating ensembles from submission files: Geometric averaging
# For more details refer to https://mlwave.com/kaggle-ensembling-guide/
#########################################################################

### Merge your submission files
nf <- 0
for (f in list.files(pattern="*.csv")) {
  if (nf == 0) pred <- read.csv(f)
  else pred <- merge(pred, read.csv(f), by="CUS_ID")
  nf = nf + 1
}

### Calculate geometric mean
nc <- ncol(pred)
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}

### Divide each row by the row sum (required to sum to one)
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

### Save your ensemble submission file ==> You will get an improvement on the test set!
names(pred) <- c("CUS_ID", "F20-", "F30", "F30+", "M20-", "M30", "M30+")
write.csv(pred, "submission_ensemble.csv", row.names = F)


##### 앙상블 적용 

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