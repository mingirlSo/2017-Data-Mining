#################################candidate 

library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape)
library(caret)
library(ROCR)
library(matrixStats)

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

write.csv(custsig.train, file="custsig_train.csv")

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

write.csv(custsig.test, file="custsig_test.csv")


#########Module 3 : Run Testing############

custsig.train <- read.csv("custsig_train.csv", stringsAsFactors = F)
custsig.test <- read.csv("custsig_test.csv", stringsAsFactors = F)


#

test.public <- read.csv("test_public.csv", stringsAsFactors = F)
tfi<-merge(test.public,custsig.train,by="CUS_ID")
tfi$GENDER<-ifelse(tfi$F20. | tfi$F30 | tfi$F40. ==1,"F","M")
tfi$AGE<-ifelse(tfi$F20. | tfi$M20. ==1,"20-",ifelse(tfi$F30 | tfi$M30 ==1,"30","40+"))
tfi$GROUP<-paste(tfi$GENDER,tfi$AGE,sep="")
colnames(tfi)
tfi2<-tfi[,-c(2:7,297,298)]
custsig.train_40<-rbind(custsig.train,tfi2)
str(custsig.train_40)
custsig.train_40$GROUP<-factor(make.names(custsig.train_40$GROUP,unique = F,allow_ = T))

#choosing effective variables
custsig.effc <- dplyr::select(custsig.train_40, 
                              c(GROUP, ttl_pv, starts_with("pv"), 
                                starts_with("time"),starts_with("mth"), coef_var_cat, 
                                ttl_vis_day, mon, tue, wed, thu, fri, sat, starts_with("time"),
                                coef_var_day, coef_var_time, coef_var_mth,
                                mact_게임전문지웹진, mact_종교, mact_일간지, 
                                mact_부동산, mact_커뮤니티포털,mact_군대국방,
                                mact_교육기관단체, mact_사회복지, mact_모바일게임, 
                                mact_화장품미용, mact_웹서비스, mact_운송, mact_포털, 
                                mact_골프, mact_의류패션잡화, mact_의류쇼핑몰, 
                                mact_아이템거래, mact_경매, mact_가정용품, mact_다운로드,
                                mact_자동차, mact_인터넷비즈니스, mact_분야별커뮤니티,
                                mact_도메인호스팅, mact_네트워크, mact_쇼핑기타, 
                                mact_학생교과교육, mact_가전쇼핑몰, mact_의료, 
                                mact_건강의학정보, mact_도박))
#training/test set for group
custsig.group <- dplyr::select(custsig.effc, c(-GENDER, -AGE))
custsig.group <- custsig.effc

custsig.group$GROUP[custsig.group$GROUP == "F20-"]<-"F20"
custsig.group$GROUP[custsig.group$GROUP == "F40+"]<-"F40"
custsig.group$GROUP[custsig.group$GROUP == "M20-"]<-"M20"
custsig.group$GROUP[custsig.group$GROUP == "M40+"]<-"M40"

custsig.group$GROUP <- factor(custsig.group$GROUP)

#train XGboost model and save
grp_model_xgbTree <- caret::train(GROUP ~ .,
                                  data = custsig.group, 
                                  method = "xgbTree", 
                                  metric = "logLoss",
                                  trControl = trainControl(method = "repeatedcv", number = 5, 
                                                           repeats = 2, classProbs=T, 
                                                           summaryFunction=mnLogLoss, 
                                                           verboseIter = TRUE))




#predict
pred.grp.xgb <- predict(grp_model_xgbTree, custsig.test, type="prob")
colnames(pred.grp.xgb) <- c("F20-", "F30", "F40+", "M20-", "M30", "M40+")
pred.grp.xgb$CUS_ID <- custsig.test$CUS_ID
pred.grp.xgb <- pred.grp.xgb[c(7,1:6)]

head(pred.grp.xgb)

#train RandomForest model and save

grp_model_rf <- caret::train(GROUP ~ .,
                             data = custsig.group, 
                             method = "rf", 
                             metric = "logLoss",
                             trControl = trainControl(method = "repeatedcv", number = 5, 
                                                      repeats = 2, classProbs=T, 
                                                      summaryFunction=mnLogLoss, 
                                                      verboseIter = TRUE))






#predict
pred.grp.rf <- predict(grp_model_rf, custsig.test, type="prob")
colnames(pred.grp.rf) <- c("F20-", "F30", "F40+", "M20-", "M30", "M40+")
pred.grp.rf$CUS_ID <- custsig.test$CUS_ID
pred.grp.rf <- pred.grp.rf[c(7,1:6)]

head(pred.grp.rf)

#averaging xgb and rf
pred.grp <- (pred.grp.xgb + pred.grp.rf) / 2
head(pred.grp)

write.csv(pred.grp, file="group_prediction.csv", row.names = F)
write.csv(pred.grp.rf, file="group_prediction_rf.csv", row.names = F)
write.csv(pred.grp.xgb, file="group_prediction_xgb.csv", row.names = F)