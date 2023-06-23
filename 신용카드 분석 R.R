library(tidyverse)
library(readxl)
library(pROC)
library(randomForest)
library(caret)
library(car)
library(nnet)
set.seed(1234)
cardtrain <- read.csv("C:/Users/ghkdw/OneDrive/바탕 화면/황준하/DACON/신용카드/train.csv")
cardtest <- read.csv("C:/Users/ghkdw/OneDrive/바탕 화면/황준하/DACON/신용카드/test.csv")
cardsub <- read.csv("C:/Users/ghkdw/OneDrive/바탕 화면/황준하/DACON/신용카드/sample_submission.csv")

str(cardtrain)
#index제거
cardtrain1 <- cardtrain[2 : 20]

# 성별, 자동차, 부동산 같은 문자열 숫자로 변환
cardtrain1$gender <- ifelse(cardtrain1$gender == 'M', 1, 0)
cardtrain1$car <- ifelse(cardtrain1$car == 'N', 0, 1)
cardtrain1$reality <- ifelse(cardtrain1$reality == 'N', 0, 1)

summary(cardtrain1$income_total)


hist(cardtrain1$income_total)
#소득을 11구간으로 나눈 income category 만들기
cardtrain1$income_total <- ifelse(cardtrain1$income_total <= 100000, 1,
                                     ifelse(cardtrain1$income_total <= 200000, 2,
                                            ifelse(cardtrain1$income_total <= 300000, 3,
                                                   ifelse(cardtrain1$income_total <= 400000, 4,
                                                          ifelse(cardtrain1$income_total <= 500000, 5,
                                                                 ifelse(cardtrain1$income_total <= 600000, 6,
                                                                        ifelse(cardtrain1$income_total <= 700000, 7,
                                                                               ifelse(cardtrain1$income_total <= 800000, 8,
                                                                                      ifelse(cardtrain1$income_total <= 900000, 9,
                                                                                             ifelse(cardtrain1$income_total <= 1000000, 10, 11))))))))))
hist(cardtrain1$income_total)

#income type
table(cardtrain1$income_type)

cardtrain1$income_type <- ifelse(cardtrain1$income_type == 'Commercial associate', 1,
                                 ifelse(cardtrain1$income_type == 'Pensioner', 2,
                                        ifelse(cardtrain1$income_type == 'State servant', 3,
                                               ifelse(cardtrain1$income_type == 'Student', 4, 5))))
table(cardtrain1$income_type)

# edu_tyep
table(cardtrain$edu_type)

cardtrain1$edu_type <- ifelse(cardtrain1$edu_type == 'Academic degree', 1,
                              ifelse(cardtrain1$edu_type == 'Higher education', 2,
                                     ifelse(cardtrain1$edu_type == 'Incomplete higher', 3,
                                            ifelse(cardtrain1$edu_type == 'Lower secondary', 4, 5))))
table(cardtrain1$edu_type)

#family
table(cardtrain1$family_type)

cardtrain1$family_type <- ifelse(cardtrain1$family_type == 'Civil marriage', 1,
                                 ifelse(cardtrain1$family_type == 'Married', 2,
                                        ifelse(cardtrain1$family_type == 'Separated', 3,
                                               ifelse(cardtrain1$family_type == 'Single / not married', 4, 5))))
table(cardtrain1$family_type)

#house_type
table(cardtrain1$house_type)

cardtrain1$house_type <- as.factor(cardtrain1$house_type)
cardtrain1$house_type <- as.numeric(cardtrain1$house_type)

table(cardtrain1$house_type)

#days_birth
cardtrain1$DAYS_BIRTH <- abs(cardtrain1$DAYS_BIRTH)
cardtrain1$DAYS_BIRTH <- round(cardtrain1$DAYS_BIRTH/365)
hist(cardtrain1$DAYS_BIRTH)

#days_employed
hist(cardtrain1$DAYS_EMPLOYED)
cardtrain1$DAYS_EMPLOYED <- ifelse(cardtrain1$DAYS_EMPLOYED >= 0, 0, cardtrain1$DAYS_EMPLOYED)
cardtrain1$DAYS_EMPLOYED <- cardtrain1$DAYS_EMPLOYED / 365
cardtrain1$DAYS_EMPLOYED <- abs(cardtrain1$DAYS_EMPLOYED)
#일하기 시작한 시점부터 1년차라 취급하기 위해 올림으로 적용
cardtrain1$DAYS_EMPLOYED <- ceiling(cardtrain1$DAYS_EMPLOYED)

#occyp_type(직업유형)
table(cardtrain1$occyp_type)
cardtrain1$occyp_type <- as.factor(cardtrain1$occyp_type)
cardtrain1$occyp_type <- as.numeric(cardtrain1$occyp_type)
# 다 1빼서 직업유형이 없는 사람은 0 그외에는 1부터 시작하게 만들기
cardtrain1$occyp_type <- cardtrain1$occyp_type - 1

cardtrain1$credit <- as.factor(cardtrain1$credit)

cardtrain1$begin_month <- abs(cardtrain1$begin_month)

table(cardtrain1$family_size)

#다중로지스틱을 해봄
idx <- sample(1 :nrow(cardtrain1), nrow(cardtrain1)*0.8, replace = F)

train <- cardtrain1[idx,]
test <- cardtrain1[-idx,]


multglm <- multinom(credit ~ .,
                    data = train)

fitted(multglm)
predmglm <- predict(multglm, data = test, type = 'prob')

multiclass.roc(train$credit, predmglm)

#파생변수 만들어보기

cardtrain2 <- cardtrain1
cardtrain2$moneypower <- cardtrain2$car + cardtrain2$reality + cardtrain2$income_total

cardtrain2 <- cardtrain2[,-c(2,3,5)]
cardtrain2$credit <- as.factor(cardtrain2$credit)
idx <- sample(1 :nrow(cardtrain2), nrow(cardtrain2)*0.8, replace = F)
train <- cardtrain2[idx,]
test <- cardtrain2[-idx,]

rfc <- randomForest(credit ~ .,
                    data = train,
                    ntree = 100,
                    mtry = sqrt(17),
                    importance = TRUE)

###roc 그래프 연습
predrfc <- predict(rfc, test, type = 'response')
view(predrfc)
str(predrfc)
predrfc <- as.numeric(predrfc)
mr <- multiclass.roc(test$credit, predrfc)

pROC::plot.roc(mr$rocs[[1]])
pROC::plot.roc(mr$rocs[[2]])
pROC::plot.roc(mr$rocs[[3]])

## 예측
predrfc <- predict(rfc, test, type = 'prob')
view(predrfc)
mr <- multiclass.roc(test$credit, predrfc)
mr


##test 데이터셋
str(cardtest)
cardtest1 <- cardtest[2 : 19]
cardtest1

cardtest1$credit <- NA


#######전처리
cardtest1$gender <- ifelse(cardtest1$gender == 'M', 1, 0)
cardtest1$car <- ifelse(cardtest1$car == 'N', 0, 1)
cardtest1$reality <- ifelse(cardtest1$reality == 'N', 0, 1)


cardtest1$income_total <- ifelse(cardtest1$income_total <= 100000, 1,
                                  ifelse(cardtest1$income_total <= 200000, 2,
                                         ifelse(cardtest1$income_total <= 300000, 3,
                                                ifelse(cardtest1$income_total <= 400000, 4,
                                                       ifelse(cardtest1$income_total <= 500000, 5,
                                                              ifelse(cardtest1$income_total <= 600000, 6,
                                                                     ifelse(cardtest1$income_total <= 700000, 7,
                                                                            ifelse(cardtest1$income_total <= 800000, 8,
                                                                                   ifelse(cardtest1$income_total <= 900000, 9,
                                                                                          ifelse(cardtest1$income_total <= 1000000, 10, 11))))))))))


cardtest1$income_type <- ifelse(cardtest1$income_type == 'Commercial associate', 1,
                                 ifelse(cardtest1$income_type == 'Pensioner', 2,
                                        ifelse(cardtest1$income_type == 'State servant', 3,
                                               ifelse(cardtest1$income_type == 'Student', 4, 5))))


cardtest1$edu_type <- ifelse(cardtest1$edu_type == 'Academic degree', 1,
                              ifelse(cardtest1$edu_type == 'Higher education', 2,
                                     ifelse(cardtest1$edu_type == 'Incomplete higher', 3,
                                            ifelse(cardtest1$edu_type == 'Lower secondary', 4, 5))))


cardtest1$family_type <- ifelse(cardtest1$family_type == 'Civil marriage', 1,
                                 ifelse(cardtest1$family_type == 'Married', 2,
                                        ifelse(cardtest1$family_type == 'Separated', 3,
                                               ifelse(cardtest1$family_type == 'Single / not married', 4, 5))))


cardtest1$house_type <- as.factor(cardtest1$house_type)
cardtest1$house_type <- as.numeric(cardtest1$house_type)


cardtest1$DAYS_BIRTH <- abs(cardtest1$DAYS_BIRTH)
cardtest1$DAYS_BIRTH <- round(cardtest1$DAYS_BIRTH/365)


cardtest1$DAYS_EMPLOYED <- ifelse(cardtest1$DAYS_EMPLOYED >= 0, 0, cardtest1$DAYS_EMPLOYED)
cardtest1$DAYS_EMPLOYED <- cardtest1$DAYS_EMPLOYED / 365
cardtest1$DAYS_EMPLOYED <- abs(cardtest1$DAYS_EMPLOYED)
cardtest1$DAYS_EMPLOYED <- ceiling(cardtest1$DAYS_EMPLOYED)


cardtest1$occyp_type <- as.factor(cardtest1$occyp_type)
cardtest1$occyp_type <- as.numeric(cardtest1$occyp_type)
cardtest1$occyp_type <- cardtest1$occyp_type - 1


cardtest1$begin_month <- abs(cardtest1$begin_month)


cardtest1$moneypower <- cardtest1$car + cardtest1$reality + cardtest1$income_total

cardtest1 <- cardtest1[,-c(2,3,5)]

predtest <- predict(rfc, cardtest1[,-16], type = 'prob')

head(predtest)

cardsub[,2] <- predtest[,1]
cardsub[,3] <- predtest[,2]
cardsub[,4] <- predtest[,3]
write.csv(cardsub,'sample_submission.csv')
## 변수는 최대한 살리기, 숫자 차이가 큰거는 스케일링하기, 모델이 어떻게 구성되고 돌아가는지,
# 확인하기 p-vaule 이런거 잘 확인하기 다중공선성 확인 근거를 가지고 분석하기

a <- mtcars
a$minmax <- (a$qsec - min(a$qsec))/(max(a$qsec) - min(a$qsec))
a %>% 
  filter(a$minmax > 0.5) %>% 
  summarise(n = n())

