library(tidyverse)


library(mlbench)

data("PimaIndiansDiabetes")

df <- PimaIndiansDiabetes[c(3:5, 8)]

str(df)

summary(df)
#상관관계 분석
cor(x = df, method = "pearson")

cor(x = df, method = "spearman")


install.packages("corrplot")
library(corrplot)
window(width = 12, height = 10)
corrplot(cor(df), method = "circle", type = "lower")

shapiro.test(df$triceps)

cor.test(df$triceps, df$insulin, method = "kendall")

df1 <- PimaIndiansDiabetes

#파생변수 생성
df1 <- df1 %>% 
  mutate(age_num = cut(df1$age, c(20, 40, 60, 100), right = FALSE,
                       label = c("young", "middle", "old")))

#주성분 분석
table(df1$age_num)

pcairis <- princomp(iris[, -5],
                    cor = FALSE,
                    scores = TRUE)

summary(pcairis)

plot(pcairis, type = "l", main = "PCA")

pcairis$loadings

pcairis$scores

biplot(pcairis)

biplot(pcairis, scale = 0, main = "PCA")

q()
#회귀

install.packages("Lahman")
library(Lahman)


install.packages("ISLR")
library(ISLR)

hitters <- na.omit(Hitters)

summary(hitters)

hit <- lm(Salary ~ ., data = hitters)
summary(hit)

hit_md <- lm(Salary ~ AtBat + Hits + Walks + CWalks + Division + PutOuts,
             data = hitters)

hit_md1 <- step(hit_md, direction = "backward")

#로지스틱 회귀

str(Default)
summary(Default)

def <- Default

set.seed(202012)
train <- sample(1:nrow(def),
                size = 0.8*nrow(def),
                replace = FALSE)
test <- (-train)

deftrain <- def[train,]
deftest <- def[test,]
model <- glm(default ~ .,
             family = binomial,
             data = deftrain)

setp_modle <- step(model, direction = "both")

summary(setp_modle)

library(car)
vif(setp_modle)

#의사결정나무
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
md <- rpart(Species ~., data = iris)

plot(md, compress = TRUE, margin = 0.5)
text(md, cex = 1)

prp(md, type = 2, extra = 2)

ls(md)

md$cptable
install.packages("caret")
library(caret)

treemd <- predict(md,
                  newdata = iris,
                  type = "class")
confusionMatrix(treemd, reference = iris$Species)


# svm(서포트벡터머신)

install.packages('e1071')
library(e1071)

modle1 <- svm(Species ~ .,data = iris)

pred <- predict(modle1, iris)

confusionMatrix(data = pred, reference = iris$Species)


#k-nn
install.packages("class")
library(class)

da <- iris[, c("Sepal.Length", "Sepal.Width", "Species")]
set.seed(1234)

idx <- sample(x = c("train","valid", "test"),
              size = nrow(da),
              replace = TRUE, prob = c(3,1,1))

tr <- da[idx == "train",]
va <- da[idx == "valid",]
te <- da[idx == "test",]

trx <- tr[, -3]
vax <- va[, -3]
tex <- te[, -3]

try <- tr[, 3]
vay <- tr[, 3]
tey <- tr[, 3]

knn1 <- knn(train = trx,
            test = vax,
            cl = try,
            k = 1)

knn2 <- knn(train = trx,
            test = vax,
            cl = try,
            k = 2)

ack <- NULL

for(i in c(1:nrow(trx))){
  
  set.seed(1234)
  knnk <- knn(train = trx, test = vax, cl = try, k = i)
              ack <- c(ack,
              sum(knnk == vay)/length(vay))
              
}

vak <- data.frame(k = c(1:nrow(trx)),
                  accuracy = ack)

plot(formula = accuracy ~ k,
     data = vak,
     type = "o",
     pch = 20,
     main = "validation - optimal k")


#ANN
install.packages("nnet")
library(nnet)
iris

irisscale <- cbind(scale(iris[-5]),
                   iris[5])
scale(iris[5])

irisscale

set.seed(1000)

index <- c(sample(1 : 50, size = 35),
           sample(51 : 100, size = 35),
           sample(101 : 150, size = 35))

train <- irisscale[index,]
test <- irisscale[-index,]

set.seed(1234)


model_ann <- nnet(Species ~ .,
                  data = train,
                  size = 2,
                  maxit = 200,
                  decay = 5e-04)

summary(model_ann)

#naive bayes
library(e1071)

train_data <- sample(1 : 150, size = 100) #100개의 1~150랜덤 숫자 생성

navie <- naiveBayes(Species ~ .,
                    data = iris,
                    subset = train_data)

navie

pred <- predict(navie, newdata = iris)
confusionMatrix(pred, reference = iris$Species) #혼돈행렬(예측값, 실제값)

#배깅

library(mlbench)

data("PimaIndiansDiabetes2")
str(PimaIndiansDiabetes2)


PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)
summary(PimaIndiansDiabetes2)

train_index <-  sample(1 : nrow(PimaIndiansDiabetes2),
                       size = nrow(PimaIndiansDiabetes2)*2/3)

train <- PimaIndiansDiabetes2[train_index,]
test <- PimaIndiansDiabetes2[-train_index,]

install.packages("ipred")


library(ipred)

bagg <- bagging(diabetes~.,
                data = train, nbagg = 25)

bagg

pred <- predict(bagg, test)
confusionMatrix(as.factor(pred),
                reference = test$diabetes,
                positive = "pos")

#boosting

install.packages("xgboost")
library(xgboost)

train_lable <- as.integer(train$diabetes) - 1
mattrain_data <- as.matrix(train[, -9])
mattest_data <- as.matrix(train[, -9])

xgb_train <- xgb.DMatrix(data = mattrain_data,
                         label = train_lable)

xgb_test <- xgb.DMatrix(data = mattest_data)

param_list <- list(booster = "gbtree",
                   eta = 0.001,
                   max_depth = 10,
                   gamma = 5,
                   subsample = 0.8,
                   colsample_bytree = 0.8,
                   objective = "binary : logistic",
                   eval_metric = "auc")

md_xgb <- xgb_train(params = param_list,
                    data = xgb_train,
                    nrounds = 200,
                    early_stopping_rounds = 10,
                    watchlist =  list(val1 = xgb_train),
                    verbose = 1)


library(faraway)
library(MASS)
library(randomForest)
library(pROC)
library(caret)
library(ModelMetrics)
library(dplyr)
##################################################
a <- read.csv("job.csv")




b <- a %>% 
  rename("id" = "癤풦D")

b$Warehouse_block <- ifelse(b$Warehouse_block == "A", 1,
                            ifelse(b$Warehouse_block == "B", 2,
                                   ifelse(b$Warehouse_block == "C", 3,
                                          ifelse(b$Warehouse_block == "D", 4, 5))))

b$Mode_of_Shipment <- ifelse(b$Mode_of_Shipment =='Flighit', 1,
                             ifelse(b$Mode_of_Shipment == 'Road', 2, 3))

b$Product_importance <- ifelse(b$Product_importance == 'high', 1,
                               ifelse(b$Product_importance == 'medium', 2, 3))

str(b)      
b$Gender <- ifelse(b$Gender == 'F', 1, 0)

b$Reached.on.Time_Y.N <- as.factor(b$Reached.on.Time_Y.N)

set.seed(1234)

idx <- sample(1:nrow(b), nrow(b)*0.8, replace = F)

trainb <- b[idx,]
testb <- b[-idx,]

glmb <- glm(Reached.on.Time_Y.N ~ .,
            data = trainb,
            family = "binomial")

predglmb <- predict(glmb, testb[, -12], type = "response")

auc(testb[, 12], predglmb)

final <- data.frame(id = testb$id,
                    Reached.on.Time_Y.N = predglmb)

str(b)

final %>% 
  write.csv("fianl.csv")

##################################################

rfb <- randomForest(Reached.on.Time_Y.N ~ .,
                    data = trainb,
                    ntree = 150,
                    mtry = sqrt(12),
                    importance = T)

predrfb <- predict(rfb, testb[, -12], type = "prob")

auc(testb[,12], predrfb[,2])


final <- data.frame(custid = testb$id,
                    abc = predrfb)

########################################

a <- read.csv("titanic.csv")
table(a$Age)
set.seed(1234)
b <- a[,-c( 4, 9, 11)]
mean(b$Age, na.rm = T)
b$Age <- ifelse(is.na(b$Age), 30, b$Age)

str(b)
b$Sex <- ifelse(b$Sex == "male", 1, 2)
b$Survived <- as.factor(b$Survived)
b$Embarked <- ifelse(b$Embarked == "S", 1,
                     ifelse(b$Embarked == "c", 2, 3))

idx <- sample(1:nrow(b), nrow(b)*0.8, replace = FALSE)

train <- b[idx,]
test <- b[-idx,]

## 랜포
str(b)
rft <- randomForest(Survived ~ .,
                    data =train,
                    ntree = 300,
                    mtry = sqrt(9),
                    importance = TRUE)

predrft <- predict(rft, test[, -2], type = "prob")


auc(test[,4], predrft[,1])

#로지스틱

glt <- glm(Survived~ .,
            data = train,
           family = "binomial")

predt <- predict(glt, test[,-2], type = "response")

auc(test[, 2], predt)

#######테스트 데이터 
titanictest <- read.csv("titanictest.csv")
titanictest$Survived <- NA

titanictest1 <- titanictest[, -c(3, 8, 10)]
mean(titanictest1$Age, na.rm = TRUE)

titanictest1$Age <- ifelse(is.na(titanictest1$Age), 30, titanictest1$Age)
titanictest1$Sex <- ifelse(titanictest1$Sex == 'male', 1, 2)
titanictest1$Embarked <- ifelse(titanictest1$Embarked == "S", 1,
                     ifelse(titanictest1$Embarked == "c", 2, 3))

#######테스트와 로지스틱

titanicpred <- predict(glt, titanictest1[, -9], type = "response")

final <- data.frame(PassengerID = titanictest$PassengerId,
                    Survived = titanicpred)



######테스트와 랜덤포레스트

titanicpred <- predict(rft, titanictest1[, -9], type = "prob")

final <- data.frame(PassengerID = titanictest$PassengerId,
                    Survived = titanicpred)

##
a <- read.csv('customer.csv')

summary(a)

mean(a$TotalCharges, na.rm = T)

b <- a
b$TotalCharges <- ifelse(is.na(b$TotalCharges), 2283.3, b$TotalCharges)

summary(b)

b <- b[,-1]
table(b$PaymentMethod)
b$gender <- ifelse(b$gender == 'Male', 1, 2)
b$Partner <- ifelse(b$Partner == 'Yes', 1, 2)
b$Dependents <- ifelse(b$Dependents == 'Yes', 1, 2)
b$tenure <- b$tenure/12
b$PhoneService <- ifelse(b$PhoneService == 'Yes', 1, 2)
b$MultipleLines <- ifelse(b$MultipleLines == 'Yes', 1,
                          ifelse(b$MultipleLines == 'No', 2, 3))
b$InternetService <- ifelse(b$InternetService == 'DSL', 1,
                            ifelse(b$InternetService == 'No', 3, 2))
b$OnlineSecurity <- ifelse(b$OnlineSecurity == 'Yes', 1,
                           ifelse(b$OnlineSecurity == 'No', 2, 3))
b$OnlineBackup <- ifelse(b$OnlineBackup == 'Yes', 1,
                           ifelse(b$OnlineBackup == 'No', 2, 3))
b$DeviceProtection <- ifelse(b$DeviceProtection == 'Yes', 1,
                             ifelse(b$DeviceProtection == 'No', 2, 3))
b$TechSupport <- ifelse(b$TechSupport == 'Yes', 1,
                        ifelse(b$TechSupport == 'No', 2, 3))
b$StreamingTV <- ifelse(b$StreamingTV == 'Yes', 1,
                        ifelse(b$StreamingTV == 'No', 2, 3))
b$StreamingMovies <- ifelse(b$StreamingMovies == 'Yes', 1,
                        ifelse(b$StreamingMovies == 'No', 2, 3))
b$Contract <- ifelse(b$Contract == 'Month-to-month', 1,
                            ifelse(b$Contract == 'One year', 2, 3))
b$PaperlessBilling<- ifelse(b$PaperlessBilling == 'Yes', 1, 2)
b$PaymentMethod <- ifelse(b$PaymentMethod == 'Bank transfer (automatic)', 1,
                     ifelse(b$PaymentMethod == 'Credit card (automatic)', 2,
                            ifelse(b$PaymentMethod == 'Electronic check', 3, 4)))
b$MonthlyCharges <- scale(b$MonthlyCharges)
b$TotalCharges <- scale(b$TotalCharges)
b$Churn <- as.factor(b$Churn)
cor(x = b, method = 'pearson')

step(lm(Churn ~ .,
        data = b))

b$Churn <- as.factor(b$Churn)
###############################
idx <- sample(1:nrow(b), nrow(b)*0.8, replace = FALSE)

train <- b[idx,]
test <- b[-idx,]

glb <- glm(Churn ~ .,
           data = b,
           family = "binomial")

pred <- predict(glb, test[,-20], type = "response")

auc(test[,20], pred)
###############################

rfb <- randomForest(Churn ~ .,
                    data = b,
                    ntree = 70,
                    mtry = sqrt(20),
                    importance = TRUE)

pred <- predict(rfb, test[,-20], type = "prob")

auc(test[,20], pred[,2])

confusionMatrix(as.factor(pred),
                refrence = test$Churn,
                positive = "pos")

######################
a <- read.csv("Loan payments data.csv")

b <- a
b <- b[, -c(1, 7, 8)]
str(b)

table(b$Gender)

b$loan_status <- ifelse(b$loan_status == 'COLLECTION', 1,
                        ifelse(b$loan_status == 'COLLECTION_PAIDOFF', 2, 3))

b$education <- ifelse(b$education == 'Bechalor', 1,
                      ifelse(b$education == 'college', 2,
                             ifelse(b$education == 'High School or Below', 3, 4)))
b$Gender <- ifelse(b$Gender == 'male', 1, 2)
b$loan_status <- as.factor(b$loan_status)

idx <- sample(1:nrow(b), nrow(b)*0.8, replace = FALSE)

train <- b[idx,]
test <- b[-idx,]



glb <- glm(loan_status ~ .,
           data = b,
           family = 'binomial')

perd <- predict(glb, test[,-1], type = "response")
auc(test[,1], perd)


sale <- read.csv("sales_train_v2.csv", header = T,
                 fileEncoding = "UTF-8-BOM")

b <- sale %>% 
  group_by(item_id) %>% 
  summarise(n = n()) %>%
  arrange(desc(n))

c <- sd(sale$item_price)
d <- sale %>% 
  filter(item_id == 20949 | item_id == 5822 | item_id == 17717)

e <- sd(d$item_price)

c - e

1312/1564*100
100 - 68.4

(1312 - 1099)/1099 *100
(132 - 84)/84 *100
2/4