### 빅데이터마이닝:머신러닝
### 데이터과학융합스쿨 20173237 심창우

# 필요한 패키지 불러오기
install.packages("pROC")
install.packages("caret")
install.packages("InformationValue")
install.packages("ISLR")
library(pROC)
library(e1071)
library(class)
library(gmodels)
library(caret)
library(InformationValue)
library(ISLR)
library(class)

### 1번 데이터 불러오기 및 구조확인
dat <- read.csv("anesthetic.csv")
View(dat)

# roc curve, 민감도 0.875, 특이도 0.7142857

roc1 <- roc(dat$nomove~dat$conc, data = dat)
coords(roc1, "best", ret=c("threshold", "specificity", "sensitivity"))

plot(roc1)
abline(v=0.7142857, col='red')
abline(h=0.875, col='blue')

# logistic, 민감도 0.75, 특이도 0.6666667, 정확도 0.714285

log <- glm(nomove~conc, family = binomial(), data=dat)
summary(log)

set.seed(123456789)
sample <- sample(c(TRUE, FALSE), nrow(dat), replace=TRUE, prob=c(0.8,0.2))
train_log <- dat[sample,]
test_log <- dat[!sample,]
model_log <- glm(nomove~conc, family="binomial", data=train_log)

predicted <- predict(model_log, test_log, type="response")
test_log$nomove <- ifelse(test_log$nomove >=0.5, 1, 0)
optimal <- optimalCutoff(test_log$nomove, predicted)[1]
confusionMatrix(test_log$nomove, predicted)

#민감도 = 0.75
sensitivity(test_log$nomove, predicted)
#특이도 = 0.6666667
specificity(test_log$nomove, predicted)

# svm , 민감도 0.6666667, 특이도 0.5, 정확도 0.6

set.seed(5)
train_svm <- sample(1:30, 25)
classifier1 <- svm(nomove~conc, subset =train_svm, type="C", kernel="linear", data = dat)
predict(classifier1, dat[-train_svm,])

ta_svm <- table(dat$nomove[-train_svm], predict(classifier1,dat[-train_svm,]))
ta_svm

# Knn, 민감도 0.6666667, 특이도 0.75, 정확도 0.714285

set.seed(2)
index <- sample(c(1,2), nrow(dat), prob=c(0.8, 0.2), replace=T)

table(index)

train.data <- dat[index==1, -2]
train.class <- dat[index==1, 2]

test.data <- dat[index==2, -2]
test.class <- dat[index==2, 2]
test_knn <- knn(data.frame(train.data), data.frame(test.data), train.class, k=3)
ta_knn <- table(test_knn, test.class)
ta_knn



### 2번 데이터 불러오기 및 구조확인
dat2 <- read.csv("crabs-l.csv")
View(dat2)

# roc, 민감도 0.4774775, 특이도 0.9032258
roc2 <- roc(dat2$satell~dat2$weight, data = dat2)
coords(roc2, "best", ret=c("threshold", "specificity", "sensitivity"))

plot(roc2)
abline(v= 0.9032258, col='red')
abline(h= 0.4774775, col='blue')

# logistic, 민감도 0.8636364, 특이도 0.3333333, 정확도 0.68

log2 <- glm(satell~. , family = binomial(), data=dat2)
summary(log2)

set.seed(123456789)
sample <- sample(c(TRUE, FALSE), nrow(dat2), replace=TRUE, prob=c(0.8,0.2))
train2_log <- dat2[sample,]
test2_log <- dat2[!sample,]
model2_log <- glm(satell~. , family="binomial", data=train2_log)

predicted2 <- predict(model2_log, test2_log, type="response")
test2_log$satell <- ifelse(test2_log$satell >=0.5, 1, 0)
optimal2 <- optimalCutoff(test2_log$satell, predicted2)[1]
confusionMatrix(test2_log$satell, predicted2)

#민감도 = 0.8636364
sensitivity(test2_log$satell, predicted2)
#특이도 = 0.3333333
specificity(test2_log$satell, predicted2)

# svm, 민감도 0.75, 특이도 0.5, 정확도 0.67

set.seed(3)
train2_svm <- sample(1:173, 143)
classifier2 <- svm(satell ~. , subset =train2_svm, type="C", kernel="linear", data = dat2)
predict(classifier2, dat2[-train2_svm,])

ta2_svm <- table(dat2$satell[-train2_svm], predict(classifier2,dat2[-train2_svm,]))
ta2_svm

# knn, 민감도 0.8333333, 특이도 0.4118, 정확도 0.66

set.seed(6)
index2 <- sample(c(1,2), nrow(dat2), prob=c(0.8, 0.2), replace=T)

table(index2)

train.data2 <- dat2[index==1, -4]
train.class2 <- dat2[index==1, 4]

test.data2 <- dat2[index==2, -4]
test.class2 <- dat2[index==2, 4]
test2_knn <- knn(data.frame(train.data2), data.frame(test.data2), train.class2, k=3)
ta2_knn <- table(test2_knn, test.class2)
ta2_knn