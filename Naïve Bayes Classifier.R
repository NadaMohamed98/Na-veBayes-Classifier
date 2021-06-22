############################################
#model1: predict individual's income
############################################
setwd("E:/collage/Fourth Year/Distributed Computation/Task")
library(e1071)
#read data into a table and convert it into data frame
data <- read.table("nbtrain.csv", header = TRUE, sep=",")

#split the dataset (train = 9010, test = 1000)
traindata <- as.data.frame(data[1:9010,])
testdata <- as.data.frame(data[9011:10010,])

#create naive Bayes model
model1 <- naiveBayes(income ~ age + gender + educ,traindata)

model1

#predict using testdata
results1 = predict(model1, testdata)
results1
###########################################################
#calculate confusion matrix
###########################################################
conf1 <- table(actual = testdata$income, predicted=results1)
conf1

############################################################
#calculate score
############################################################
score1 <-sum(diag(conf1))
score1
############################################################
#calculate accuracy
############################################################
acc_1 <- sum(diag(conf1))/sum(conf1)
acc_1*100
################################################################################
#Model2: Predict individual's gender
###########################################################################
#split the dataset (train = 9010, test = 1000)
traindata <- as.data.frame(data[1:9010,])
testdata <- as.data.frame(data[9011:10010,])

#create naiveBayes model
model2 <- naiveBayes(gender ~ age + income + educ,traindata)
model2

#predict using test data
results2 = predict(model2, testdata)
results2
###########################################################
#calculate confusion matrix
###########################################################
conf2 <- table(actual = testdata$gender, predicted=results2)
conf2

############################################################
#calculate score
############################################################
#score2 <-sum(diag(conf2))
#score2
############################################################
#calculate accuracy
############################################################
acc_2 <- sum(diag(conf2))/sum(conf2)
acc_2*100
############################################################
#part3
###########################################################
#for gender
###########################################################
#divide train data
part_f=traindata[traindata$gender == 'F', ]
part_m=traindata[traindata$gender == 'M', ]

#select Randon records
f_sample <- part_f[sample(nrow(part_f), 3500), ]
m_sample <- part_m[sample(nrow(part_m), 3500), ]

#combine the two partitions
new_traindata <- rbind(f_sample, m_sample)

model3 <- naiveBayes(gender ~ age + income + educ,new_traindata)
model3

#predict using test data
results3 = predict(model3, testdata)
results3
###########################################################
#calculate confusion matrix
###########################################################
conf3 <- table(actual = testdata$gender, predicted=results3)
conf3

############################################################
#calculate accuracy
############################################################
acc_3 <- sum(diag(conf3))/sum(conf3)
acc_3*100

############################################################
#for income
############################################################
#select Random records
f_sample <- part_f[sample(nrow(part_f), 3500), ]
m_sample <- part_m[sample(nrow(part_m), 3500), ]

#combine the two partitions
new_traindata <- rbind(f_sample, m_sample)

model4 <- naiveBayes(income ~ age + gender + educ,new_traindata)
model4

#predict using test data
results4 = predict(model4, testdata)
results4
###########################################################
#calculate confusion matrix
###########################################################
conf4 <- table(actual = testdata$income, predicted=results4)
conf4
############################################################
#calculate accuracy
############################################################
acc_4 <- sum(diag(conf4))/sum(conf4)
acc_4*100
############################################################
