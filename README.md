# Classification-Recognition-Convulsion
class_breath = function(datax, ch, mass) {

##############INPUT########################################
  
#datax = data#
#ch = number of clusters##
#Mass of the participant#  
#############OUTPUT#########################################
  
#Out = list of accuracies for KNN, naive, SVM, decision tree  

  
################Librraiers##################################
  
library('cardidates')
library('biosignalEMG')
library('psd')
library('pracma')
library('cluster')
library('scatterplot3d')
library('kknn')
library('colorspace')
library('gridExtra')
library('MASS')
library('ggplot2')
library('sClust')
library('dbscan')
library('TTR')
library('car')
##############acceleration estimation

mass_chest = mass*0.21
datax = (datax/mass_chest)-9.8

##############Data Loading############

X = envelope(as.emg.data.frame(datax), 1, "RMS", 50)
X = matrix(X$values)
X_dev = diff(X)/0.001
X_dev2 = diff(X_dev)/0.01
data = cbind.data.frame(X[1:nrow(X_dev2),], X_dev[1:nrow(X_dev2),])                            #X_dev2[5000:nrow(X_dev2),])
data = na.omit(data)
colnames(data) = c('x','y')

#############Levenes test#######

categ = cbind.data.frame(rep('H', 10000), rep('C', 10000))
com = as.factor(matrix(unlist(categ)))
test = cbind.data.frame(datax, com)
colnames(test) = c('Cond', 'catg')
testl = leveneTest(Cond~catg,test)

#########Cluster Analysis (k means)###

library(zoo)
library(dplyr)
mm = as.data.frame(matrix(0, ncol = 2, nrow = floor(nrow(X_dev2)/2)))
Feed = as.data.frame(matrix(0, ncol = 2, nrow = floor(nrow(X_dev2)/2)))
for (i in 1:2) 
  mm[,i] = sample(sort(data[1:floor(nrow(X_dev2)/2),i], decreasing = TRUE), floor(nrow(X_dev2)/2))
colnames(mm) = c('x','y')
H = as.data.frame(sample(sort(data[10000:nrow(X_dev2),1], decreasing = TRUE),floor(nrow(X_dev2)/2), replace = FALSE))
B = as.data.frame(sample(sort(data[10000:nrow(X_dev2),2], decreasing = TRUE),floor(nrow(X_dev2)/2), replace = FALSE))
AA = cbind.data.frame(H,B) 
colnames(AA) = c('x','y')
OUT = combine(AA, mm)

###############Upper envelopes############
E = list()
OK = matrix(NA, nrow = nrow(OUT), ncol = 1)
for (i in 1:2) {
  E[[i]] = rollapply(OUT[,i],25,max)
}

##############Lower envelopes#############
G = list()
for (i in 1:2) {
  G[[i]] = rollapply(OUT[,i],25,min)  
}
Feed1 = cbind.data.frame(E[[1]], E[[2]])
colnames(Feed1) = c('x','y')
Feed2 = cbind.data.frame(G[[1]], G[[2]])
colnames(Feed2) = c('x','y')
Feed = data.frame(combine(Feed1, Feed2))
set.seed(1234)
CL = kmeans(Feed, ch, iter.max = 1000)

plot(Feed, col = CL$cluster)

######## Classification##########
library(class)
library(e1071)
library(caret)
library(party)
library(naivebayes)
clusterss = factor(CL$cluster)
Cdata = cbind(Feed, clusterss)
colnames(Cdata) = c('X','Y', 'Phases')
set.seed(123)
train_ind <- sample(seq_len(nrow(Cdata)), size = floor(0.75*nrow(Cdata)), replace = FALSE)

train <- Cdata[train_ind, ]
test <- Cdata[-train_ind, ]

svmfit = svm(Phases~., train, kernel = "linear", cost = 625, scale = FALSE)
treefit = ctree(Phases~., train)
naivefit = naive_bayes(Phases~., train, usekernel = TRUE)
knnfit = knn(train[,1:2], test[,1:2], train$Phases, 3)
prvaluesvm = predict(svmfit, test)
prvaluetree = predict(treefit, test)
prvaluenaive = predict(naivefit, test)
accuracyknn = table(test$Phases, knnfit)
accuracysvm = table(test$Phases, prvaluesvm)
accuracytree = table(test$Phases, prvaluetree)
accuracynaive = table(test$Phases, prvaluenaive)
knn_acc = sum(diag(accuracyknn))/sum(accuracyknn)
nai_acc = sum(diag(accuracynaive))/sum(accuracynaive)
svm_acc = sum(diag(accuracysvm))/sum(accuracysvm)
tre_acc = sum(diag(accuracytree))/sum(accuracytree)
output = list(knn_acc, nai_acc, svm_acc, tre_acc)
return(output)
}
