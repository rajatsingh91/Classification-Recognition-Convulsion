# Classification-Recognition-Convulsion (Refer to the classification file name coded on R studio
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

