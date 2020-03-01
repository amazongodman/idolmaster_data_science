
library(ggplot2)
library(GGally)
library(vctrs)
library(tidyverse)
library(skimr)
library(glmnet)
library(rpart)
library(rpart.plot)
library(Rmisc)
library(plotly)
library(magrittr)

library(randomForest)
library(xgboost)
library(data.table)
library(Matrix)






library(kernlab)
library(Metrics)
library(glmnet)

data<-read.csv("plot_data.csv")
data2<-read.csv("plot_data2.csv")
unknown_weight<-read.csv("unknown_weight.csv")
unknown_old<-read.csv("unknown_old.csv")
unknown_3size<-read.csv("unknown_3size.csv")



data[(data[,"waist"] / data[,"hight"])>=0.5,]



data[(data[,"waist"] / data[,"hight"])==sort(data[,"waist"] / data[,"hight"],decreasing = T)[1],]
data[(data[,"waist"] / data[,"hight"])==sort(data[,"waist"] / data[,"hight"],decreasing = T)[2],]
data[(data[,"waist"] / data[,"hight"])==sort(data[,"waist"] / data[,"hight"],decreasing = T)[3],]


data[(data[,"waist"] / data[,"hight"])==sort(data[,"waist"] / data[,"hight"],decreasing = T)[5],]

data[data[,"waist"]==sort(data[,"waist"],decreasing = T)[1],]
data[data[,"waist"]==sort(data[,"waist"],decreasing = T)[3],]


data[data[,"waist"]==sort(data[,"waist"],decreasing = T)[5],]













lm_model<-lm(old~hight,data=data)
summary(lm_model)


lm_model<-lm(old~weight,data=data)
summary(lm_model)



lm_model<-lm(old~waist,data=data)
summary(lm_model)
lm_model<-lm(old~bust,data=data)
summary(lm_model)
lm_model<-lm(old~hip,data=data)
summary(lm_model)




lm_model<-lm(old~hight,data=data)
summary(lm_model)

predict(lm_model,unknown_old)







#lm model

#lm_model<-lm(old~.,data=data[,c(1:6)])
#summary(lm_model)


lm_model<-lm(old~.,data=data[,c(1:6,8)])
summary(lm_model)




if(0){
  Call:
    lm(formula = old ~ ., data = data[, c(1:6, 8)])
  
  Residuals:
    Min      1Q  Median      3Q     Max 
  -7.5781 -1.7232 -0.4598  1.4356 10.2042 
  
  Coefficients:
  Estimate Std. Error t value Pr(>|t|)   
  (Intercept) -91.97824   53.89544  -1.707  0.08964 . 
  hight         0.62639    0.34593   1.811  0.07186 . 
  weight       -0.82525    0.64564  -1.278  0.20285   
  bust          0.22828    0.08364   2.729  0.00698 **
  waist        -0.01595    0.12880  -0.124  0.90158   
  hip          -0.01443    0.11073  -0.130  0.89647   
  BMI           1.73723    1.53706   1.130  0.25990   
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  
  Residual standard error: 2.898 on 178 degrees of freedom
  Multiple R-squared:  0.5044,	Adjusted R-squared:  0.4877 
  F-statistic:  30.2 on 6 and 178 DF,  p-value: < 2.2e-16
}



predict(lm_model,unknown_old)



par(mfrow=c(2,2),oma = c(1,1,2,1),mar = c(4, 4, 2, 1))
plot(lm_model)


data[c(140,144,158),]

data[-c(140,144,158),]
nrow(data)
data<-data[-c(140,144,158),]
nrow(data)



lm_model<-lm(old~.,data=data[,c(1:6,8)])
summary(lm_model)


predict(lm_model,unknown_old)











rowdata<-nrow(data)
random_ids<-sample(rowdata,rowdata*0.8)
data_training<-data[random_ids, c(1:6,8)]
data_predicting<-data[-random_ids, c(1:6,8)]

idol_svm<-ksvm(old ~., data=data_training )
idol_svm

if(0){
  Support Vector Machine object of class "ksvm" 
  
  SV type: eps-svr  (regression) 
  parameter : epsilon = 0.1  cost C = 1 
  
  Gaussian Radial Basis kernel function. 
  Hyperparameter : sigma =  0.474194300372978 
  
  Number of Support Vectors : 133 
  
  Objective Function Value : -51.7903 
  Training error : 0.32686 
}

result_predict_train <-predict(idol_svm, data_training)
rmse(data_training$old, result_predict_train)
#2.428764

result_predict<-predict(idol_svm, data_predicting)
rmse(data_predicting$old, result_predict)
#2.871298

#3.489724

predict(idol_svm, unknown_old)

#14.92152　3人除いた
#15.31592　3人いれた


dev.off()

#09
model = rpart(type ~ ., data = data[,c(1:6,8)])
rpart.plot(model, extra = 4)

#install.packages("partykit")

library(partykit)

model = rpart(old ~ ., data = data[,c(1:6,8)])
plotcp(model) 
rpart.plot(model)

predict(model, unknown_old) 









set.seed(123)
rf_model <- randomForest(old ~ ., data = data_training,importance=T,proximity=T,ntree=100)

rf.fitted <- predict(rf_model,data_training)
rmse(rf.fitted,data_training$old)
#2.843818　3人入れてる
#2.693453　3人除いた

rf.fitted <- predict(rf_model,data_predicting)
rmse(rf.fitted,data_predicting$old)
#
#3.395286 3人除いた

print(rf_model)

if(0){
Call:
  randomForest(formula = old ~ ., data = data_training, importance = T,      proximity = T, ntree = 100) 
Type of random forest: regression
Number of trees: 100
No. of variables tried at each split: 2

Mean of squared residuals: 8.087302
% Var explained: 50.46
}



varImpPlot(rf_model, main = "RF_MODEL")

ds<-as.dist(1.0-rf_model$proximity)
hc<-hclust(ds,"ward.D")
plot(hc)


pred_rf <- predict(rf_model,data_training)
rmse(data_training$old,pred_rf)
#1.317822 3人のぞく

pred_rf <- predict(rf_model,data_predicting)
rmse(data_predicting$old,pred_rf)
#3.37454　3人含む
#3.395286 3人のぞく

predict(rf_model,unknown_old)
#15.59667 3人含める
#15.68567 3人のぞく






#特徴量をシンプルにしてランダムフォレストをかける

set.seed(123)
rf_model_2 <- randomForest(old ~ ., data = data_training[,c("weight","hight","bust","old")],importance=T,proximity=T)

rf.fitted2 <- predict(rf_model_2)
rmse(rf.fitted2,data_training$old)
#2.672817

print(rf_model_2)

if(0){
  Call:
    randomForest(formula = old ~ ., data = data_training[, c("weight", "hight", "bust", "old")], importance = T, proximity = T) 
  Type of random forest: regression
  Number of trees: 500
  No. of variables tried at each split: 1
  
  Mean of squared residuals: 7.143951
  % Var explained: 56.24
}

#varImpPlot(rf_model_2, main = "RF_MODEL_2")

ds<-as.dist(1.0-rf_model$proximity)
hc<-hclust(ds,"ward.D")
plot(hc)


pred_rf2 <- predict(rf_model_2,data_predicting)
rmse(data_predicting$old,pred_rf2)
#3.281235


predict(rf_model_2,unknown_old)
#15.42079




####################

bust_model<-lm(bust~old+hight,data=data2)
pred_bust<-predict(bust_model,unknown_3size)

waist_model<-lm(waist~old+hight,data=data2)
pred_waist<-predict(waist_model,unknown_3size)

hip_model<-lm(hip~old+hight,data=data2)
pred_hip<-predict(hip_model,unknown_3size)



unknown_3size$pred_bust<-pred_bust
unknown_3size$pred_waist<-pred_waist
unknown_3size$pred_hip<-pred_hip

write.csv(unknown_3size, "pred_3size.csv", fileEncoding = "CP932",row.names=F)


