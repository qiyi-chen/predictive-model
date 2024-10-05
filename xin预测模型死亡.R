library(readr)
library(precrec)
library(rms)
library(rmda)
library(pROC)
library(caret)
library(gtsummary)
da <- read.csv("Downloads/predictdeath1.csv")
da<data.frame(da)
da<-na.omit(da)
cont_covariates <- c("Age","High","Weight","BMI","CPBT","ACC","DHCAT","LNT","LPT","Reb","Plam","SymhosT","HossurgT","Sym.surgT","Root","LAD","LVEDD","LVESD","IVS","LVEF","ProxAo","Hb","WBC","Plt","N","cTnt","BNP","fibrinogen","D2","INR","Tbil","albumin","ALT","AST","urea","Cr","Na","K",data=da)
cate_covariates <- c("Male","HBP","DM","CAD","CKD","Smoking","FamilyHistory","Heartsurgery","Stroke","AFhistory","anticoagulation","Warfarin","antiplatelet","aspirin","IMH","PAU","MFS","BAV","TEVARR","IscCerebral","IscSpinal","IscCoronary","IscMesenteric","IscRenal","IscUEM","IscLEM","Hypotension","Ventilation","Shock","Tamponade","CPStatus","Emergency","ARR","Bentall","David","FET","CABG","FLRoot","FLascending","FLarch","FLdescending","tearRoot","tearascending","teararch","teardescending","commissuredistachment","RNCD","LNCD","LRCD","Sinusinvolve","RCANeri","LCANeri","IA","LCCA","LSCA","ReCPB","ReACC","UACP","BACP","Transfusion","AI","hydropericardium","hosMortality","RereCPB","AS","90Mortality",data=da)
da$Male=factor(da$Male)
da$HBP=factor(da$HBP)
da$DM=factor(da$DM)
da$CAD=factor(da$CAD)
da$CKD=factor(da$CKD)
da$Smoking=factor(da$Smoking)
da$FamilyHistory=factor(da$FamilyHistory)
da$Heartsurgery=factor(da$Heartsurgery)
da$Stroke=factor(da$Stroke)
da$AFhistory=factor(da$AFhistory)
da$anticoagulation=factor(da$anticoagulation)
da$Warfarin=factor(da$Warfarin)
da$antiplatelet=factor(da$antiplatelet)
da$aspirin=factor(da$aspirin)
da$IMH=factor(da$IMH)
da$PAU=factor(da$PAU)
da$MFS=factor(da$MFS)
da$BAV=factor(da$BAV)
da$TEVARR=factor(da$TEVARR)
da$IscCerebral=factor(da$IscCerebral)
da$IscSpinal=factor(da$IscSpinal)
da$IscCoronary=factor(da$IscCoronary)
da$IscMesenteric=factor(da$IscMesenteric)
da$IscRenal=factor(da$IscRenal)
da$IscUEM=factor(da$IscUEM)
da$IscLEM=factor(da$IscLEM)
da$Hypotension=factor(da$Hypotension)
da$Ventilation=factor(da$Ventilation)
da$Tamponade=factor(da$Tamponade)
da$CPStatus=factor(da$CPStatus)
da$Emergency=factor(da$Emergency)
da$ARR=factor(da$ARR)
da$Bentall=factor(da$Bentall)
da$David=factor(da$David)
da$FET=factor(da$FET)
da$CABG=factor(da$CABG)
da$FLRoot=factor(da$FLRoot)
da$FLascending=factor(da$FLascending)
da$FLarch=factor(da$FLarch)
da$FLdescending=factor(da$FLdescending)
da$tearRoot=factor(da$tearRoot)
da$tearascending=factor(da$tearascending)
da$teararch=factor(da$teararch)
da$teardescending=factor(da$teardescending)
da$commissuredistachment=factor(da$commissuredistachment)
da$Sinusinvolve=factor(da$Sinusinvolve)
da$RCANeri=factor(da$RCANeri)
da$LCANeri=factor(da$LCANeri)
da$IA=factor(da$IA)
da$LCCA=factor(da$LCCA)
da$LSCA=factor(da$LSCA)
da$ReCPB=factor(da$ReCPB)
da$ReACC=factor(da$ReACC)
da$UACP=factor(da$UACP)
da$ReACC=factor(da$ReACC)
da$UACP=factor(da$UACP)
da$BACP=factor(da$BACP)
da$Transfusion=factor(da$Transfusion)
da$AI=factor(da$AI)
da$hydropericardium=factor(da$hydropericardium)
da$Shock=factor(da$Shock)
da$RereCPB=factor(da$RereCPB)
da$AS=factor(da$AS)
set.seed(109)
trainid<-createDataPartition(y=da$hosMortality,p=0.70,list=F)
traindata<-da[trainid, ]
#write.csv(traindata,file= "predictdeath1.csv",row.names = F) #导出traindata
testdata<-da[-trainid,]
#单、多因素L
library(scitb)
allVars<-c("Age","High","Weight","BMI","CPBT","ACC","DHCAT","LNT","LPT","Reb","Plam","SymhosT","HossurgT","Sym.surgT","Root","LAD","LVEDD","LVESD","IVS","LVEF","ProxAo","Hb","WBC","Plt","N","cTnt","BNP","fibrinogen","D2","INR","Tbil","albumin","ALT","AST","urea","Cr","Na","K","Male","HBP","DM","CAD","CKD","Smoking","FamilyHistory","Heartsurgery","Stroke","AFhistory","anticoagulation","Warfarin","antiplatelet","aspirin","IMH","PAU","MFS","BAV","TEVARR","IscCerebral","IscSpinal","IscCoronary","IscMesenteric","IscRenal","IscUEM","IscLEM","Hypotension","Ventilation","Tamponade","CPStatus","Emergency","ARR","Bentall","David","FET","CABG","FLRoot","FLascending","FLarch","FLdescending","tearRoot","tearascending","teararch","teardescending","commissuredistachment","Sinusinvolve","RCANeri","LCANeri","IA","LCCA","LSCA","ReCPB","ReACC","UACP","BACP","Transfusion","AI","hydropericardium","hosMortality","Shock","RereCPB","AS")
fvars<-c("Male","HBP","DM","CAD","CKD","Smoking","FamilyHistory","Heartsurgery","Stroke","AFhistory","anticoagulation","Warfarin","antiplatelet","aspirin","IMH","PAU","MFS","BAV","TEVARR","IscCerebral","IscSpinal","IscCoronary","IscMesenteric","IscRenal","IscUEM","IscLEM","Hypotension","Ventilation","Tamponade","CPStatus","Emergency","ARR","Bentall","David","FET","CABG","FLRoot","FLascending","FLarch","FLdescending","tearRoot","tearascending","teararch","teardescending","commissuredistachment","Sinusinvolve","RCANeri","LCANeri","IA","LCCA","LSCA","ReCPB","ReACC","UACP","BACP","Transfusion","AI","hydropericardium","hosMortality","Shock","RereCPB","AS")
strata<-c("hosMortality")
table1<-scitb1(vars=allVars,fvars=fvars,strata=strata,data=da) #自动检验正态性
table1<-scitb1(vars=allVars,fvars=fvars,strata=strata,data=da, 
               nonnormal=c("meal.cal"),atotest=F)  #指定非正态
table1
#write.csv(table1,file= "PpF1.csv",row.names = F) #导出table1
library(autoReg)
library(dplyr)
table2=gaze(hosMortality~.,data=da) %>% myft()
table2
table3=gaze(hosMortality~.,data=da,method=3) %>% myft()
table3
#traindata$hosMortality=factor(traindata$hosMortality)
#多个变量因子化,运行autoReg命令前，把分类变量因子化
fit1<-glm(hosMortality~MFS+IscCerebral+IscSpinal+IscCoronary+IscMesenteric+IscRenal+IscLEM+CPStatus+Hypotension+Ventilation+Tamponade+Shock+CABG+FLRoot+CPBT+ACC+BACP+Transfusion+Reb+Plam+IVS+WBC+Plt+N+cTnt+BNP+fibrinogen+D2+INR+albumin+AST+urea+Cr+RereCPB+AS,
          family=binomial(link = logit),
          data = traindata)
#install.packages("autoReg")
library(autoReg)
autoReg(fit1,uni=TRUE,threshold=0.05)
autoReg(fit1)
result1<-autoReg(fit1, uni=TRUE) %>% myft()
result1
fit<-glm(hosMortality~IscCerebral+IscSpinal+IscCoronary+IscMesenteric+IscRenal+IscLEM+CPStatus+Hypotension+Ventilation+Tamponade+Shock+CABG+FLRoot+CPBT+Transfusion+Reb+IVS+WBC+Plt+N+D2+INR+RereCPB,
         family=binomial(link = logit),
         data = traindata)
res<-tbl_regression(fit, exponentiate=T) #进行回归分析
res

fit.reduce<-glm(hosMortality~IscCerebral+IscMesenteric+CPStatus+CPBT+D2+INR+CABG,data = traindata,family = binomial(link = logit))
summary(fit.reduce)

coef(fit.reduce)
exp(coef(fit.reduce))
exp(confint(fit.reduce))
# 在外部验证集上绘制ROC曲线roc_external <- roc(external_data$response, external_predictions)auc_external <- auc(roc_external)plot.roc(roc_external, main = paste("External Set ROC - AUC =", round(auc_external, 2)))
traindata$predictlog<- predict(newdata=traindata,fit.reduce,"response")
#在验证人群计算预测值，命名为predict
testdata$predictlog<- predict(newdata=testdata,fit.reduce,"response")


#利用pROC绘制ROC要注意direction参数的设置，如果算出来auc<0.05,可以设置为controls>cases，
#较小的指标结果代表更加肯定的检验。
#在建模人群绘制ROC
trainroc <- roc(hosMortality~predictlog, data = traindata,smooth=F)
auc(trainroc);ci(trainroc)

#在验证人群中绘制ROC
testroc <- roc(hosMortality~predictlog, data = testdata,smooth=F)
auc(testroc);ci(testroc)

plot(trainroc,      
     print.auc=TRUE, # 图像上输出AUC的值     
     print.auc.x=0.2, 
     print.auc.y=0.5, # 设置AUC值坐标为（x，y）     
     #auc.polygon=TRUE, # 将ROC曲线下面积转化为多边形     
     #auc.polygon.col="#C87A8A",  # 设置ROC曲线下填充色     
     grid=c(0.5, 0.2), # 设置两轴网格线的间隔为0.5，0.2     
     grid.col=c("black", "black"),  # 设置两轴间隔线条的颜色     
     print.thres=T, # 图像上输出最佳截断值     
     main="ROC curves of Fit.LR model",  # 添加图形标题     
     col="navy",    # 设置ROC曲线颜色     
     legacy.axes=TRUE)
plot.roc(testroc,         
         add=TRUE, # 增加曲线         
         col="#B9181A", # 设置ROC曲线颜色         
         print.thres=0.130, # 图像上输出最佳截断值         
         print.auc=TRUE,   # 图像上输出AUC         
         print.auc.x=0.2,
         print.auc.y=0.45) # AUC的坐标为（x，y）
legend(0.6, 0.4,  # 图例位置x，y      
       bty = "n",   # 图例样式      
       legend=c("Training dataset","Testing dataset"), # 添加图例分组       
       col=c("navy","#B9181A"),  # 颜色跟前面一致       
       lwd=1.5)
#校准曲线
#在建模人群中绘制,p<0.05说明校准度不好(注：如果一开始y变量因子化，需要在此处数字化)
val.prob(traindata$predictlog,as.numeric(traindata$hosMortality))
library(rms)
CstatisticCI <- function(x) {
  se <- x["S.D."]/sqrt(x["n"])
  Low95 <- x["C Index"] - 1.96*se 
  Upper95 <- x["C Index"] + 1.96*se 
  cbind(x["C Index"], Low95, Upper95) 
}
cindex <- rcorr.cens(da$predictlog, da$hosMortality)
cindex
print(CstatisticCI(cindex))


fitdtree<-glm(hosMortality~CPBT+Plt+IscMesenteric+ACC+MFS+cTnt+AS+IscLEM,family=binomial(link = logit),
              data = traindata)
summary(fitdtree)
traindata$predictdtree<- predict(newdata=traindata,fitdtree,"response")
#在验证人群计算预测值，命名为predict
testdata$predictdtree<- predict(newdata=testdata,fitdtree,"response")

#利用pROC绘制ROC要注意direction参数的设置，如果算出来auc<0.05,可以设置为controls>cases，
#较小的指标结果代表更加肯定的检验。
#在建模人群绘制ROC
trainrocdtree <- roc(hosMortality~predictdtree, data = traindata,smooth=F)
auc(trainrocdtree);ci(trainrocdtree)

#在验证人群中绘制ROC
testrocdtree <- roc(hosMortality~predictdtree, data = testdata,smooth=F)
auc(testrocdtree);ci(testrocdtree)

plot(trainrocdtree,      
     print.auc=TRUE, # 图像上输出AUC的值     
     print.auc.x=0.2, 
     print.auc.y=0.5, # 设置AUC值坐标为（x，y）     
     #auc.polygon=TRUE, # 将ROC曲线下面积转化为多边形     
     #auc.polygon.col="#C87A8A",  # 设置ROC曲线下填充色     
     grid=c(0.5, 0.2), # 设置两轴网格线的间隔为0.5，0.2     
     grid.col=c("black", "black"),  # 设置两轴间隔线条的颜色     
     print.thres=T, # 图像上输出最佳截断值     
     main="ROC curves of Fit.Dtree model",  # 添加图形标题     
     col="navy",    # 设置ROC曲线颜色     
     legacy.axes=TRUE)
plot.roc(testrocdtree,         
         add=TRUE, # 增加曲线         
         col="#B9181A", # 设置ROC曲线颜色         
         print.thres=0.072, # 图像上输出最佳截断值         
         print.auc=TRUE,   # 图像上输出AUC         
         print.auc.x=0.2,
         print.auc.y=0.45) # AUC的坐标为（x，y）
legend(0.6, 0.4,  # 图例位置x，y      
       bty = "n",   # 图例样式      
       legend=c("Training dataset","Testing dataset"), # 添加图例分组       
       col=c("navy","#B9181A"),  # 颜色跟前面一致       
       lwd=1.5)
#校准曲线
#在建模人群中绘制,p<0.05说明校准度不好(注：如果一开始y变量因子化，需要在此处数字化)
val.prob(traindata$predictdtree,as.numeric(traindata$hosMortality))

#svm:
fitsvm<-glm(hosMortality~CPBT+IscMesenteric+CPStatus+MFS+D2+Plt+IscCerebral+CABG,
            family=binomial(link = logit),
            data = traindata)
res<-tbl_regression(fitsvm, exponentiate=T) #进行回归分析
res
summary(fitsvm)
traindata$predictsvm<- predict(newdata=traindata,fitsvm,"response")
#在验证人群计算预测值，命名为predict
testdata$predictsvm<- predict(newdata=testdata,fitsvm,"response")

#利用pROC绘制ROC要注意direction参数的设置，如果算出来auc<0.05,可以设置为controls>cases，
#较小的指标结果代表更加肯定的检验。
#在建模人群绘制ROC
trainrocsvm <- roc(hosMortality~predictsvm, data = traindata,smooth=F)
auc(trainrocsvm);ci(trainrocsvm)

#在验证人群中绘制ROC
testrocsvm <- roc(hosMortality~predictsvm, data = testdata,smooth=F)
auc(testrocsvm);ci(testrocsvm)

plot(trainrocsvm,      
     print.auc=TRUE, # 图像上输出AUC的值     
     print.auc.x=0.2, 
     print.auc.y=0.5, # 设置AUC值坐标为（x，y）     
     #auc.polygon=TRUE, # 将ROC曲线下面积转化为多边形     
     #auc.polygon.col="#C87A8A",  # 设置ROC曲线下填充色     
     grid=c(0.5, 0.2), # 设置两轴网格线的间隔为0.5，0.2     
     grid.col=c("black", "black"),  # 设置两轴间隔线条的颜色     
     print.thres=T, # 图像上输出最佳截断值     
     main="ROC curves of Fit.SVM model",  # 添加图形标题     
     col="navy",    # 设置ROC曲线颜色     
     legacy.axes=TRUE)
plot.roc(testrocsvm,         
         add=TRUE, # 增加曲线         
         col="#B9181A", # 设置ROC曲线颜色         
         print.thres=0.179, # 图像上输出最佳截断值         
         print.auc=TRUE,   # 图像上输出AUC         
         print.auc.x=0.2,
         print.auc.y=0.45) # AUC的坐标为（x，y）
legend(0.6, 0.4,  # 图例位置x，y      
       bty = "n",   # 图例样式      
       legend=c("Training dataset","Testing dataset"), # 添加图例分组       
       col=c("navy","#B9181A"),  # 颜色跟前面一致       
       lwd=1.5)

#校准曲线
#在建模人群中绘制,p<0.05说明校准度不好(注：如果一开始y变量因子化，需要在此处数字化)
val.prob(traindata$predictsvm,as.numeric(traindata$hosMortality))

#xgboost
fitxgboost<-glm(hosMortality~CPBT+IscMesenteric+MFS+Shock+IscCerebral+CABG+RereCPB+Hypotension,
                family=binomial(link = logit),
                data = traindata)
res<-tbl_regression(fitxgboost, exponentiate=T) #进行回归分析
res
traindata$predictxgboost<- predict(newdata=traindata,fitxgboost,"response")
#在验证人群计算预测值，命名为predict
testdata$predictxgboost<- predict(newdata=testdata,fitxgboost,"response")

#利用pROC绘制ROC要注意direction参数的设置，如果算出来auc<0.05,可以设置为controls>cases，
#较小的指标结果代表更加肯定的检验。
#在建模人群绘制ROC
trainrocxgboost <- roc(hosMortality~predictxgboost, data = traindata,smooth=F)
auc(trainrocxgboost);ci(trainrocxgboost)

#在验证人群中绘制ROC
testrocxgboost <- roc(hosMortality~predictxgboost, data = testdata,smooth=F)
auc(testrocxgboost);ci(testrocxgboost)

plot(trainrocxgboost,      
     print.auc=TRUE, # 图像上输出AUC的值     
     print.auc.x=0.2, 
     print.auc.y=0.5, # 设置AUC值坐标为（x，y）     
     #auc.polygon=TRUE, # 将ROC曲线下面积转化为多边形     
     #auc.polygon.col="#C87A8A",  # 设置ROC曲线下填充色     
     grid=c(0.5, 0.2), # 设置两轴网格线的间隔为0.5，0.2     
     grid.col=c("black", "black"),  # 设置两轴间隔线条的颜色     
     print.thres=T, # 图像上输出最佳截断值     
     main="ROC curves of Fit.XGBoost model",  # 添加图形标题     
     col="navy",    # 设置ROC曲线颜色     
     legacy.axes=TRUE)
plot.roc(testrocxgboost,         
         add=TRUE, # 增加曲线         
         col="#B9181A", # 设置ROC曲线颜色         
         print.thres=0.084, # 图像上输出最佳截断值         
         print.auc=TRUE,   # 图像上输出AUC         
         print.auc.x=0.2,
         print.auc.y=0.45) # AUC的坐标为（x，y）
legend(0.6, 0.4,  # 图例位置x，y      
       bty = "n",   # 图例样式      
       legend=c("Training dataset","Testing dataset"), # 添加图例分组       
       col=c("navy","#B9181A"),  # 颜色跟前面一致       
       lwd=1.5)
#校准曲线
#在建模人群中绘制,p<0.05说明校准度不好(注：如果一开始y变量因子化，需要在此处数字化)
val.prob(traindata$predictxgboost,as.numeric(traindata$hosMortality))

#forest
fitforest<-glm(hosMortality~D2+CPBT+IscMesenteric+Plt+INR+Tamponade+Reb+albumin,
               family=binomial(link = logit),
               data = traindata)
res<-tbl_regression(fitforest, exponentiate=T) #进行回归分析
res

traindata$predictforest<- predict(newdata=traindata,fitforest,"response")
#在验证人群计算预测值，命名为predict
testdata$predictforest<- predict(newdata=testdata,fitforest,"response")

#利用pROC绘制ROC要注意direction参数的设置，如果算出来auc<0.05,可以设置为controls>cases，
#较小的指标结果代表更加肯定的检验。
#在建模人群绘制ROC
trainrocforest <- roc(hosMortality~predictforest, data = traindata,smooth=F)
auc(trainrocforest);ci(trainrocforest)

#在验证人群中绘制ROC
testrocforest <- roc(hosMortality~predictforest, data = testdata,smooth=F)
auc(testrocforest);ci(testrocforest)

plot(trainrocforest,      
     print.auc=TRUE, # 图像上输出AUC的值     
     print.auc.x=0.2, 
     print.auc.y=0.5, # 设置AUC值坐标为（x，y）     
     #auc.polygon=TRUE, # 将ROC曲线下面积转化为多边形     
     #auc.polygon.col="#C87A8A",  # 设置ROC曲线下填充色     
     grid=c(0.5, 0.2), # 设置两轴网格线的间隔为0.5，0.2     
     grid.col=c("black", "black"),  # 设置两轴间隔线条的颜色     
     print.thres=T, # 图像上输出最佳截断值     
     main="ROC curves of Fit.RF model",  # 添加图形标题     
     col="navy",    # 设置ROC曲线颜色     
     legacy.axes=TRUE)
plot.roc(testrocforest,         
         add=TRUE, # 增加曲线         
         col="#B9181A", # 设置ROC曲线颜色         
         print.thres=0.153, # 图像上输出最佳截断值         
         print.auc=TRUE,   # 图像上输出AUC         
         print.auc.x=0.2,
         print.auc.y=0.45) # AUC的坐标为（x，y）
legend(0.6, 0.4,  # 图例位置x，y      
       bty = "n",   # 图例样式      
       legend=c("Training dataset","Testing dataset"), # 添加图例分组       
       col=c("navy","#B9181A"),  # 颜色跟前面一致       
       lwd=1.5)

#校准曲线
#在建模人群中绘制,p<0.05说明校准度不好(注：如果一开始y变量因子化，需要在此处数字化)
val.prob(traindata$predictforest,as.numeric(traindata$hosMortality))
library(rms)
CstatisticCI <- function(x) {
  se <- x["S.D."]/sqrt(x["n"])
  Low95 <- x["C Index"] - 1.96*se 
  Upper95 <- x["C Index"] + 1.96*se 
  cbind(x["C Index"], Low95, Upper95) 
}
cindex <- rcorr.cens(da$predictforest, da$hosMortality)
cindex
print(CstatisticCI(cindex))

#临床决策曲线DCA
library(rms)
#install.packages("rmda")

library(dcurves)
dca(hosMortality ~ predictlog+predictforest+predictdtree+predictxgboost+predictsvm,
    data = traindata) %>%
  plot(smooth = TRUE)

library(rms)
library(DynNom)
library(regplot)
regplot(fitxgboost,
        plots = c("violin", "boxes"), ##连续性变量形状，可选"no plot" "density" "boxes" "ecdf" "bars" "boxplot" "violin" "bean" "spikes"；分类变量的形状，可选"no plot" "boxes" "bars" "spikes"
        observation = F,#da[1,], #用哪行观测，或者T F
        center = T, # 对齐变量
        subticks = T,
        droplines = T,#是否画竖线
        title = "nomogram",
        points = T, # 截距项显示为0-100
        odds = F, # 是否显示OR值
        showP = T, # 是否显示变量的显著性标记
        rank = "sd", # 根据sd给变量排序
        interval="confidence", # 展示可信区间
        clickable = F # 是否可以交互
)
#网页计算器
library(DynNom)
library(magrittr)
DNbuilder(fitsvm,da)
#NRI计算
#install.packages("nricens") # 安装R包
library(nricens)
library(survival)
# 定义结局事件，0是存活，1是死亡
event = ifelse(da$hosMortality,0,1)

# 两个只由预测变量组成的矩阵
z.std = as.matrix(subset(traindata, select = c(CPBT,IscMesenteric,CPStatus,MFS,D2,Plt,IscCerebral,CABG)))#旧模型
z.new = as.matrix(subset(traindata, select = c(D2,CPBT,IscMesenteric,Plt,INR,Tamponade,Reb,albumin)))#新模型

# 建立2个模型
mstd = glm(hosMortality~CPBT+IscMesenteric+CPStatus+MFS+D2+Plt+IscCerebral+CABG, family = binomial(), data = traindata, x=TRUE)
mnew = glm(hosMortality~D2+CPBT+IscMesenteric+Plt+INR+Tamponade+Reb+albumin, family = binomial(), data = traindata, x=TRUE)

# 取出模型预测概率
p.std = mstd$fitted.values
p.new = mnew$fitted.values
## 结果变量 + 两个模型得到的预测概率
nribin(event = traindata$hosMortality, p.std = p.std, p.new = p.new, 
       cut = c(0.10,0.20), 
       niter = 500, 
       updown = 'category')
#install.packages("XQuartz") #安装R包
library(PredictABEL)  

triandata$event <- traindata$hosMortality

reclassification(data = traindata,
                 cOutcome = 100, # 结果变量在哪一列
                 predrisk1 = p.std,
                 predrisk2 = p.new,
                 cutoff = c(0,0.10,0.20,1)
)
# 定义结局事件，0是存活，1是死亡
event = ifelse(da$hosMortality,0,1)

# 两个只由预测变量组成的矩阵
z.std = as.matrix(subset(traindata, select = c(CPBT,IscMesenteric,CPStatus,MFS,D2,Plt,IscCerebral,CABG)))#旧模型
z.new = as.matrix(subset(traindata, select = c(IscCerebral,IscMesenteric,CPStatus,CPBT,D2,INR,CABG)))#新模型

# 建立2个模型
mstd = glm(hosMortality~CPBT+IscMesenteric+CPStatus+MFS+D2+Plt+IscCerebral+CABG, family = binomial(), data = traindata, x=TRUE)
mnew = glm(hosMortality~IscCerebral+IscMesenteric+CPStatus+CPBT+D2+INR+CABG, family = binomial(), data = traindata, x=TRUE)

# 取出模型预测概率
p.std = mstd$fitted.values
p.new = mnew$fitted.values
## 结果变量 + 两个模型得到的预测概率
nribin(event = traindata$hosMortality, p.std = p.std, p.new = p.new, 
       cut = c(0.10,0.20), 
       niter = 500, 
       updown = 'category')
#install.packages("XQuartz") #安装R包
library(PredictABEL)  

traindata$event <- traindata$hosMortality

reclassification(data = traindata,
                 cOutcome = 100, # 结果变量在哪一列
                 predrisk1 = p.std,
                 predrisk2 = p.new,
                 cutoff = c(0,0.10,0.20,1)
)
