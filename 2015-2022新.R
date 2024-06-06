library(readr)
library(precrec)
library(rms)
library(rmda)
library(pROC)
library(caret)
library(gtsummary)
da <- read.csv("Downloads/mortality prediction data/predictionF.csv")
da<data.frame(da)
da<-na.omit(da)
cont_covariates <- c("Age","High","Weight","BMI","CPBT","ACC","DHCAT","LNT","LPT","Reb","Plam","SymhosT","HossurgT","Sym.surgT","Root","LAD","LVEDD","LVESD","IVS","LVEF","ProxAo","Hb","WBC","Plt","N","cTnt","BNP","fibrinogen","D2","INR","Tbil","albumin","ALT","AST","urea","Cr","Na","K",data=da)
cate_covariates <- c("Male","HBP","DM","CAD","CKD","Smoking","FamilyHistory","Heartsurgery","Stroke","AFhistory","anticoagulation","Warfarin","antiplatelet","aspirin","IMH","PAU","MFS","BAV","TEVARR","IscCerebral","IscSpinal","IscCoronary","IscMesenteric","IscRenal","IscUEM","IscLEM","Hypotension","Ventilation","Shock","Tamponade","CPStatus","Emergency","ARR","Bentall","David","FET","CABG","FLRoot","FLascending","FLarch","FLdescending","tearRoot","tearascending","teararch","teardescending","commissuredistachment","RNCD","LNCD","LRCD","Sinusinvolve","RCANeri","LCANeri","IA","LCCA","LSCA","ReCPB","ReACC","UACP","BACP","Transfusion","AI","hydropericardium","hosMortality","RereCPB","AS",data=da)
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
#write.csv(traindata,file= "traindata108.csv",row.names = F) #导出traindata
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
#多个变量因子化,运行autoReg命令前，把分类变量因子化
fit1<-glm(hosMortality~MFS+IscCerebral+CPStatus+IscSpinal+IscCoronary+IscMesenteric+IscRenal+IscLEM+Hypotension+Ventilation+Tamponade+Shock+CPBT+ACC+Reb+Plam+IVS+WBC+Plt+N+cTnt+BNP+fibrinogen+D2+INR+albumin+AST+urea+Cr+IVS+FLRoot+CABG+CPBT+ACC+BACP+Transfusion+Reb+Plam+RereCPB+AS,
          family=binomial(link = logit),
          data = traindata)
#install.packages("autoReg")
library(autoReg)
autoReg(fit1,uni=TRUE,threshold=0.05)
autoReg(fit1)
result1<-autoReg(fit1, uni=TRUE) %>% myft()
result1
fit<-glm(hosMortality~IscCerebral+CPStatus+IscSpinal+IscCoronary+IscMesenteric+IscRenal+IscLEM+Hypotension+Ventilation+Tamponade+Shock+CPBT+Reb+IVS+WBC+Plt+N+D2+INR+albumin+FLRoot+CABG+CPBT+ACC+Transfusion+RereCPB,
         family=binomial(link = logit),
         data = traindata)
res<-tbl_regression(fit, exponentiate=T) #进行回归分析
res
fit3<-glm(hosMortality~CPStatus+IscMesenteric+IscLEM+CPBT+Reb+IVS+INR+FLRoot+CABG,
          family=binomial(link = logit),
          data = traindata)
res<-tbl_regression(fit3, exponentiate=T) #进行回归分析
res
#IscMesenteric+CPStatus+CPBT+Reb+IVS+CABG
#IscMesenteric+IscLEM+CPStatus+CPBT+Reb
fit.reduce<-glm(hosMortality~CPStatus+IscMesenteric+IscLEM+CPBT+Reb+IVS+CABG,data = traindata,family = binomial(link = logit))
summary(fit.reduce)

coef(fit.reduce)
exp(coef(fit.reduce))
exp(confint(fit.reduce))


traindata$predictlog<- predict(newdata=traindata,fit.reduce,"response")
#在验证人群计算预测值，命名为predict
testdata$predictlog<- predict(newdata=testdata,fit.reduce,"response")


#利用pROC绘制ROC要注意direction参数的设置，如果算出来auc<0.05,可以设置为controls>cases，
#较小的指标结果代表更加肯定的检验。
#在建模人群绘制ROC
trainroc <- roc(hosMortality~predictlog, data = traindata,smooth=F)
plot(trainroc, auc.polygon=T,auc.polygon.col="steelblue",
     print.auc=TRUE, print.auc.col="firebrick",
     print.thres=TRUE,main = "ROC curve in traindata",
     identity.lty=1,identity.lwd=1)
auc(trainroc);ci(trainroc)

#在验证人群中绘制ROC
testroc <- roc(hosMortality~predictlog, data = testdata,smooth=F)
plot(testroc, auc.polygon=T,auc.polygon.col="steelblue",
     print.auc=TRUE, print.auc.col="firebrick",
     print.thres=TRUE,main = "ROC curve in testdata",
     identity.lty=1,identity.lwd=1)
auc(testroc);ci(testroc)
#校准曲线

da$predictlog<- predict(newdata=da,fit.reduce,"response")
#在建模人群中绘制,p<0.05说明校准度不好(注：如果一开始y变量因子化，需要在此处数字化)
val.prob(da$predictlog,da$hosMortality)
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


fitdtree<-glm(hosMortality~CPStsatus+N+BNP+fibrinogen+Ventilation+IscMesenteric+IscLEM+FLRoot,family=binomial(link = logit),
              data = traindata)
summary(fitdtree)
traindata$predictdtree<- predict(newdata=traindata,fitdtree,"response")
#在验证人群计算预测值，命名为predict
testdata$predictdtree<- predict(newdata=testdata,fitdtree,"response")

#利用pROC绘制ROC要注意direction参数的设置，如果算出来auc<0.05,可以设置为controls>cases，
#较小的指标结果代表更加肯定的检验。
#在建模人群绘制ROC
trainrocdtree <- roc(hosMortality~predictdtree, data = traindata,smooth=F)
plot(trainrocdtree, auc.polygon=T,auc.polygon.col="steelblue",
     print.auc=TRUE, print.auc.col="firebrick",
     print.thres=TRUE,main = "ROC curve in traindata",
     identity.lty=1,identity.lwd=1)
auc(trainrocdtree);ci(trainrocdtree)

#在验证人群中绘制ROC
testrocdtree <- roc(hosMortality~predictdtree, data = testdata,smooth=F)
plot(testrocdtree, auc.polygon=T,auc.polygon.col="steelblue",
     print.auc=TRUE, print.auc.col="firebrick",
     print.thres=TRUE,main = "ROC curve in testdata",
     identity.lty=1,identity.lwd=1)
auc(testrocdtree);ci(testrocdtree)
#校准曲线
da$predictdtree<- predict(newdata=da,fitdtree,"response")
#在建模人群中绘制,p<0.05说明校准度不好(注：如果一开始y变量因子化，需要在此处数字化)
val.prob(da$predictdtree,as.numeric(da$hosMortality))
library(rms)
CstatisticCI <- function(x) {
  se <- x["S.D."]/sqrt(x["n"])
  Low95 <- x["C Index"] - 1.96*se 
  Upper95 <- x["C Index"] + 1.96*se 
  cbind(x["C Index"], Low95, Upper95) 
}
cindex <- rcorr.cens(da$predictdtree, da$hosMortality)
cindex
print(CstatisticCI(cindex))
#svm:CPBT+CPStatus+Plam+N+commissuredistachment+XclampT+D2+IscCerebral+CABG+IscMesenteric+urea
fitsvm<-glm(hosMortality~CPBT+AST+Plam+CPStatus+ACC+D2+IVS+FLRoot,#+IscLEM+Reb+WBC+IA+CABG,
            family=binomial(link = logit),
            data = traindata)
res<-tbl_regression(fitsvm, exponentiate=T) #进行回归分析
res
#fitsvm<-glm(hosMortality~CPBT+CPStatus+commissuredistachment+CABG+IscMesenteric,
#          family=binomial(link = logit),
#          data = traindata)
summary(fitsvm)
traindata$predictsvm<- predict(newdata=traindata,fitsvm,"response")
#在验证人群计算预测值，命名为predict
testdata$predictsvm<- predict(newdata=testdata,fitsvm,"response")

#利用pROC绘制ROC要注意direction参数的设置，如果算出来auc<0.05,可以设置为controls>cases，
#较小的指标结果代表更加肯定的检验。
#在建模人群绘制ROC
trainrocsvm <- roc(hosMortality~predictsvm, data = traindata,smooth=F)
plot(trainrocsvm, auc.polygon=T,auc.polygon.col="steelblue",
     print.auc=TRUE, print.auc.col="firebrick",
     print.thres=TRUE,main = "ROC curve in traindata",
     identity.lty=1,identity.lwd=1)
auc(trainrocsvm);ci(trainrocsvm)

#在验证人群中绘制ROC
testrocsvm <- roc(hosMortality~predictsvm, data = testdata,smooth=F)
plot(testrocsvm, auc.polygon=T,auc.polygon.col="steelblue",
     print.auc=TRUE, print.auc.col="firebrick",
     print.thres=TRUE,main = "ROC curve in testdata",
     identity.lty=1,identity.lwd=1)
auc(testrocsvm);ci(testrocsvm)

#校准曲线
da$predictsvm<- predict(newdata=da,fitsvm,"response")
#在建模人群中绘制,p<0.05说明校准度不好(注：如果一开始y变量因子化，需要在此处数字化)
val.prob(da$predictsvm,as.numeric(da$hosMortality))
library(rms)
CstatisticCI <- function(x) {
  se <- x["S.D."]/sqrt(x["n"])
  Low95 <- x["C Index"] - 1.96*se 
  Upper95 <- x["C Index"] + 1.96*se 
  cbind(x["C Index"], Low95, Upper95) 
}
cindex <- rcorr.cens(da$predictsvm, da$hosMortality)
cindex
print(CstatisticCI(cindex))

#xgboost:(D2\Plam\CPBT\INR\WBC\Cr\albumin)\CPStstus\Reb\AFhistory\LSCA\IscCerebral\AS\LCANeri\urea
fitxgboost<-glm(hosMortality~CPStatus+N+CPBT+Plam+BNP+D2+BACP+Hypotension,
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
plot(trainrocxgboost, auc.polygon=T,auc.polygon.col="steelblue",
     print.auc=TRUE, print.auc.col="firebrick",
     print.thres=TRUE,main = "ROC curve in traindata",
     identity.lty=1,identity.lwd=1)
auc(trainrocxgboost);ci(trainrocxgboost)

#在验证人群中绘制ROC
testrocxgboost <- roc(hosMortality~predictxgboost, data = testdata,smooth=F)
plot(testrocxgboost, auc.polygon=T,auc.polygon.col="steelblue",
     print.auc=TRUE, print.auc.col="firebrick",
     print.thres=TRUE,main = "ROC curve in testdata",
     identity.lty=1,identity.lwd=1)
auc(testrocxgboost);ci(testrocxgboost)
#校准曲线
da$predictxgboost<- predict(newdata=da,fitxgboost,"response")
#在建模人群中绘制,p<0.05说明校准度不好(注：如果一开始y变量因子化，需要在此处数字化)
val.prob(da$predictxgboost,as.numeric(da$hosMortality))
library(rms)
CstatisticCI <- function(x) {
  se <- x["S.D."]/sqrt(x["n"])
  Low95 <- x["C Index"] - 1.96*se 
  Upper95 <- x["C Index"] + 1.96*se 
  cbind(x["C Index"], Low95, Upper95) 
}
cindex <- rcorr.cens(da$predictxgboost, da$hosMortality)
cindex
print(CstatisticCI(cindex))
#forest:CPBT+INR+D2+LA+WBC+Reb+fibrinogen+urea+BNP+N+Cr+Plam+XclampT+AST+LAC+IVS+cTnt+albumin+K
fitforest<-glm(hosMortality~CPBT+AST+fibrinogen+Plam+D2+N+BNP+INR,#+Cr+Plam+XclampT+AST+LAC+IVS+cTnt+albumin+K+CPStatus+IscMesenteric+CABG+IscSpinal+Sinusinvolve+Ventilation+IscCerebral+hydropericardium+IscLEM+LSCA,
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
plot(trainrocforest, auc.polygon=T,auc.polygon.col="steelblue",
     print.auc=TRUE, print.auc.col="firebrick",
     print.thres=TRUE,main = "ROC curve in traindata",
     identity.lty=1,identity.lwd=1)
auc(trainrocforest);ci(trainrocforest)

#在验证人群中绘制ROC
testrocforest <- roc(hosMortality~predictforest, data = testdata,smooth=F)
plot(testrocforest, auc.polygon=T,auc.polygon.col="steelblue",
     print.auc=TRUE, print.auc.col="firebrick",
     print.thres=TRUE,main = "ROC curve in testdata",
     identity.lty=1,identity.lwd=1)
auc(testrocforest);ci(testrocforest)


#校准曲线
da$predictforest<- predict(newdata=da,fitforest,"response")
#在建模人群中绘制,p<0.05说明校准度不好(注：如果一开始y变量因子化，需要在此处数字化)
val.prob(da$predictforest,as.numeric(da$hosMortality))
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
    data = da) %>%
  plot(smooth = TRUE)

library(rms)
library(DynNom)
library(regplot)
regplot(fitsvm,
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
z.std = as.matrix(subset(da, select = c(CPStatus,IscMesenteric,IscLEM,CPBT,Reb,IVS,CABG)))#旧模型
z.new = as.matrix(subset(da, select = c(CPBT,AST,Plam,CPStatus,ACC,D2,IVS,FLRoot)))#新模型

# 建立2个模型
mstd = glm(hosMortality~CPStatus+IscMesenteric+IscLEM+CPBT+Reb+IVS+CABG, family = binomial(), data = da, x=TRUE)
mnew = glm(hosMortality~CPBT+AST+Plam+CPStatus+ACC+D2+IVS+FLRoot, family = binomial(), data = da, x=TRUE)

# 取出模型预测概率
p.std = mstd$fitted.values
p.new = mnew$fitted.values
## 结果变量 + 两个模型得到的预测概率
nribin(event = da$hosMortality, p.std = p.std, p.new = p.new, 
       cut = c(0.10,0.20), 
       niter = 500, 
       updown = 'category')
#install.packages("XQuartz") #安装R包
library(PredictABEL)  

da$event <- da$hosMortality

reclassification(data = da,
                 cOutcome = 97, # 结果变量在哪一列
                 predrisk1 = p.std,
                 predrisk2 = p.new,
                 cutoff = c(0,0.10,0.20,1)
)

