library(readr)
library(mlr3verse)
library(ggplot2)
library(caret)
dat <- read_csv("Downloads/predictdeath2.csv")
dat<data.frame(dat)
dat<-na.omit(dat)
cont_covariates <- c("Age","High","Weight","BMI","CPBT","ACC","DHCAT","LNT","LPT","Reb","Plam","SymhosT","HossurgT","Sym.surgT","Root","LAD","LVEDD","LVESD","IVS","LVEF","ProxAo","Hb","WBC","Plt","N","cTnt","BNP","fibrinogen","D2","INR","Tbil","albumin","ALT","AST","urea","Cr","Na","K",data=dat)
cate_covariates <- c("Male","HBP","DM","CAD","CKD","COPD","Smoking","FamilyHistory","Heartsurgery","Stroke","AFhistory","anticoagulation","Warfarin","antiplatelet","aspirin","clopidogrel","IMH","PAU","MFS","BAV","TEVARR","IscCerebral","IscSpinal","IscCoronary","IscMesenteric","IscRenal","IscUEM","IscLEM","Hypotension","Ventilation","Shock","Tamponade","CPStatus","Emergency","ARR","Bentall","David","Wheat","FET","CABG","MVprocedures","TVprocedures","FLRoot","FLascending","FLarch","FLdescending","tearRoot","tearascending","teararch","teardescending","commissuredistachment","RNCD","LNCD","LRCD","Sinusinvolve","RCANeri","LCANeri","IA","LCCA","LSCA","ReCPB","ReACC","UACP","BACP","Transfusion","AI","hydropericardium","hosMortality","RereCPB","AS",data=dat)
dat<data.frame(dat)
dat<-na.omit(dat)
# 确保响应变量是因子类型
dat$hosMortality <- as.factor(dat$hosMortality)
# 设置 RFE 控制函数
control <- rfeControl(functions = rfFuncs,# 使用随机森林 
                      method = "cv",# 交叉验证
                      number = 10)# 交叉验证的重复次数
# 指定 RFE 过程中考虑的特征子集大小
sizes <- c(1, 2, 5, 10, 15, 20,25,30)
# 执行 RFE
set.seed(123)
rfe_results <- rfe(x = dat[, -ncol(dat)],
                   y = dat$hosMortality,
                   sizes = sizes,
                   rfeControl = control)
# 查看 RFE 结果
print(rfe_results)
# 查看 RFE 结果
plot(rfe_results, type = c("g", "o"))
selected_features <- predictors(rfe_results)
print(selected_features)
sdat<-subset(dat,select=c('CPBT','IscMesenteric','CABG','D2','INR','CPStatus','N','WBC','fibrinogen','ACC','Ventilation','Cr','Plam','BNP','IscLEM','hosMortality','RereCPB','IscCerebral','urea','Plt','IscCoronary','Tamponade','Reb','MFS','IVS','AST','albumin','Hypotension','Shock','IscRenal','cTnt'))
task<-as_task_classif(sdat, target="hosMortality")
learner = lrn("classif.rpart",
              predict_type = "prob")
task
final_score <- msrs(c("classif.auc",
                      "classif.ce",
                      "classif.acc",
                      "classif.precision",
                      "classif.recall",
                      "classif.sensitivity", 
                      "classif.specificity"))
learner$train(task)
print(learner$model)
forest<-learner$model
pred1 <- learner$predict(task )#测试集
print(pred1)
conf <- pred1$confusion
print(conf)
autoplot(pred1, type = "roc")
pred1$score(msr("classif.acc")) #准确度
pred1$score(msr("classif.auc"))#auc曲面下面积
pred1$score(final_score)
library(kernelshap)
dat$hosMortality=factor(dat$hosMortality)
s <- kernelshap(forest,X=sdat[,-c(16)],bg_X=sdat)#成功
s
library(shapviz)
# 构建可视化对象
shp <- shapviz(s) 
# 变量重要性
sv_importance(shp,kind = "bee") 
sv_importance(shp, kind = "beeswarm")
sv_importance(shp,fill = "#0085FF")
#D2+CPBT+IscMesenteric+Plt+INR+/Tamponade+Reb+albumin