library(readr)
library(mlr3verse)
library(ggplot2)
dat <- read.csv("traindata0.5.csv")
dat<data.frame(dat)
dat<-na.omit(dat)
cont_covariates <- c("Age","High","Weight","BMI","CPBT","XclampT","DHCAT","cereperfusionT","cereperfusion","LNT","LPT","Reb","Plam","Plat","SymhosT","HossurgT","Sym.surgT","Root","LA","LVEDD","LVESD","IVS","LVEF","ProxAo","RBC","Hb","WBC","Plt","N.","N","L.","cTnt","BNP","fibrinogen","D2","INR","Tbil","albumin","ALT","AST","urea","Cr","Na","K","CL","LAC",data=dat)
cate_covariates <- c("Male","HBP","DM","CAD","CKD","Smoking","FamilyHistory","Heartsurgery","Stroke","AFhistory","anticoagulation","Warfarin","antiplatelet","aspirin","IMH","PAU","MFS","BAV","TEVARR","IscCerebral","IscSpinal","IscCoronary","IscMesenteric","IscRenal","IscUEM","IscLEM","Hypotension","Ventilation","Shock","Tamponade","CPStatus","Emergency","ARR","Bentall","David","FET","CABG","FLRoot","FLascending","FLarch","FLdescending","tearRoot","tearascending","teararch","teardescending","commissuredistachment","RNCD","LNCD","LRCD","Sinusinvolve","RCANeri","LCANeri","IA","LCCA","LSCA","ReCPB","ReACC","UACP","BACP","Transfusion","AI","hydropericardium","hosMortality","RereCPB","AS",data=dat)

task<-as_task_classif(dat, target="hosMortality")
#int_cols <- task$feature_types[type == "integer"]$id
#dat[, int_cols] <- lapply(dat[, int_cols], as.numeric)
#task$cbind(dat[, int_cols])
task
library(mlr3fselect)
#交叉验证的递归特征消除
optimizer = fs("rfecv",
               n_features = 2,
               feature_number = 1)
learner = lrn("classif.rpart",
              predict_type = "prob")
instance = fsi(
  task = task,
  learner = learner,
  resampling = rsmp("cv", folds = 10),
  measures = msr("classif.auc"),
  terminator = trm("none"))
optimizer$optimize(instance)
library(ggplot2)
library(viridisLite)
library(mlr3misc)

data = as.data.table(instance$archive)[!is.na(iteration), ]
aggr = data[, list("y" = mean(unlist(.SD))), by = "batch_nr", .SDcols = "classif.auc"]
aggr[, batch_nr := 39 - batch_nr]

data[, n:= map_int(importance, length)]

ggplot(aggr, aes(x = batch_nr, y = y)) +
  geom_line(
    color = viridis(1, begin = 0.5),
    linewidth = 1) +
  geom_point(
    fill = viridis(1, begin = 0.5),
    shape = 21,
    size = 3,
    stroke = 0.5,
    alpha = 0.8) +
  geom_vline(
    xintercept = aggr[y == max(y)]$batch_nr,
    colour = viridis(1, begin = 0.33),
    linetype = 3
  ) +
  xlab("Number of Features") +
  ylab("Mean AUC") +
  scale_x_reverse() +
  theme_minimal()
as.data.table(instance$archive)[, list(features, classif.auc, iteration, importance)]
task$select(instance$result_feature_set)
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
s <- kernelshap(learner,X=dat,bg_X=dat)#成功
s
library(shapviz)
# 构建可视化对象
shp <- shapviz(s) 
# 变量重要性
sv_importance(shp,kind = "bee") 
sv_importance(shp, kind = "beeswarm")
sv_importance(shp,fill = "#0085FF")
#(dbl)[p0.1L](INR\CPBT\Male\IscCerebral\anticoagulation\Cr\IscCoronary\cirfailure)\albumin\AHF\IVS\Iscspinal\Iscspinal\IscLEM\D2\Reb
#[p0.1L](INR\Reb\CPBT\LA)\cirfailure\AHF\Plam\AST\AFhistory\D2\Isccoronary\Cr\Ventilation\IscSpinal\IscCerebral
#(dbl)[p0.05](INR\CPBT)\Male\fibrinogen\IscCerebral\LAC\IscMesenteric\AST\IscLEM\IscSpinal\Tamponade\Bypass\BNP\CPStatus
#[p0.05](INR\Reb\CPBT\XclampT)\urea\AS\cirfailure\albumin\cTnt\Tamponade\N\Bypass\IscMesenteric\fibrinogen
#(dbl)[p0.1L](INR\Reb\CPBT\LA)\LAC\Tamponade\IscCoronary\IVS\Ventilation\IscMesenteric\D2\IscSpinal\cirfailure\CPStatus
#[p0.1L](INR\Reb\CPBT\LA)\LAC\Tamponade\IscCoronary\IVS\Ventilation\IscMesenteric\D2\IscSpinal\cirfailure\CPStatus
#[p0.1](D2\Reb\INR\CPBT\Plam\IVS\Cr\LA\XclampT)\CPStatus\hydropericardium\AHF\N\IscSpinal
#15-22（CPStsatus\CPBT\N)\CABG\AS\FLThrombosed\IscSpinal\IVS\Male\IscLEM\AHF\Cirfailure
#0.05p CPStsatus\N\BNP\FLRoot\Tamponade\IVS\Plt\cTnt\Ventilation\Transfudion
#CPStsatus\N\BNP\fibrinogen\Ventilation\IscMesenteric\IscLEM\FLRoot