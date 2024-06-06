library(readr)
library(mlr3verse)
library(ggplot2)
dat <- read.csv("traindata0.5.csv")
dat<data.frame(dat)
dat<-na.omit(dat)
cont_covariates <- c("Age","High","Weight","BMI","CPBT","ACC","DHCAT","LNT","LPT","Reb","Plam","SymhosT","HossurgT","Sym.surgT","Root","LAD","LVEDD","LVESD","IVS","LVEF","ProxAo","Hb","WBC","Plt","N","cTnt","BNP","fibrinogen","D2","INR","Tbil","albumin","ALT","AST","urea","Cr","Na","K",data=dat)
cate_covariates <- c("Male","HBP","DM","CAD","CKD","Smoking","FamilyHistory","Heartsurgery","Stroke","AFhistory","anticoagulation","Warfarin","antiplatelet","aspirin","IMH","PAU","MFS","BAV","TEVARR","IscCerebral","IscSpinal","IscCoronary","IscMesenteric","IscRenal","IscUEM","IscLEM","Hypotension","Ventilation","Shock","Tamponade","CPStatus","Emergency","ARR","Bentall","David","FET","CABG","FLRoot","FLascending","FLarch","FLdescending","tearRoot","tearascending","teararch","teardescending","commissuredistachment","RNCD","LNCD","LRCD","Sinusinvolve","RCANeri","LCANeri","IA","LCCA","LSCA","ReCPB","ReACC","UACP","BACP","Transfusion","AI","hydropericardium","hosMortality","RereCPB","AS",data=dat)
task<-as_task_classif(dat, target="hosMortality")
#int_cols <- task$feature_types[type == "integer"]$id
#dat[, int_cols] <- lapply(dat[, int_cols], as.numeric)
#task$cbind(dat[, int_cols])
#task
library(mlr3fselect)
optimizer = fs("rfecv",
               n_features = 1,
               feature_number = 1)
learner = lrn("classif.svm",
              type = "C-classification",
              kernel = "linear",
              predict_type = "prob")

instance = fsi(
  task = task,
  learner = learner,
  resampling = rsmp("cv", folds = 10),
  measures = msr("classif.auc"),
  terminator = trm("none"),
  callback = clbk("mlr3fselect.svm_rfe"))

optimizer$optimize(instance)
library(ggplot2)
library(viridisLite)
library(mlr3misc)

data = as.data.table(instance$archive)[!is.na(iteration), ]
aggr = data[, list("y" = mean(unlist(.SD))), by = "batch_nr", .SDcols = "classif.auc"]
aggr[, batch_nr := 30 - batch_nr]

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
#最终模型使用完整数据集上的最佳特征集进行训练。最佳集包含20个特征，并存储在instance$result_feature_set中
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
pred1 <- learner$predict(task)#测试集
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
#(dbl)[p0.1L]Plam\CPStatus\LA\D2\CPBT\LAC\albumin\IVS\INR\fibrinogen\IscMesenteric\CABG\IscCerebral\IscSpinal\cirfailure
#(dbl)[p0.05]CPBT/Plam/INR/LAC/IVS/CPStatus/D2/XclampT/IscMesenteric/Male/albumin/urea/CPBN/Cr
#(dbl)[p0.1PL]D2/CPStatus/CPBT/LA/Reb/INR/albumin/IVS/LAC/IscMesenteric/IscCerebral/IscLEM/Tamponade/CABG/IscSpinal
#[p0.1PL]CPStatus/D2/CPBT/LA/Reb/albumin/LAC/IVS/INR/IscMesenteric/IscLEM/Tamponade/IscCerebral/IscCoronary/CABG
#[p0.05]Plam/CPBT/CPStatus/Reb/XclampT/albumin/LAC/N/IVS/D2/IscMesenteric/INR/Male/CABG
#[p0.1]CPBT/CPStatusP/Plam/Sinusinvolve/LSCA/IscRenal/N/XclampT/IscMesenteric/D2/IscLEM/Reb/WBC/IA/CABG
#CPBT\CPStatus\Plam\N\commissuredistachment\XclampT\D2\IscCerebral\CABG\IscMesenteric\urea
#0,1p CPBT\Plam\N\commissuredistachment\XclampT\AST\WBC\D2\CPStatus\Age\IVS
#0.05p CPBT\Plam\Transfusion\CPStatus\AST\D2\IVS\ACC\N\FLRoot\fibrinogen\Temponade\Reb\CBG
#CPBT\AST\Plam\CPStatus\ACC\D2\IVS\FLRoot