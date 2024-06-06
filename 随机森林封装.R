dat <- read.csv("traindata0.5.csv")
dat<data.frame(dat)
dat<-na.omit(dat)
library(randomForest)
set.seed(321)
fit <- randomForest(hosMortality~., data = dat)
fit
plot(fit)
which.min(fit$err.rate[,1])
ff<-importance(fit)
ff
varImpPlot(fit)
set.seed(647)
res <- rfcv(trainx = dat[,-1],trainy = dat[,1],
            cv.fold = 10,
            recursive = T
)
res$n.var #变量个数
res$error.cv #错误率
with(res, plot(n.var, error.cv, type="o", lwd=2))
#(dbl)[p0.1L]CPBT\INR\D2\LA\BNP\Cr\fibrinogen\XclampT\AST\Reb\LAC\Plam\IVS\albumin\CPStatus\IscMesenteric\CABG\IscSpinal\IscCerebral\IscLEM\Ventilation\Bypass\IscCoronary\Male\cirfailure\anticoagulation\Tamponade\AHF\AFhistory
#(dbl)[p0.05]CPBT\INR\D2\fibrinogen\WBC\BNP\Cr\N\Reb\urea\XclampT\Plam\AST\LAC\IVS\cTnt\albumin\CPStatus\IscMesenteric\CABG\IscSpinal\Ventilation\IscCerebral\Bypass\IscLEM\CPBN\Male\IscCoronary\Tamponade\cirfailure
#(dbl)[p0.1PL](CPBT\INR\D2\LA\Reb\LAC\IVS)\albumin\CPStatus\IscMesenteric\CABG\IscSpinal\IscLEM\IscCerebral\Ventilation\Bypass\IscCoronary\Tamponade\cirfailure
#[p0.1](CPBT\INR\D2\LA\WBC\Reb\fibrinogen\urea\BNP\N\Cr\Plam\XclampT\AST\LAC\IVS\cTnt\albumin\K)\CPStatus\IscMesenteric\CABG\IscSpinal\Sinusinvolve\Ventilation\IscCerebral\hydropericardium\IscLEM\LSCA
#CPBT\INR\Plt\D2\AST\fibrinogen\Plam\BNP
#0.05p CPBT\AST\fibrinogen\ACC\BNP\Plam\D2\CPStatus\N\urea\INR\Plt\WBC
#CPBT\AST\fibrinogen\Plam\D2\N\BNP\INR\urea