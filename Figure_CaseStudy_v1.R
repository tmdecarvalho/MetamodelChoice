library(openxlsx)
library(ggplot2)
library(gridExtra)

# options                               **************************************
workstation <- "office" # "laptop" #  # "cloud"  # 
typeoffig <-  "single" #   "panel" #   
resolution <- 100


if(workstation=="office") { filedir <- "N:\\Documents\\Metamodels\\CaseStudy" } 
if(workstation=="laptop") { filedir <- "C:\\Users\\Tiago\\Documents\\GP2020\\CaseStudy"}
if(workstation=="cloud")  { filedir <- "/cloud/project"}


if(!workstation=="cloud")  {separ <- "\\"}
if(workstation=="cloud")  {separ <- "/"}

modeloutcome <- 4
# options: 3, 4, 8, 9  (these correspond to model outcomes, total costs VATS, SBRT ;  QALYs VATS,  SBRT)
n_expdes <- 220
runname <- paste("PSAdata_outcome",modeloutcome,"nexpdes",n_expdes,"v2.xlsx",sep="_")
runname2 <- paste("PSAdata_outcome",modeloutcome,"nexpdes",n_expdes,"v2.tiff",sep="_")
filetoread <- paste(filedir,runname,sep="\\") 
figurename1 <-paste(filedir,runname2,sep="\\") 

table1 <- data.frame (read.xlsx(filetoread, sheet=4,colNames=TRUE,rowNames = FALSE))

tiff(filename=figurename1,width =12, height =12, units = 'in', res = resolution)  # set resolution
fig1 <- ggplot(table1,aes(x=out_mod_pred,y=predict_ols2))+
 geom_point(color="grey50") + geom_abline(slope=1,intercept = 0) +
 labs(y="lm", x="Observed model outcome") +
  ylim(0,max(table1$out_mod_pred)) + xlim (0,max(table1$out_mod_pred))

fig2 <- ggplot(table1,aes(x=out_mod_pred,y=predict_GP32))+
  geom_point(color="grey50") + geom_abline(slope=1,intercept = 0) +
  labs(y="GPFit", x="Observed model outcome")+
  ylim(0,max(table1$out_mod_pred)) + xlim (0,max(table1$out_mod_pred))

fig3 <- ggplot(table1,aes(x=out_mod_pred,y=predict_nn))+
  geom_point(color="grey50") + geom_abline(slope=1,intercept = 0) +
  labs(y="nnet", x="Observed model outcome")+
  ylim(0,max(table1$out_mod_pred)) + xlim (0,max(table1$out_mod_pred))

fig4 <- ggplot(table1,aes(x=out_mod_pred,y=predict_nn2))+
  geom_point(color="grey50") + geom_abline(slope=1,intercept = 0) +
  labs(y="neuralnet", x="Observed model outcome")+
  ylim(0,max(table1$out_mod_pred)) + xlim (0,max(table1$out_mod_pred))

fig5 <- ggplot(table1,aes(x=out_mod_pred,y=predict_mgcv2))+
  geom_point(color="grey50") + geom_abline(slope=1,intercept = 0) +
  labs(y="mgcv", x="Observed model outcome")+
  ylim(0,max(table1$out_mod_pred)) + xlim (0,max(table1$out_mod_pred))

fig6 <- ggplot(table1,aes(x=out_mod_pred,y=predict_mb2))+
  geom_point(color="grey50") + geom_abline(slope=1,intercept = 0) +
  labs(y="mboost", x="Observed model outcome")+
  ylim(0,max(table1$out_mod_pred)) + xlim (0,max(table1$out_mod_pred))

fig7 <- ggplot(table1,aes(x=out_mod_pred,y=predict_gbm2))+
  geom_point(color="grey50") + geom_abline(slope=1,intercept = 0) +
  labs(y="gbm", x="Observed model outcome")+
  ylim(0,max(table1$out_mod_pred)) + xlim (0,max(table1$out_mod_pred))

fig8 <- ggplot(table1,aes(x=out_mod_pred,y=predict_rf2))+
  geom_point(color="grey50") + geom_abline(slope=1,intercept = 0) +
  labs(y="ranger", x="Observed model outcome")+
  ylim(0,max(table1$out_mod_pred)) + xlim (0,max(table1$out_mod_pred))

mainfig2 <- grid.arrange(fig1,fig2,fig3,fig4,fig5,fig6,fig7,fig8, ncol=2) 

dev.off()
