#####################  Author : Tiago M. de Carvalho  11072019 ########

# test computation time of several metamodels per number of parameters (microbenchmark)

# version March 2020: first generate a dataset as for the accuracy study and
# use it to measure computation time. 

# MEMORY management ************************************************************************************************************
ls()  #list all objects
gc() 
rm(list=ls()) # remove all objects in memory
options(digits=3) # print digits

# PACKAGES 
# Emulators/Metamodels            **********************************************
# decision trees
library(randomForest) # standard random forest package
library(ranger)  # random forest II
library(xgboost) # boosting  (and random forest)
library(gbm)  # boosting
# gaussian process regression
library(GPfit)  # main GP
library(kernlab) # fast GP with multiple types of correlation matrices
library(tgp)  # fast GP (bayesian) , treed-GP
# neural networks
library(nnet)  
library(neuralnet)
#library(RSNNS) # to be tried?
#generalized additive model
library(mgcv) # GAM 
library(gam)  # GAM
library(mboost) # boosting GAM
#penalized likelihood
library(glmnet)  # lasso, ridge, elastic nets (for generalized linear models)

#bayesian networks
library(bnlearn)
#general
#library(caret) # machine learning : 238 models available
library(microbenchmark) # check computation time
#library(dlstats)  # for downloads stats of each package...does not work in the office computer
library(MASS)  # to simulate multivariate normal
library(xlsx)   # reading/writing Excel; may require Java installation , make sure Java version is compatible with R
# figures
library(ggplot2) # plot figures


# file directory *********************

# inputs to be pre-defined by the user 
workstation <-  "work" # "workcloud" #   "laptop"  # where is the analyses being run?
#set file directory according to work location
if(workstation=="cloud")     { filedir <-"/cloud/project"; separ <- "/" }  
if(!workstation=="cloud")  {separ <- "\\"}
if(workstation=="laptop")    { filedir <-"C:/Users/Tiago/Documents/GP2020/R" }
if(workstation=="work")      { filedir <- "N:\\Documents\\Metamodels\\R code\\article" }
if(workstation=="workcloud") { filedir <- "C:\\Users\\userEB\\Documents\\Tiago_Projects\\Tiago_metamodel_project2"}




# SIMULATION PARAMETERS ******************************************************************************
repetitions <-5 # how many times should the computation be repeated
nparams <-10
allruns <- FALSE  # TRUE: p10s100 and p10s200  FALSE: p5s50, p5s100
# specific emulator options  ***************************************************************************

# # isotropic GP # kernels list : "rbfdot","polydot","vanilladot","tanhdot","laplacedot","besseldot", "anovadot", "splinedot"
kernels<-c("rbfdot") #,"laplacedot","besseldot", "anovadot", "splinedot") #

#NN
# single layer: nnet package ... # architecture: number of nodes for the hidden layer
architectures <- c(nparams,round(nparams/1.5),round(nparams/3),nparams,round(nparams/1.5),round(nparams/3))
architectures2<- 5 ; 
i.weights2<- (5+1)*architectures2 + (architectures2+1) ; initial.weight2 <-0.1
initial.weight <- c(0.1,0.1,0.1,0.01,0.01,0.01)  # value for initial weight vector (note: this is the same value for every weight!)
# for each architecture set the number of weights. .... # note: maximum number of weights is set to 1000 by default.
i.weights <- c( (nparams+1)*architectures[1] + (architectures[1]+1), (nparams+1)*architectures[2] + (architectures[2]+1),
                (nparams+1)*architectures[3] + (architectures[3]+1), (nparams+1)*architectures[4] + (architectures[4]+1),
                (nparams+1)*architectures[5] + (architectures[5]+1), (nparams+1)*architectures[6] + (architectures[6]+1))
maxiter_nn <- 100000

# GAM  mboost
shrink <-c(0.1,0.1,0.1,0.5,0.5,0.5) ; stop <- c(100,200,300,100,200,300)
# GAM : other models?
allp <- c(1:nparams)
somep <- c(1:5)
formulagam <-as.formula(paste0("y_stand~",paste0("s(V",allp,")",collapse="+"),collapse=""))
formulagam2<-as.formula(paste0("y_stand~",paste0("s(V",somep,")",collapse="+"),collapse=""))
spline <- c(1:4) ; linear <- c(5:nparams) ; linear2 <-5
formulagam_mgcv1 <-as.formula(paste0("y_stand~",paste0("s(V",spline,")",collapse="+"),"+",paste0("V",linear,collapse="+"),collapse=""))
formulagam_mgcv2 <-as.formula(paste0("y_stand~",paste0("s(V",spline,")",collapse="+"),"+",paste0("V",linear2,collapse="+"),collapse=""))
formulamboost1 <- as.formula(paste0("y_training1~",paste0("X",allp,collapse="+"),collapse=""))
formulamboost2 <- as.formula(paste0("y_training1~",paste0("X",somep,collapse="+"),collapse=""))


#decision trees ... #random forest parameters
ntrees <- 2500 ;  vars <- c(nparams, round(nparams/1.5),round(nparams/2),round(nparams/3))  #round(sqrt(nparams))
#boosting (can choose either one of them for a loop)
eta <- c(0.01,0.05,0.25,0.01,0.05,0.25)
subsample <- c(0.6,0.6,0.6,0.8,0.8,0.8)


#   END : parameter specifications                  ********************************************

# readfiles with data for model fitting

filename1 <- "accur_M_10000_N_5000_Scenario_comptime_p10_s50_.xlsx"
filename2 <- "accur_M_10000_N_5000_Scenario_comptime_p10_s100_.xlsx"
filename3 <- "accur_M_10000_N_5000_Scenario_comptime_p10_s200_.xlsx"
filetoread1 <- paste(filedir,filename1,sep=separ)
filetoread2 <- paste(filedir,filename2,sep=separ)
filetoread3 <- paste(filedir,filename3,sep=separ)

expdes1 <- data.frame (read.xlsx(filetoread1, sheetIndex=3))
out_train1 <- data.frame (read.xlsx(filetoread1, sheetIndex=5))
expdes2 <- data.frame (read.xlsx(filetoread2, sheetIndex=3))
out_train2 <- data.frame (read.xlsx(filetoread2, sheetIndex=5))
expdes3 <- data.frame (read.xlsx(filetoread3, sheetIndex=3))
out_train3 <- data.frame (read.xlsx(filetoread3, sheetIndex=5))


# scenario 1  nparam1=10, nexpdes =50             ------   ****************************

n_expdes1 <- dim(expdes1)[1] ;  nparams1 <- dim(expdes1)[2]
H <- matrix(0, n_expdes1 ,nparams1) ; Z <- H
H <- as.matrix(expdes1)
out_mod <- as.matrix(out_train1)
y_stand <- (out_mod - mean(out_mod))/sd(out_mod)
for (i in c(1:nparams1)) { Z[,i] <- (H[,i] - mean(H[,i]))/sd(H[,i]) }
H_df <- as.data.frame(Z)
ddata <- data.frame(y_stand,Z) # for mboost and bnlearn only
W <- data.frame(Z,y_stand) 

# run simulation
if(!allruns) {
set.seed(1001) # random seed [for reproducibility]

computation_times1 <- microbenchmark( 
    lm          =  lm(y_stand~.,data=H_df),
    kernlab     =  gausspr(Z,y_stand,scaled=FALSE,kernel=kernels), 
    bgp          =  bgp(Z,y_stand,meanfn = "linear", bprior = "bflat", corr = "expsep", BTE = c(1000, 4000,2)) ,    
    GPfit        =   GP_fit(X=Z,Y=y_stand,control = c(100*nparams, 40* nparams, 2* nparams),maxit=50,trace=TRUE),                                                                         # exclude if nparams > 5
   nnet         =  nnet(Z,y_stand,maxit=maxiter_nn,Wts=rep(initial.weight[2],i.weights[2]),size=architectures[2],linout=TRUE,abstol = 1.0e-4,reltol=1.0e-8),
   neuralnet    =  try(neuralnet(y_stand~.,data=H_df,stepmax=maxiter_nn,rep=1, startweights=rep(initial.weight[2],times=i.weights[2]), hidden=architectures[2],linear.output=TRUE,threshold=0.01,algorithm="rprop+")),
    gam         =  gam::gam(formulagam,data=H_df,family="gaussian"),
    mgcv        =  mgcv::gam(formulagam_mgcv1,data=H_df,method="REML"),
    mboost      =  mboost(formulamboost1,data=ddata,family=Gaussian(),baselearner=("bbs"),control=boost_control(mstop=stop[2],nu= shrink[2] ,center=FALSE) )  ,
   ranger       =   ranger(y_stand~.,H_df,num.trees=ntrees,mtry=vars[1]), 
    gbm         =  gbm(y_stand~.,data=H_df,distribution="gaussian",n.trees=ntrees,interaction.depth =2,bag.fraction =  subsample[4] ,shrinkage= eta[2]),
    bnlearn     =   { baynetwork2 <- hc(W)  ;    bn.fit(baynetwork2,data=W) } , 
   times=repetitions) #repetitions

# summarize results
computation_times_df1 <- data.frame(summary(computation_times1))
}

# scenario 2  nparam1=10, nexpdes = 100          ------   ****************************
if(allruns) {
n_expdes2 <- dim(expdes2)[1] ;  nparams2 <- dim(expdes2)[2]
H <- matrix(0, n_expdes2 ,nparams2) ; Z <- H
H <- as.matrix(expdes2)
out_mod <- as.matrix(out_train2)
y_stand <- (out_mod - mean(out_mod))/sd(out_mod)
for (i in c(1:nparams2)) { Z[,i] <- (H[,i] - mean(H[,i]))/sd(H[,i]) }
H_df <- as.data.frame(Z)
ddata <- data.frame(y_stand,Z) # for mboost and bnlearn only
W <- data.frame(Z,y_stand) 



set.seed(1001) # random seed [for reproducibility]

computation_times2 <- microbenchmark( 
  lm          =  lm(y_stand~.,data=H_df),
  kernlab     =  gausspr(Z,y_stand,scaled=FALSE,kernel=kernels), 
   bgp          =  bgp(Z,y_stand,meanfn = "linear", bprior = "bflat", corr = "expsep", BTE = c(1000, 4000,2)) ,    
   GPfit        =   GP_fit(X=Z,Y=y_stand,control = c(100*nparams, 40* nparams, 2* nparams),maxit=50,trace=TRUE),                                                                         # exclude if nparams > 5
  nnet         =  nnet(Z,y_stand,maxit=maxiter_nn,Wts=rep(initial.weight[2],i.weights[2]),size=architectures[2],linout=TRUE,abstol = 1.0e-4,reltol=1.0e-8),
  neuralnet    =  try(neuralnet(y_stand~.,data=H_df,stepmax=maxiter_nn,rep=1, startweights=rep(initial.weight[2],times=i.weights[2]), hidden=architectures[2],linear.output=TRUE,threshold=0.01,algorithm="rprop+")),
  gam         =  gam::gam(formulagam,data=H_df,family="gaussian"),
    mgcv        =  mgcv::gam(formulagam_mgcv1,data=H_df,method="REML"),
  mboost      =  mboost(formulamboost1,data=ddata,family=Gaussian(),baselearner=("bbs"),control=boost_control(mstop=stop[2],nu= shrink[2] ,center=FALSE) )  ,
  ranger       =   ranger(y_stand~.,H_df,num.trees=ntrees,mtry=vars[1]), 
  gbm         =  gbm(y_stand~.,data=H_df,distribution="gaussian",n.trees=ntrees,interaction.depth =2,bag.fraction =  subsample[4] ,shrinkage= eta[2]),
  bnlearn     =   { baynetwork2 <- hc(W)  ;    bn.fit(baynetwork2,data=W) } , 
  times=repetitions) #repetitions

# summarize results
computation_times_df2 <- data.frame(summary(computation_times2))


# scenario 3  nparam1=10, nexpdes = 100   ------   ****************************

n_expdes3 <- dim(expdes3)[1] ;  nparams3 <- dim(expdes3)[2]
H <- matrix(0, n_expdes3 ,nparams3) ; Z <- H
H <- as.matrix(expdes3)
out_mod <- as.matrix(out_train3)
y_stand <- (out_mod - mean(out_mod))/sd(out_mod)
for (i in c(1:nparams3)) { Z[,i] <- (H[,i] - mean(H[,i]))/sd(H[,i]) }
H_df <- as.data.frame(Z)
ddata <- data.frame(y_stand,Z) # for mboost and bnlearn only
W <- data.frame(Z,y_stand) 

set.seed(1001) # random seed [for reproducibility]



computation_times3 <- microbenchmark( 
  lm          =  lm(y_stand~.,data=H_df),
  kernlab     =  gausspr(Z,y_stand,scaled=FALSE,kernel=kernels), 
   bgp          =  bgp(Z,y_stand,meanfn = "linear", bprior = "bflat", corr = "expsep", BTE = c(1000, 4000,2)) ,    
   GPfit        =   GP_fit(X=Z,Y=y_stand,control = c(100*nparams, 40* nparams, 2* nparams),maxit=50,trace=TRUE),                                                                         # exclude if nparams > 5
  nnet         =  nnet(Z,y_stand,maxit=maxiter_nn,Wts=rep(initial.weight[2],i.weights[2]),size=architectures[2],linout=TRUE,abstol = 1.0e-4,reltol=1.0e-8),
  neuralnet    =  try(neuralnet(y_stand~.,data=H_df,stepmax=maxiter_nn,rep=1, startweights=rep(initial.weight[2],times=i.weights[2]), hidden=architectures[2],linear.output=TRUE,threshold=0.01,algorithm="rprop+")),
  gam         =  gam::gam(formulagam,data=H_df,family="gaussian"),
  mgcv        =  mgcv::gam(formulagam_mgcv1,data=H_df,method="REML"),
  mboost      =  mboost(formulamboost1,data=ddata,family=Gaussian(),baselearner=("bbs"),control=boost_control(mstop=stop[2],nu= shrink[2] ,center=FALSE) )  ,
  ranger       =   ranger(y_stand~.,H_df,num.trees=ntrees,mtry=vars[1]), 
  gbm         =  gbm(y_stand~.,data=H_df,distribution="gaussian",n.trees=ntrees,interaction.depth =2,bag.fraction =  subsample[4] ,shrinkage= eta[2]),
  bnlearn     =   { baynetwork2 <- hc(W)  ;    bn.fit(baynetwork2,data=W) } , 
  times=repetitions) #repetitions

# summarize results
computation_times_df3 <- data.frame(summary(computation_times3))


}


# scenario 4  nparam1=5, nexpdes = 50   ------   ****************************
if (!allruns) {
  
n_expdes1 <- dim(expdes1)[1] ;  nparams1 <- dim(expdes1)[2]/2
H <- matrix(0, n_expdes1 ,nparams1) ; Z <- H
H <- as.matrix(expdes1)
H <- H[,1:nparams1]  # only 5 params
out_mod <- as.matrix(out_train1)
y_stand <- (out_mod - mean(out_mod))/sd(out_mod)
for (i in c(1:nparams1)) { Z[,i] <- (H[,i] - mean(H[,i]))/sd(H[,i]) }
H_df <- as.data.frame(Z)
ddata <- data.frame(y_stand,Z) # for mboost and bnlearn only
W <- data.frame(Z,y_stand) 

# run simulation
set.seed(1001) # random seed [for reproducibility]

computation_times4 <- microbenchmark( 
  lm          =  lm(y_stand~.,data=H_df),
  kernlab     =  gausspr(Z,y_stand,scaled=FALSE,kernel=kernels), 
  bgp          =  bgp(Z,y_stand,meanfn = "linear", bprior = "bflat", corr = "expsep", BTE = c(1000, 4000,2)) ,    
  GPfit        =   GP_fit(X=Z,Y=y_stand,control = c(100*nparams, 40* nparams, 2* nparams),maxit=50,trace=TRUE),                                                                         # exclude if nparams > 5
  nnet         =  nnet(Z,y_stand,maxit=maxiter_nn,Wts=rep(initial.weight2,i.weights2),size=architectures2,linout=TRUE,abstol = 1.0e-4,reltol=1.0e-8),
  neuralnet    =  try(neuralnet(y_stand~.,data=H_df,stepmax=maxiter_nn,rep=1, startweights=rep(initial.weight2,times=i.weights2), hidden=architectures2,linear.output=TRUE,threshold=0.01,algorithm="rprop+")),
  gam         =  gam::gam(formulagam2,data=H_df,family="gaussian"),
   mgcv        =  mgcv::gam(formulagam_mgcv2,data=H_df,method="REML"),
  mboost      =  mboost(formulamboost2,data=ddata,family=Gaussian(),baselearner=("bbs"),control=boost_control(mstop=stop[2],nu= shrink[2] ,center=FALSE) )  ,
  ranger       =   ranger(y_stand~.,H_df,num.trees=ntrees,mtry=vars[3]), 
  gbm         =  gbm(y_stand~.,data=H_df,distribution="gaussian",n.trees=ntrees,interaction.depth =2,bag.fraction =  subsample[4] ,shrinkage= eta[2]),
  bnlearn     =   { baynetwork2 <- hc(W)  ;    bn.fit(baynetwork2,data=W) } , 
  times=repetitions) #repetitions

# summarize results
computation_times_df4 <- data.frame(summary(computation_times4))

}


# FIGURE
runname <- paste("CompTime_",repetitions,".xlsx",sep="_")
runname2 <-  paste("CompTime",repetitions,".tiff",sep="_")
runname3 <-  paste("CompTime2",repetitions,".tiff",sep="_")
figurename <- paste(filedir,runname2,sep=separ) 
figurename2 <- paste(filedir,runname3,sep=separ) 
filename <- paste(filedir,runname,sep=separ) 

## as a 4 panel figure
#tiff(filename=figurename,width = 6, height = 6, units = 'in', res = 100)
#p1<-autoplot(computation_times1,y_max = 500) + ggtitle("p=10,S=50")
#p2<-autoplot(computation_times4,y_max = 500) + ggtitle("p=5, S=50")
#p3<-autoplot(computation_times2,y_max = 6000) + ggtitle("p=10, S=100")
#p4<-autoplot(computation_times3,y_max= 6000 ) + ggtitle("p=10, S=200")
#mainfig<- grid.arrange(p1,p2,p3,p4,ncol=2) 
#dev.off()

# maybe better as two panel
tiff(filename=figurename,width = 6, height = 6, units = 'in', res = 100)
p1<-autoplot(computation_times4) + ggtitle("p=5, S=50") 
p2<-autoplot(computation_times1) + ggtitle("p=10, S=50")
mainfig<- grid.arrange(p1,p2)
dev.off()

#tiff(filename=figurename2,width = 6, height = 6, units = 'in', res = 100)
#p1<-autoplot(computation_times2) + ggtitle("p=10, S=100") 
#p2<-autoplot(computation_times3) + ggtitle("p=10, S=200")
#mainfig<- grid.arrange(p1,p2)
#dev.off()



listoutputs <-list("n50p10"=computation_times_df1,  "n50p5"= computation_times_df4)
#listoutputs2 <- list("n100p10"=computation_times_df2,"n200p10"=computation_times_df3)
openxlsx::write.xlsx(listoutputs2,filename,asTable =TRUE,colnames=TRUE,colWidths=12)

