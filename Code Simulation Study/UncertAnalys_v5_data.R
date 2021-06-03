
#####################  Author : Tiago M. de Carvalho  22 05 2016   ########


# this file runs the case study analyses of the lung cancer model in MDM article 2021


# MEMORY management ************************************************************************************************************
ls()
gc()
rm(list=ls())
options(digits=3) # print digits


# USES PACKAGES  ############################################
library(openxlsx)

# Emulators/Metamodels **********************************************
# decision trees / boosting
#library(randomForest) # "slow random forest" 
library(ranger)  # fast random forest 
#library(xgboost) # boosting and random forest
library(gbm)
library(mboost)  
# gaussian process regression
library(GPfit)  # slow GP
library(kernlab) # fast GP and other multiple types of correlation matrices
library(tgp)  # fast GP (bayesian) , treed-GP
# neural networks
library(neuralnet)
library(nnet)
#generalized additive model
library(mgcv)
library(gam)# GAM 
#penalized likelihood
#library(glmnet)  # lasso, ridge, elastic nets (for glm's)
#general
#library(caret) # machine learning : 238 models available

# figures
# library(ggplot2) # does not work at office....



# INPUT         ############################################

workstation <- "work" # "cloud" # "cloud"  # "workcloud" #   "laptop"  # where is the analyses being run?
#set file directory according to work location
if(workstation=="cloud")     { filedir <-"/cloud/project"; separ <- "/" }  
if(!workstation=="cloud")  {separ <- "\\"}
if(workstation=="laptop")    { filedir <-"C:/Users/Tiago/Documents/GP2020/R" }
if(workstation=="work")      { filedir <- "N:\\Documents\\Metamodels\\CaseStudy" }
if(workstation=="workcloud") { filedir <- "C:\\Users\\userEB\\Documents\\Tiago_Projects\\Tiago_metamodel_project2"}

filen <- "LC_PSA_dataset.xlsx"
filetoread <- paste(filedir,filen,sep=separ)

testing<-FALSE

emulatornames <-c("LM","GP: kernlab","GP: bgp", "GP: GPFit","ANN: nnet","ANN: neuralnet","GAM: gam","GAM: mgcv","GAM: mboost","RF: ranger","Boost: gbm", "BN: bnlearn") 
accuracynames <- c("predmean CV","PAE CV","MAE CV","MSE CV","RMSE CV","predmean all","PAE all ","MAE all","MSE all","RMSE all","hyperp1","hyperp2","hyperp3","hyperp4","hyperp5","hyperp6")

# model outcomes list =  1 to 10
# COSTS_VATS COSTS_SBRT	 disc_COSTS_VATS	 disc_COSTS_SBRT	
# disc_COSTS_incr	 QALY_VATS	 QALY_SBRT	 disc_QALY_VATS	 disc_QALY_SBRT	
# disc_QALY_incr
modeloutcomes<- 3  # column  article: 3,4,8,9
totalmodelparams <- c(1:22)

# if outcome is cost => utilities do not enter metamodel and costs of competing treatment
# if outcome is qaly => costs do not enter metamodel and also not qaly of the competing treatment
if(modeloutcomes %in% c(1,3)) {modelparams <- totalmodelparams[-c(11,13:19,21)]}
if(modeloutcomes %in% c(6,8)) {modelparams <- totalmodelparams[-c(15,20:22)]}
if(modeloutcomes %in% c(2,4)) {modelparams <- totalmodelparams[-c(11,12,14:20)]}
if(modeloutcomes %in% c(7,9)) {modelparams <- totalmodelparams[-c(12,14,20:22)]}



nparams <- length(modelparams) # parameters included in metamodel, 
sample_param <- 10
n_expdes <-length(totalmodelparams)* sample_param #nparams*sample_param # observations training data (in the article: S)
cvfold <- n_expdes/sample_param



#which metamodels should be fit?
fitNN <- FALSE # fit neural networks?
fitmboost <-FALSE# fit GAM boosting ?
fitGAM <-FALSE # FALSE  # fit GAM ?
fitGP <-FALSE# fit gaussian process (isotropic) 
fitBayesGP <- FALSE   # bayesian GP implementation
fitGP_fit <- FALSE
#decision trees (models not used in basecase)
fitboost <- FALSE #TRUE # # fit decision trees boosting?
fitRandomForest <- FALSE
fitBN <-TRUE

# emulator options: hyperparameters for each metamodel    ***************************************

# models used for main results
# isotropic GP
# kernels list : "rbfdot","polydot","vanilladot","tanhdot","laplacedot","besseldot", "anovadot", "splinedot"
kernels<-c("rbfdot","laplacedot","besseldot", "anovadot", "splinedot") #

#NN
# single layer: nnet package
# architecture: number of nodes for the hidden layer
architectures <- c(nparams,round(nparams/1.5),round(nparams/3),nparams,round(nparams/1.5),round(nparams/3))
initial.weight <- c(0.1,0.1,0.1,0.01,0.01,0.01)  # value for initial weight vector (note: this is the same value for every weight!)
# for each architecture set the number of weights.
# note: maximum number of weights is set to 1000 by default.
i.weights <- c( (nparams+1)*architectures[1] + (architectures[1]+1), (nparams+1)*architectures[2] + (architectures[2]+1),
                (nparams+1)*architectures[3] + (architectures[3]+1), (nparams+1)*architectures[4] + (architectures[4]+1),
                (nparams+1)*architectures[5] + (architectures[5]+1), (nparams+1)*architectures[6] + (architectures[6]+1))

# 2 layers one architecture, for neuralnet package
#architectures2 <- rbind(c(round(nparams/5),round(nparams/5)),c(round(nparams/2),round(nparams/2)),c(round(nparams/1.5),round(nparams/1.5)),
#                        c(round(nparams/5),round(nparams/5)),c(round(nparams/2),round(nparams/2)),c(round(nparams/1.5),round(nparams/1.5)) )

#i.weights2 <- c((nparams+1)*architectures2[1,1] + (architectures2[1,1]+1) *architectures2[1,2] + (architectures2[1,2]+1),
#                (nparams+1)*architectures2[2,1] + (architectures2[2,1]+1) *architectures2[2,2] + (architectures2[2,2]+1),
#                (nparams+1)*architectures2[3,1] + (architectures2[3,1]+1) *architectures2[3,2] + (architectures2[3,2]+1),
#                (nparams+1)*architectures2[4,1] + (architectures2[4,1]+1) *architectures2[4,2] +(architectures2[4,2]+1),
#                (nparams+1)*architectures2[5,1] + (architectures2[5,1]+1) *architectures2[5,2] + (architectures2[5,2]+1), 
#                (nparams+1)*architectures2[6,1] + (architectures2[6,1]+1) *architectures2[6,2] +(architectures2[6,2]+1))
maxiter_nn <- 100000

# GAM  mboost
shrink <-c(0.1,0.1,0.1,0.5,0.5,0.5)
stop <- c(100,200,300,100,200,300)
# GAM : other models?
allp <- c(1:nparams)
formulagam <-as.formula(paste0("y_stand[-cvobs[a:b]]~",paste0("s(V",allp,")",collapse="+"),collapse=""))
formulagam2<-as.formula(paste0("y_stand[-cvobs[a:b]]~",paste0("s(V",allp,",2)",collapse="+"),collapse=""))
formulagam3<-as.formula(paste0("y_stand[-cvobs[a:b]]~",paste0("s(V",allp,",6)",collapse="+"),collapse=""))
formulagam5<-as.formula(paste0("y_stand[-cvobs[a:b]]~",paste0("s(V",allp,",8)",collapse="+"),collapse=""))

formulagam11 <-as.formula(paste0("y_stand ~",paste0("s(V",allp,")",collapse="+"),collapse=""))
formulagam12<-as.formula(paste0("y_stand ~",paste0("s(V",allp,",2)",collapse="+"),collapse=""))
formulagam13<-as.formula(paste0("y_stand ~",paste0("s(V",allp,",6)",collapse="+"),collapse=""))
formulagam15<-as.formula(paste0("y_stand ~",paste0("s(V",allp,",8)",collapse="+"),collapse=""))


# s() = penalized spline, ti/te/t2()= tensor product various specs. 
spline <- c(1:((n_expdes/10)-1))
linear <- c((n_expdes/10):nparams)

formulagam_mgcv1 <-as.formula(paste0("y_stand[-cvobs[a:b]]~",paste0("s(V",allp,")",collapse="+"))) #,"+",paste0("V",linear,collapse="+"),collapse=""))
formulagam_mgcv3 <-as.formula(paste0("y_stand[-cvobs[a:b]]~",paste0("s(V",allp,",k=4)",collapse="+"))) # min(k)==3
formulagam_mgcv5 <-as.formula(paste0("y_stand[-cvobs[a:b]]~",paste0("s(V",allp,")",collapse="+")))

formulagam_mgcv11 <-as.formula(paste0("y_stand~",paste0("s(V",allp,")",collapse="+"))) #,"+",paste0("V",linear,collapse="+"),collapse=""))
formulagam_mgcv13 <-as.formula(paste0("y_stand~",paste0("s(V",allp,",k=4)",collapse="+"))) # min(k)==3
formulagam_mgcv15 <-as.formula(paste0("y_stand~",paste0("s(V",allp,")",collapse="+")))

#formulamboost <-as.formula(paste0("y_stand[-cvobs[a:b]]~",paste0("bbs(X",spline,")",collapse="+"),"+",paste0("X",linear,collapse="+"),collapse=""))
#formulamboost <- as.formula(paste0("y_stand[-cvobs[a:b]]~",paste0("X",allp,collapse="+"),collapse=""))
formulamboost <- as.formula(paste0("y_stand~",paste0("X",allp,collapse="+"),collapse=""))

#                       ***************************************
#decision trees
#random forest parameters
ntrees <- 2500 ;  
vars <- c(nparams, round(nparams/1.5),round(nparams/2),round(nparams/3))  #round(sqrt(nparams))
#boosting (can choose either one of them for a loop)
eta <- c(0.01,0.05,0.25,0.01,0.05,0.25)
subsample <- c(0.6,0.6,0.6,0.8,0.8,0.8)




#read and prepare data ************************************************************


exp_des_valid  <-data.frame(openxlsx::read.xlsx(filetoread,sheet=3,colNames=TRUE,rowNames = FALSE))
out_valid   <-data.frame(openxlsx::read.xlsx(filetoread,sheet=2,colNames=TRUE,rowNames = FALSE))

exp_des_train  <-data.frame(openxlsx::read.xlsx(filetoread,sheet=5,colNames=TRUE,rowNames = FALSE))
out_train   <-data.frame(openxlsx::read.xlsx(filetoread,sheet=4,colNames=TRUE,rowNames = FALSE))

set.seed(9999)

# training                      ************************************
H <- matrix(0, n_expdes ,nparams) ; Z <- matrix(0, n_expdes ,nparams)

H <- exp_des_train[,modelparams]
out_mod <- out_train[,modeloutcomes]

#prediction                   ************************************
H_pred <-  exp_des_valid[,modelparams]   #exp_des[-selection,modelparams]
out_mod_pred <- out_valid[,modeloutcomes] #[-selection,modeloutcomes]

#out_train <- NULL
#exp_des<-NULL
Z <- matrix(0,length(out_mod),nparams)
Z_pred <- matrix(0,length(out_mod_pred),nparams)

# standardize data and tranforming from matrix to data frame (necessary for many packages)
y_stand <- (out_mod - mean(out_mod) ) /sd(out_mod)
for (i in c(1:nparams)) { Z[,i] <- (H[,i] - mean(H[,i]))/sd(H[,i]) }
for (i in c(1:nparams)) { Z_pred[,i] <- (H_pred[,i] - mean(H[,i]))/sd(H[,i]) }

H_df <- as.data.frame(Z) ;   H_pred_df <- as.data.frame(Z_pred)
#auxiliary variables for cross-validation
number_cv <- round(n_expdes/cvfold) 
cvobs <- c(1:n_expdes)

#error stats collected            ************************************
  errorstats <- matrix(0,length(emulatornames),length(accuracynames))
  colnames(errorstats) <- accuracynames
  rownames(errorstats) <- emulatornames
  # add error distribution for each emulator
  error_matrix <- matrix(0,length(out_mod),length(emulatornames))
  

  
  yrange <-max(out_mod)-min(out_mod) ;   yrange2 <- max(out_mod_pred)-min(out_mod_pred)
  
# save predicted values           ************************************
  predict_ols2 <- matrix(0,length(out_mod_pred),1)
  predict_GP12 <- matrix(0,length(out_mod_pred),1) ;   predict_GP22 <- matrix(0,length(out_mod_pred),1)
  predict_GP32 <- matrix(0,length(out_mod_pred),1) 
  predict_nn <- matrix(0,length(out_mod_pred),1) ;   predict_nn2 <- matrix(0,length(out_mod_pred),1)
  predict_gam2 <- matrix(0,length(out_mod_pred),1) ;   predict_mgcv2 <- matrix(0,length(out_mod_pred),1)
  predict_mb2 <- matrix(0,length(out_mod_pred),1) ;   predict_gbm2 <- matrix(0,length(out_mod_pred),1)
 predict_rf2 <- matrix(0,length(out_mod_pred),1)
 
 
#EMULATORS******************************************************************* 
  
  # OLS
  predict_ols <- matrix(0,n_expdes,1)
  
  for (cv in 1: number_cv) {
    a<-(1 + cvfold*(cv-1)) ; b<-(cvfold + cvfold*(cv-1)) 
    OLS_model <- lm(y_stand[-cvobs[a:b]]~.,data=H_df[-cvobs[a:b],])
    predict_ols[cvobs[a:b]] <- predict(OLS_model,H_df[cvobs[a:b],])* sd(out_mod) +mean(out_mod)
  }
  
  errorstats[1,1]  <- mean(predict_ols)
  errorstats[1,2] <-  mean(abs(predict_ols - out_mod) / abs(out_mod)*100)
  errorstats[1,3] <-  mean(abs(predict_ols - out_mod))
  errorstats[1,4] <-  mean((predict_ols - out_mod)^2)
  errorstats[1,5] <-  mean( sqrt((predict_ols - out_mod)^2)/(yrange) )
  error_matrix[,1] <- abs((predict_ols - out_mod)/ abs(out_mod)*100)
  
  OLS_model2 <- lm(y_stand~.,data=H_df)
  predict_ols2 <- predict(OLS_model2,H_pred_df)*sd(out_mod) + mean(out_mod)
  
  #OLS_model2 <- lm(out_mod~.,data=H)
  #predict_ols2 <- predict(OLS_model2,H_pred)
  
  #OLS_model2 <- lm(y_stand~.,data=H_df)
  #predict_ols2 <- predict(OLS_model2,H_pred_df)*sd(outmod)mean(out_mod)
  
  errorstats[1,6]  <- mean(predict_ols2)
  errorstats[1,7] <-  mean(abs(predict_ols2 - out_mod_pred) / abs(out_mod_pred)*100)
  errorstats[1,8] <-  mean(abs(predict_ols2 - out_mod_pred))
  errorstats[1,9] <-  mean((predict_ols2 - out_mod_pred)^2)
  errorstats[1,10] <-  mean( sqrt((predict_ols2 -out_mod_pred)^2)/(yrange2) )
  
 #random forest emulator 
  if (fitRandomForest) {
    
    predicted_rf <- matrix(0,n_expdes,length(vars))
    for (cv in 1: number_cv) {
      a<-(1 + cvfold*(cv-1)) ; b<-(cvfold + cvfold*(cv-1))
      for (v in 1:length(vars)) { 
        ranger_model <- ranger(y_stand[-cvobs[a:b]]~.,H_df[-cvobs[a:b],],num.trees=ntrees,mtry=vars[v])
        predicted_rf[cvobs[a:b],v]<- predict(ranger_model,H_df[cvobs[a:b],])$predictions*sd(out_mod) + mean(out_mod) 
      }
    }
    mean_error_rf <- matrix(0,length(vars),1)
    for(k in 1:length(vars)) {
      mean_error_rf[k] <- mean( sqrt((predicted_rf[,k] - out_mod)^2)/(yrange) )  
    }
    
    index <- which(mean_error_rf==min(mean_error_rf))
    modelID <-10
    errorstats[modelID,1]  <- mean(predicted_rf[,index])
    errorstats[modelID,2] <-  mean(abs(predicted_rf[,index]- out_mod) / abs(out_mod) *100)
    errorstats[modelID,3] <-  mean(abs(predicted_rf[,index] - out_mod))
    errorstats[modelID,4] <-  mean((predicted_rf[,index]- out_mod)^2)
    errorstats[modelID,5] <-  mean( sqrt((predicted_rf[,index] - out_mod)^2)/(yrange) )
    
    
    ranger_model2 <- ranger(y_stand~.,H_df,num.trees=ntrees,mtry=vars[index])
    predict_rf2<- predict(ranger_model2,H_pred_df)$predictions*sd(out_mod) + mean(out_mod) 
    
    errorstats[modelID,6]  <- mean(predict_rf2)
    errorstats[modelID,7] <-  mean(abs(predict_rf2- out_mod_pred) / abs(out_mod_pred) *100)
    errorstats[modelID,8] <-  mean(abs(predict_rf2 -out_mod_pred))
    errorstats[modelID,9] <-  mean((predict_rf2 - out_mod_pred)^2)
    errorstats[modelID,10] <-  mean( sqrt((predict_rf2 - out_mod_pred)^2)/(yrange2) )

    #hyperp
    errorstats[modelID,11] <- mean_error_rf[1] ;  errorstats[modelID,12] <- mean_error_rf[2]
    errorstats[modelID,13] <- mean_error_rf[3] ;  errorstats[modelID,14] <- mean_error_rf[4]
    error_matrix[,10] <- abs((predicted_rf[,index] - out_mod)/ abs(out_mod)*100)
    
  }  #  ********************************************************************************************
  
 
print(errorstats)  
  # Gaussian Process (1-parameter)    ******************************************************
  if (fitGP) {
    
    predict_GP <-matrix(0,n_expdes,length(kernels)) 
  
    for (cv in 1: number_cv) {
      a<-(1 + cvfold*(cv-1)) ; b<-(cvfold + cvfold*(cv-1))
      for(k in 1:length(kernels)) {
        gp_model <- gausspr(H_df[-cvobs[a:b],],y_stand[-cvobs[a:b]],scaled=FALSE,kernel=kernels[k])   
        predict_GP[cvobs[a:b],k] <-  predict(gp_model,H_df[cvobs[a:b],])*sd(out_mod) + mean(out_mod)
      }
    } 
    
    mean_error_gp <-matrix(0,length(kernels),1)
    for(k in 1:length(kernels)) {
      mean_error_gp[k] <- mean( sqrt((predict_GP[,k] - out_mod)^2)/(yrange) ) 
    }
    index <- which(mean_error_gp==min(mean_error_gp)) 
    errorstats[2,1]  <- mean(predict_GP[,index])
    errorstats[2,2] <-  mean(abs(predict_GP[,index] - out_mod) / abs(out_mod) *100)
    errorstats[2,3] <-  mean(abs(predict_GP[,index] - out_mod))
    errorstats[2,4] <-  mean((predict_GP[,index] - out_mod)^2)
    errorstats[2,5] <-  mean( sqrt((predict_GP[,index] - out_mod)^2)/(yrange) )
    #error_matrix[,2] <- abs((predict_GP[,index] - out_mod)/ abs(out_mod)*100)
    
    gp_model12 <- gausspr(H_df,y_stand,scaled=FALSE,kernel=kernels[index])   
    predict_GP12 <-  predict(gp_model12,H_pred_df)*sd(out_mod) + mean(out_mod)
    
    errorstats[2,11] <-mean_error_gp[1]
    errorstats[2,12] <-mean_error_gp[2]
    errorstats[2,13] <-mean_error_gp[3]
    errorstats[2,14] <-mean_error_gp[4]
    errorstats[2,15] <-mean_error_gp[5]
    modelID <-2
    errorstats[modelID,6]  <- mean(predict_GP12)
    errorstats[modelID,7] <-  mean(abs(predict_GP12- out_mod_pred) / abs(out_mod_pred) *100)
    errorstats[modelID,8] <-  mean(abs(predict_GP12 -out_mod_pred))
    errorstats[modelID,9] <-  mean((predict_GP12 - out_mod_pred)^2)
    errorstats[modelID,10] <-  mean( sqrt((predict_GP12 - out_mod_pred)^2)/(yrange2) )

  }
  
  # bayesian GP                   *******************************************
  
  if(fitBayesGP) { 
 
    #no crosssvalidation
    gp_model22 <- bgp(X=Z,y_stand,XX=H_pred_df[1:1000,], meanfn = "linear", bprior = "bflat", corr = "expsep", BTE = c(1000, 4000,2))  
    predict_GP22 <-  predict(gp_model22,H_pred_df)$ZZ.mean *sd(out_mod) + mean(out_mod)
    
    modelID <-3
    errorstats[modelID,6]  <- mean(predict_GP22)
    errorstats[modelID,7] <-  mean(abs(predict_GP22- out_mod_pred) / abs(out_mod_pred) *100)
    errorstats[modelID,8] <-  mean(abs(predict_GP22 -out_mod_pred))
    errorstats[modelID,9] <-  mean((predict_GP22 - out_mod_pred)^2)
    errorstats[modelID,10] <-  mean( sqrt((predict_GP22 - out_mod_pred)^2)/(yrange2) )
    
    
  }  
  
print(errorstats)  
 # classic GP  (with nugget term estimation)               *********************
  
  if(fitGP_fit) {  
    
    gp_model32 <- GP_fit(X=Z,Y=y_stand,control = c(100*nparams, 40* nparams, 2* nparams),maxit=50,trace=TRUE) 
    predict_GP32 <-  predict(gp_model32,H_pred_df)$Y_hat*sd(out_mod) + mean(out_mod)
    
    modelID <-12
    errorstats[modelID,6]  <- mean(predict_GP32)
    errorstats[modelID,7] <-  mean(abs(predict_GP32- out_mod_pred) / abs(out_mod_pred) *100)
    errorstats[modelID,8] <-  mean(abs(predict_GP32 -out_mod_pred))
    errorstats[modelID,9] <-  mean((predict_GP32 - out_mod_pred)^2)
    errorstats[modelID,10] <-  mean( sqrt((predict_GP32 - out_mod_pred)^2)/(yrange2) )
    
  }
  
print(errorstats)  
  # ANN                      ************************************************
  
  if (fitNN) {  
    
    predict_nn_e <- matrix(0,n_expdes,length(architectures))
    for(k in 1:length(architectures)) {
      for (cv in 1:number_cv) {
        a<-(1 + cvfold*(cv-1)) ; b<-(cvfold + cvfold*(cv-1))
        nn_model  <- nnet(Z[-cvobs[a:b],],y_stand[-cvobs[a:b]],maxit=maxiter_nn,Wts=rep(initial.weight[k],i.weights[k]),size=architectures[k],linout=TRUE,abstol = 1.0e-4,reltol=1.0e-8)
        predict_nn_e[cvobs[a:b],k]<- predict(nn_model,H_df[cvobs[a:b],])*sd(out_mod) + mean(out_mod)
      } 
    }   
    mean_error_nn <-matrix(0,length(architectures),1)
    for(k in 1:length(architectures)) {
      mean_error_nn[k] <- mean(sqrt((predict_nn_e[,k] - out_mod)^2)/(yrange) ) 
    }
    index <- which(mean_error_nn==min(mean_error_nn))
    if(length(index)>1) { index <- index[1]} 
    
    errorstats[5,1]  <- mean(predict_nn_e[,index])
    errorstats[5,2] <-  mean(abs(predict_nn_e[,index] - out_mod) / abs(out_mod) *100)
    errorstats[5,3] <-  mean(abs(predict_nn_e[,index] - out_mod))
    errorstats[5,4] <-  mean((predict_nn_e[,index]- out_mod)^2)
    errorstats[5,5] <-  mean( sqrt((predict_nn_e[,index] - out_mod)^2)/(yrange) )
    error_matrix[,5] <- abs((predict_nn_e[,index] - out_mod)/ abs(out_mod)*100)
    
    errorstats[5,11] <- mean_error_nn[1] ;     errorstats[5,12] <- mean_error_nn[2]
    errorstats[5,13] <- mean_error_nn[3] ;     errorstats[5,14] <- mean_error_nn[4]
    errorstats[5,15] <- mean_error_nn[5] ;     errorstats[5,16] <-  mean_error_nn[6]
    
    nn_model1  <- nnet(Z,y_stand,maxit=maxiter_nn,Wts=rep(initial.weight[index],i.weights[index]),size=architectures[index],linout=TRUE,abstol = 1.0e-4,reltol=1.0e-8)
    predict_nn<- predict(nn_model1,H_pred_df)*sd(out_mod) + mean(out_mod)
    
    modelID <-5
    errorstats[modelID,6]  <- mean(predict_nn)
    errorstats[modelID,7] <-  mean(abs(predict_nn- out_mod_pred) / abs(out_mod_pred) *100)
    errorstats[modelID,8] <-  mean(abs(predict_nn -out_mod_pred))
    errorstats[modelID,9] <-  mean((predict_nn - out_mod_pred)^2)
    errorstats[modelID,10] <-  mean( sqrt((predict_nn - out_mod_pred)^2)/yrange2 )
    
    
    predict_nn2_e <- matrix(0,n_expdes,length(architectures))
    #maxiter_nn
    for(k in 1:length(architectures)) {
      for (cv in 1: number_cv) {
        a<-(1 + cvfold*(cv-1)) ; b<-(cvfold + cvfold*(cv-1))
        nn_model  <- neuralnet(y_stand[-cvobs[a:b]]~.,data=H_df[-cvobs[a:b],],stepmax=maxiter_nn,rep=1, startweights=rep(initial.weight[k],times=i.weights[k]), hidden=architectures[k] ,linear.output=TRUE,threshold=0.01,algorithm="rprop+")
        predict_nn2_e[cvobs[a:b],k] <-tryCatch(predict(nn_model,H_df[cvobs[a:b],]),error=function(e) 100)*sd(out_mod) + mean(out_mod)
      } 
    }   
    
    mean_error_nn2 <-matrix(0,length(architectures),1)
    for(k in 1:length(architectures)) {
      mean_error_nn2[k] <- mean( sqrt((predict_nn2_e[,k] - out_mod)^2)/(yrange) )
    }
    index <- which(mean_error_nn2==min(mean_error_nn2)) 
    
    errorstats[6,1]  <- mean(predict_nn2_e[,index])
    errorstats[6,2] <-  mean(abs(predict_nn2_e[,index] - out_mod) / out_mod *100)
    errorstats[6,3] <-  mean(abs(predict_nn2_e[,index] - out_mod))
    errorstats[6,4] <-  mean((predict_nn2_e[,index] - out_mod)^2)
    errorstats[6,5] <-  mean( sqrt((predict_nn2_e[,index] - out_mod)^2)/(yrange) )
    error_matrix[,6] <- abs((predict_nn2_e[,index] - out_mod)/ out_mod*100)
    
    errorstats[6,11] <- mean_error_nn2[1] ;     errorstats[6,12] <- mean_error_nn2[2]
    errorstats[6,13] <- mean_error_nn2[3] ;     errorstats[6,14] <- mean_error_nn2[4]
    errorstats[6,15] <- mean_error_nn2[5] ;     errorstats[6,16] <-  mean_error_nn2[6]
    
    nn_model  <- neuralnet(y_stand~.,data=H_df,stepmax=maxiter_nn,rep=1, startweights=rep(initial.weight[index],times=i.weights[index]), hidden=architectures[index] ,linear.output=TRUE,threshold=0.01,algorithm="rprop+")
    predict_nn2<-tryCatch(predict(nn_model,H_pred_df),error=function(e) 100)*sd(out_mod) + mean(out_mod)
    
    modelID <-6
    errorstats[modelID,6]  <- mean(predict_nn2)
    errorstats[modelID,7] <-  mean(abs(predict_nn2- out_mod_pred) / abs(out_mod_pred) *100)
    errorstats[modelID,8] <-  mean(abs(predict_nn2 -out_mod_pred))
    errorstats[modelID,9] <-  mean((predict_nn2 - out_mod_pred)^2)
    errorstats[modelID,10] <-  mean( sqrt((predict_nn2 - out_mod_pred)^2)/(yrange2) )
    
    
  }  # END      fitNN      *************************************************************************
  
print(errorstats) 

 #  decision trees boosting  **************************************************
  
  
   if(fitboost) {
    predict_gbm <- matrix(0,n_expdes,length(eta)) 
    for (cv in 1: number_cv) {
      a<-(1 + cvfold*(cv-1)) ; b<-(cvfold + cvfold*(cv-1))
      for(k in 1:length(eta)) {
        gbm_model  <- gbm(y_stand[-cvobs[a:b]]~.,data=H_df[-cvobs[a:b],],distribution="gaussian",n.trees=ntrees,interaction.depth =2,bag.fraction =  subsample[k] ,shrinkage= eta[k])
        predict_gbm[cvobs[a:b],k] <- predict(gbm_model,H_df[cvobs[a:b],],n.trees=ntrees)*sd(out_mod) + mean(out_mod)
      }
    } 
    
    mean_error_gbm <-matrix(0,length(eta),1)
    for(k in 1:length(eta)) {
      mean_error_gbm[k] <- mean( sqrt((predict_gbm[,k] - out_mod)^2)/(yrange) ) 
    }
    index <- which(mean_error_gbm==min(mean_error_gbm)) 
    if(length(index)>1) { index <- index[1]}
    
    modelID <- 11
    errorstats[modelID,1]  <- mean(predict_gbm[,index])
    errorstats[modelID,2] <-  mean(abs(predict_gbm[,index] - out_mod) / abs(out_mod) *100)
    errorstats[modelID,3] <-  mean(abs(predict_gbm[,index] - out_mod))
    errorstats[modelID,4] <-  mean((predict_gbm[,index]- out_mod)^2)
    errorstats[modelID,5] <-  mean( sqrt((predict_gbm[,index] - out_mod)^2)/(yrange) )
    error_matrix[,modelID] <- abs((predict_gbm[,index] - out_mod)/ abs(out_mod)*100)
    errorstats[modelID,11] <- mean_error_gbm[1] ;   errorstats[modelID,12] <- mean_error_gbm[2]
    errorstats[modelID,13] <- mean_error_gbm[3] ;   errorstats[modelID,14] <- mean_error_gbm[4]
    errorstats[modelID,15] <- mean_error_gbm[5] ;  errorstats[modelID,16] <-  mean_error_gbm[6]
    
    gbm_model2  <- gbm(y_stand~.,data=H_df,distribution="gaussian",n.trees=ntrees,interaction.depth =2,bag.fraction =  subsample[index] ,shrinkage= eta[index])
    predict_gbm2 <- predict(gbm_model2,H_pred_df,n.trees=ntrees)*sd(out_mod) + mean(out_mod)
    
    errorstats[modelID,6]  <- mean(predict_gbm2)
    errorstats[modelID,7] <-  mean(abs(predict_gbm2- out_mod_pred) / abs(out_mod_pred) *100)
    errorstats[modelID,8] <-  mean(abs(predict_gbm2 -out_mod_pred))
    errorstats[modelID,9] <-  mean((predict_gbm2 - out_mod_pred)^2)
    errorstats[modelID,10] <-  mean( sqrt((predict_gbm2 - out_mod_pred)^2)/(yrange2) )
    
   }
  
  
  #  Generalized Additive Models estimation via boosting          **************
  
  
  if(fitmboost) {
    ddata <- data.frame(y_stand,Z)
    dddata <-data.frame(out_mod_pred,Z_pred)
    predict_mb <- matrix(0,n_expdes,length(shrink))
    for (cv in c(1:number_cv) ) {  
      a<-(1 + cvfold*(cv-1)) ; b<-(cvfold + cvfold*(cv-1))
      for (k in c(1:length(shrink)) )  {
        mboost_model <- mboost(formulamboost,data=ddata[-cvobs[a:b],],family=Gaussian(),baselearner=("bbs"),control=boost_control(mstop=stop[k],nu= shrink[k] ,center=FALSE))
        predict_mb[cvobs[a:b],k] <- predict(mboost_model,ddata[cvobs[a:b],])*sd(out_mod)+mean(out_mod) 
      }
     }
  
    mean_error_mb <-matrix(0,length(shrink),1)  
    for(k in 1:length(shrink)) {
      mean_error_mb[k] <- mean( sqrt((predict_mb[,k] - out_mod)^2)/(yrange) )
    } 
    index <- which( mean_error_mb==min(mean_error_mb)) 
    if(length(index)>1) { index <- index[1]}
    modelID <- 9
    errorstats[modelID,1]  <- mean(predict_mb[,index])
    errorstats[modelID,2] <-  mean(abs(predict_mb[,index] - out_mod) / abs(out_mod) *100)
    errorstats[modelID,3] <-  mean(abs(predict_mb[,index] - out_mod))
    errorstats[modelID,4] <-  mean((predict_mb[,index] - out_mod)^2)
    errorstats[modelID,5] <-  mean( sqrt((predict_mb[,index] - out_mod)^2)/(yrange) )
    error_matrix[,modelID] <- abs((predict_mb[,index] - out_mod)/ abs(out_mod)*100)
    
    errorstats[modelID,11] <- mean_error_mb[1] ;   errorstats[modelID,12] <- mean_error_mb[2]
    errorstats[modelID,13] <- mean_error_mb[3] ;   errorstats[modelID,14] <- mean_error_mb[4]
    errorstats[modelID,15] <- mean_error_mb[5] ;   errorstats[modelID,16] <-  mean_error_mb[6]
    
    mboost_model2 <- mboost(formulamboost,data=ddata,family=Gaussian(),baselearner=("bbs"),control=boost_control(mstop=stop[index],nu= shrink[index] ,center=FALSE))
    predict_mb2 <- predict(mboost_model2,dddata)*sd(out_mod) + mean(out_mod) 

    errorstats[modelID,6]  <- mean(predict_mb2)
    errorstats[modelID,7] <-  mean(abs(predict_mb2- out_mod_pred) / abs(out_mod_pred) *100)
    errorstats[modelID,8] <-  mean(abs(predict_mb2 -out_mod_pred))
    errorstats[modelID,9] <-  mean((predict_mb2 - out_mod_pred)^2)
    errorstats[modelID,10] <-  mean( sqrt((predict_mb2 - out_mod_pred)^2)/(yrange2) )
    ddata<-NULL
    dddata<-NULL
  }
  
  
  if (fitGAM) {
    predict_gam <- matrix(0,n_expdes,6) 
    predict_mgcv <- matrix(0,n_expdes,6) 
    
    for (cv in c(1:number_cv) ) { 
      a<-(1 + cvfold*(cv-1)) ; b<-(cvfold + cvfold*(cv-1))
      gam_model1 <- gam::gam(formulagam,data=H_df[-cvobs[a:b],],family="gaussian")
      gam_model2 <- gam::gam(formulagam2,data=H_df[-cvobs[a:b],],family="gaussian")
      gam_model3 <- gam::gam(formulagam3,data=H_df[-cvobs[a:b],],family="gaussian")
      gam_model4 <- gam::gam(formulagam5,data=H_df[-cvobs[a:b],],family="gaussian")
      
      gam_model21 <- mgcv::gam(formulagam_mgcv1,data=H_df[-cvobs[a:b],],method="REML",select=TRUE)
      gam_model31 <- mgcv::gam(formulagam_mgcv1,data=H_df[-cvobs[a:b],],method="REML",select=FALSE)
      gam_model41 <- mgcv::gam(formulagam_mgcv3,data=H_df[-cvobs[a:b],],method="REML",select=FALSE)
      gam_model51 <- mgcv::gam(formulagam_mgcv3,data=H_df[-cvobs[a:b],],method="REML",select=TRUE)
      
      predict_gam[cvobs[a:b],1] <-predict(gam_model1,H_df[cvobs[a:b],])*sd(out_mod) + mean(out_mod)
      predict_gam[cvobs[a:b],2] <-predict(gam_model2,H_df[cvobs[a:b],])*sd(out_mod) + mean(out_mod)
      predict_gam[cvobs[a:b],3] <-predict(gam_model3,H_df[cvobs[a:b],])*sd(out_mod) + mean(out_mod)
      predict_gam[cvobs[a:b],4] <-predict(gam_model4,H_df[cvobs[a:b],])*sd(out_mod) + mean(out_mod)
      
      predict_mgcv[cvobs[a:b],1] <-predict(gam_model21,H_df[cvobs[a:b],])*sd(out_mod) + mean(out_mod)
      predict_mgcv[cvobs[a:b],2] <-predict(gam_model31,H_df[cvobs[a:b],])*sd(out_mod) + mean(out_mod)
      predict_mgcv[cvobs[a:b],3] <-predict(gam_model41,H_df[cvobs[a:b],])*sd(out_mod) + mean(out_mod)
      predict_mgcv[cvobs[a:b],4] <-predict(gam_model51,H_df[cvobs[a:b],])*sd(out_mod) + mean(out_mod)
    }
    
    
    
    mean_error_gam <-matrix(0,length(predict_gam[1,]),1)  
    for(k in 1:length(predict_gam[1,])) {
      mean_error_gam[k] <-  mean( sqrt((predict_gam[,k] - out_mod)^2)/(yrange) ) 
    } 
    index <- which(mean_error_gam==min(mean_error_gam)) 
    if(length(index)>1) { index <- index[1]}
    
    modelID <-7
    errorstats[modelID,1]  <- mean(predict_gam[,index])
    errorstats[modelID,2] <-  mean(abs(predict_gam[,index] - out_mod) / abs(out_mod) *100)
    errorstats[modelID,3] <-  mean(abs(predict_gam[,index] - out_mod))
    errorstats[modelID,4] <-  mean((predict_gam[,index] - out_mod)^2)
    errorstats[modelID,5] <-  mean( sqrt((predict_gam[,index] - out_mod)^2)/(yrange) )
    error_matrix[,modelID] <- abs((predict_gam[,index] - out_mod)/ abs(out_mod)*100)
    
    errorstats[modelID,11] <- mean_error_gam[1] ;   errorstats[modelID,12] <- mean_error_gam[2]
    errorstats[modelID,13] <- mean_error_gam[3] ;   errorstats[modelID,14] <- mean_error_gam[4]
    #errorstats[modelID,10] <- mean_error_gam[5] ;   errorstats[modelID,11] <-  mean_error_gam[6]
    
    if(index==1) { formulagam111 <-formulagam11  }
    if(index==2) { formulagam111 <-formulagam12  }
    if(index==3) { formulagam111 <-formulagam13  }
    if(index==4) { formulagam111 <-formulagam15  }
    
    gam_model11 <- gam::gam(formulagam111,data=H_df,family="gaussian")
    predict_gam11 <-predict(gam_model11,H_pred_df)*sd(out_mod) + mean(out_mod)
    
    errorstats[modelID,6]  <- mean(predict_gam11)
    errorstats[modelID,7] <-  mean(abs(predict_gam11- out_mod_pred) / abs(out_mod_pred) *100)
    errorstats[modelID,8] <-  mean(abs(predict_gam11 -out_mod_pred))
    errorstats[modelID,9] <-  mean((predict_gam11 - out_mod_pred)^2)
    errorstats[modelID,10] <-  mean( sqrt((predict_gam11 - out_mod_pred)^2)/(yrange2) )
    
    
    mean_error_mgcv <-matrix(0,length(predict_gam2[1,]),1)  
    for(k in 1:length(predict_mgcv[1,])) {
      mean_error_mgcv[k] <-  mean( sqrt((predict_mgcv[,k] - out_mod)^2)/(yrange) ) 
    } 
    index <- which(mean_error_mgcv==min(mean_error_mgcv)) 
    if(length(index)>1) { index <- index[1]}
    modelID <-8
    errorstats[modelID,1]  <- mean(predict_mgcv[,index])
    errorstats[modelID,2] <-  mean(abs(predict_mgcv[,index] - out_mod) / abs(out_mod) *100)
    errorstats[modelID,3] <-  mean(abs(predict_mgcv[,index] - out_mod))
    errorstats[modelID,4] <-  mean((predict_mgcv[,index] - out_mod)^2)
    errorstats[modelID,5] <-  mean( sqrt((predict_mgcv[,index] - out_mod)^2)/(yrange) )
    error_matrix[,modelID] <- abs((predict_mgcv[,index] - out_mod)/ abs(out_mod)*100)
    
    errorstats[modelID,11] <- mean_error_mgcv[1] ;   errorstats[modelID,12] <- mean_error_mgcv[2]
    errorstats[modelID,13] <- mean_error_mgcv[3] ;   errorstats[modelID,14] <- mean_error_mgcv[4]
    #errorstats[modelID,10] <- mean_error_mgcv[5] ;   errorstats[modelID,11] <-  mean_error_mgcv[6]
    
    if(index==1 | index==2) { formulagam_mgcv22 <-formulagam_mgcv11  }
    if(index==3| index==4 ) { formulagam_mgcv22 <-formulagam_mgcv13   }
    if(index==1 | index==4) { penalizmethod <- TRUE  }
    if(index==2 | index==3) { penalizmethod <- FALSE   }
    
    gam_model22 <- mgcv::gam(formulagam_mgcv22,data=H_df,method="REML",select=penalizmethod )
    predict_mgcv2 <-predict(gam_model22,H_pred_df)*sd(out_mod) + mean(out_mod)
    
    
    errorstats[modelID,6]  <- mean(predict_mgcv2)
    errorstats[modelID,7] <-  mean(abs(predict_mgcv2- out_mod_pred) / abs(out_mod_pred) *100)
    errorstats[modelID,8] <-  mean(abs(predict_mgcv2 -out_mod_pred))
    errorstats[modelID,9] <-  mean((predict_mgcv2 - out_mod_pred)^2)
    errorstats[modelID,10] <-  mean( sqrt((predict_mgcv2 - out_mod_pred)^2)/(yrange2) )
    
    
  }


if(fitBN) {  
  predict_BN <-matrix(0,n_expdes,3) 
  for (cv in 1: number_cv) {
    a<-(1 + cvfold*(cv-1)) ; b<-(cvfold + cvfold*(cv-1))
    W <- cbind.data.frame(H_df[-cvobs[a:b],],y=y_stand[-cvobs[a:b]])
    
    # Note: this algorithm is based on multiple significance tests
    # sometimes, one arc is undirected , to solve this we make the 
    # criteria for selection stricter by changing alpha (significance level)
    baynetwork1 <- pc.stable(W, cluster = NULL, whitelist = NULL, blacklist = NULL, test = NULL,
                             alpha = 0.05, B =NULL, max.sx = NULL, debug = FALSE, undirected = FALSE)
    
    bnmodel <- tryCatch(bn.fit(baynetwork1,data=W),
                        error=function(e){    undirected <- "undirected" } )
    
    #if an error occured (ie, if fitted DAG has at least one undirected arc)
    if(class(bnmodel)=="character") {
      
      l <- length(baynetwork1$arcs[,1])
      arcs.to.test <- baynetwork1$arcs[baynetwork1$arcs[,2]!="y"]
      unique.nodes <- unique(arcs.to.test)
      baynetwork1<- set.arc(baynetwork1, from = unique.nodes[1], to =unique.nodes[2])
      bnmodel <-tryCatch(bn.fit(baynetwork1,data=W),
                         error=function(e){    undirected <- "undirected" } )
    }
    
    baynetwork2 <- hc(W)        
    baynetwork3 <-rsmax2(W)
    
    bnmodel2 <- bn.fit(baynetwork2,data=W) 
    bnmodel3 <- bn.fit(baynetwork3,data=W) 
    if(!class(bnmodel)=="character") { predict_BN[cvobs[a:b],1] <-  predict(bnmodel,H_df[cvobs[a:b],],node="y") *sd(out_mod) + mean(out_mod) }
    if(class(bnmodel)=="character") { predict_BN[cvobs[a:b],1] <- 999 }
    predict_BN[cvobs[a:b],2] <-  predict(bnmodel2,H_df[cvobs[a:b],],node="y") *sd(out_mod) + mean(out_mod)
    predict_BN[cvobs[a:b],3] <-  predict(bnmodel3,H_df[cvobs[a:b],],node="y") *sd(out_mod) + mean(out_mod)
  } 
  
  mean_error_bn <-matrix(0,length(predict_BN[1,]),1)  
  for(k in 1:length(predict_BN[1,])) {
    mean_error_bn[k] <-  mean( sqrt((predict_BN[,k] - out_mod)^2)/(yrange) ) 
  }
  
  index <- which(mean_error_bn==min(mean_error_bn)) 
  if(length(index)>1) { index <- index[1]}
  

  
  errorstats[12,1]  <- mean( predict_BN[,index])
  errorstats[12,2] <-  mean(abs( predict_BN[,index]- out_mod) / out_mod *100)
  errorstats[12,3] <-  mean(abs( predict_BN[,index]- out_mod))
  errorstats[12,4] <-  mean(( predict_BN[,index] - out_mod)^2)
  errorstats[12,5] <-  mean( sqrt(( predict_BN[,index] - out_mod)^2)/(yrange) )
  errorstats[12,11] <- mean_error_bn[1]
  errorstats[12,12] <- mean_error_bn[2] 
  errorstats[12,13] <- mean_error_bn[3]
  error_matrix[,12] <- abs(( predict_BN[,index]- out_mod)/ out_mod*100)
  
  W2 <-  cbind.data.frame(H_df,y=y_stand)
  
  if(index==1) {
    bnmodel11 <- bn.fit(baynetwork1,data=W2) 
    predict_BN2 <- predict(bnmodel11,H_pred_df,node="y")*sd(out_mod) + mean(out_mod)   }
  if(index==2) {
    bnmodel22 <- bn.fit(baynetwork2,data=W2) 
    predict_BN2 <- predict(bnmodel22,H_pred_df,node="y")*sd(out_mod) + mean(out_mod)   }
  if(index==3) {
    bnmodel33 <- bn.fit(baynetwork3,data=W2)
    predict_BN2 <- predict(bnmodel33,H_pred_df,node="y")*sd(out_mod) + mean(out_mod)   }
  
  errorstats[12,6]  <- mean(predict_BN2)
  errorstats[12,7] <-  mean(abs(predict_BN2 - out_mod_pred) / abs(out_mod_pred)*100)
  errorstats[12,8] <-  mean(abs(predict_BN2 - out_mod_pred))
  errorstats[12,9] <-  mean((predict_BN2 - out_mod_pred)^2)
  errorstats[12,10] <-  mean( sqrt((predict_BN2 -out_mod_pred)^2)/(yrange2) )
  
  
}

  
  # OUTPUT ###################################################################
  
if(!testing) {
  out_mod <-data.frame(out_mod) ; errorstats <- data.frame(errorstats)
  
  #predictions<-data.frame(out_mod_pred,predict_ols2,predict_GP12,predict_GP22,predict_GP32,predict_nn,predict_nn2,predict_gam11,predict_mgcv2,predict_mb2,predict_gbm2,predict_rf2)
  predictions <- data.frame(out_mod_pred, predict_GP22)
  
  listoutputs <-list("expdes"=H,"y1"=out_mod,"outcome1"=errorstats,"Predictions"=predictions)
  runname <-paste("PSAdata_outcome",modeloutcomes,"nexpdes",n_expdes,"vbnlearn.xlsx",sep="_")
  filename <- paste(filedir,runname,sep=separ)
  openxlsx::write.xlsx(listoutputs,filename,asTable =TRUE,colnames=TRUE,colWidths=12)
}