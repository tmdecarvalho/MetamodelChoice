
#####################  Author : Tiago M. de Carvalho  22 05 2016  ########

#version 1: De Carvalho et al 2019 Med Dec Mak, own implementation of GP regression
#version 2: several emulators/metamodels are implemented based on built-in R packages.


# MEMORY management ************************************************************************************************************
#ls()
#gc()
#rm(list=ls())
options(digits=3) # print digits


# USES PACKAGES  ############################################

#library(microbenchmark) # check computation time


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

#general
#library(caret) # machine learning : 238 models available
library(bnlearn)


# INPUT         ############################################


#standardize parameters and out_mod                       ***************************************

standardize <-TRUE  # standardize model parameters and outcomes
#workstation <- "cloud" #"work" #  "laptop"#  # choose a directory to load/save files
saveresults <-TRUE  # save results?


# emulator options: hyperparameters for each metamodel    ***************************************

# models used for main results

# note: maximum number of parametrizations used is 6 (but for some models, there 
# are many other combinations possible).

# isotropic GP
# kernels list : "rbfdot","polydot","vanilladot","tanhdot","laplacedot","besseldot", "anovadot", "splinedot"
kernels<-c("rbfdot","laplacedot","besseldot", "anovadot", "splinedot") #

#NN
# single layer: nnet package
# architecture: number of nodes for the hidden layer
architectures <- c(nparams,round(nparams/1.5),round(nparams/3),nparams,round(nparams/1.5),round(nparams/3))
# value for initial weight vector (note: this is the same value for every weight!)
initial.weight <- c(0.1,0.1,0.1,0.01,0.01,0.01)  
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

allp <- c(1:nparams)
formulamboost <- as.formula(paste0("y_stand~",paste0("X",allp,collapse="+"),collapse=""))
#formulamboost <- as.formula(paste0("y_stand[-cvobs[a:b]]~",paste0("X",allp,collapse="+"),collapse=""))


# GAM : other models
#note: # s() = penalized spline, ti/te/t2()= tensor product various specs.

# for gam package

formulagam <-as.formula(paste0("y_stand[-cvobs[a:b]]~",paste0("s(V",allp,")",collapse="+"),collapse=""))
formulagam2<-as.formula(paste0("y_stand[-cvobs[a:b]]~",paste0("s(V",allp,",2)",collapse="+"),collapse=""))
formulagam3<-as.formula(paste0("y_stand[-cvobs[a:b]]~",paste0("s(V",allp,",6)",collapse="+"),collapse=""))
formulagam5<-as.formula(paste0("y_stand[-cvobs[a:b]]~",paste0("s(V",allp,",8)",collapse="+"),collapse=""))

# for mgcv package

# note : i include some parameters only as a linear term in order not to exceed degrees of freedom
# if the size of training data changes this might need to be adjusted. 
spline <- c(1:((n_expdes/10)-1))  
linear <- c((n_expdes/10):nparams)

formulagam_mgcv1 <-as.formula(paste0("y_stand[-cvobs[a:b]]~",paste0("s(V",spline,")",collapse="+"),"+",paste0("V",linear,collapse="+"),collapse=""))
formulagam_mgcv3 <-as.formula(paste0("y_stand[-cvobs[a:b]]~",paste0("s(V",allp,",k=4)",collapse="+"))) # min(k)==3
formulagam_mgcv5 <-as.formula(paste0("y_stand[-cvobs[a:b]]~",paste0("s(V",allp,")",collapse="+")))

#                                                                  ***************************************
#decision trees
#random forest parameters
ntrees <- 2500 ;  vars <- c(nparams, round(nparams/1.5),round(nparams/2),round(nparams/3))  #round(sqrt(nparams))
#boosting 
eta <- c(0.01,0.05,0.25,0.01,0.05,0.25)
subsample <- c(0.6,0.6,0.6,0.8,0.8,0.8)


#read outcomes and experimental design (ie model inputs) 
expdes_train <- exp_des

#matrix to store outcomes to be modelled
modeloutcomes <- length(modeloutcome)
out_train <- matrix(0,length(expdes_train[,1]),modeloutcomes)

#loop to select correct model outcomes to be modelled
for (i in 1:modeloutcomes) {
  if(modeloutcome[i]<11)  { out_train[,i] <- as.numeric(outcomes[,modeloutcome[i]]) }
  if(modeloutcome[i]>=11) { out_train[,i] <- as.numeric(outcomes[,modeloutcome[i]-10] - outcomes[,modeloutcome[i]-5]) }
}
# write rest of loop for diff outcomes (treatmt no treatmt)


# loop for the different model outcomes 


for (out in c(1:modeloutcomes)) {
  
  
  #EMULATORS*******************************************************************
  
  H <- matrix(0, n_expdes ,nparams) ; Z <- matrix(0, n_expdes ,nparams)
  H <- expdes_train 
  out_mod <- out_train[,out]
  
  #error stats collected
  errorstats <- matrix(0,length(emulatornames),length(accuracynames))
  colnames(errorstats) <- accuracynames
  rownames(errorstats) <- emulatornames
  # add error distribution for each emulator
  error_matrix <- matrix(0,length(out_mod),length(emulatornames))
  
  if (standardize) {
    y_stand <- (out_mod - mean(out_mod))/sd(out_mod)
    for (i in c(1:nparams)) { Z[,i] <- (H[,i] - mean(H[,i]))/sd(H[,i]) }
    # toPred2[,i] <- (toPred[,i] - mean(H[,i]))/sd(H[,i])
  }
  
  # standardize
  H_df <- as.data.frame(Z)
  
  #auxiliary variables for cross-validation
  number_cv <- round(n_expdes/cvfold) 
  cvobs <- c(1:n_expdes)
  
  yrange <-max(out_mod)-min(out_mod)
  
  # OLS
  predict_ols <- matrix(0,n_expdes,1)
  for (cv in 1: number_cv) {
    a<-(1 + cvfold*(cv-1)) ; b<-(cvfold + cvfold*(cv-1)) 
    OLS_model <- lm(y_stand[-cvobs[a:b]]~.,data=H_df[-cvobs[a:b],])
    predict_ols[cvobs[a:b]] <-  predict(OLS_model,H_df[cvobs[a:b],])*sd(out_mod) + mean(out_mod)
  }
  
  errorstats[1,1]  <- mean(predict_ols)
  errorstats[1,2] <-  mean(abs(predict_ols - out_mod) / abs(out_mod)*100)
  errorstats[1,3] <-  mean(abs(predict_ols - out_mod))
  errorstats[1,4] <-  mean((predict_ols - out_mod)^2)
  errorstats[1,5] <-  mean( sqrt((predict_ols - out_mod)^2)/(yrange) )
  error_matrix[,1] <- abs((predict_ols - out_mod)/ abs(out_mod)*100)
  
  #random forest emulator 
  # note: some tunning may be necessary regarding mtry (nr variables) and ntree (nr of trees for the emsemble))
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
    #hyperp
    errorstats[modelID,6] <- mean_error_rf[1] ;  errorstats[modelID,7] <- mean_error_rf[2]
    errorstats[modelID,8] <- mean_error_rf[3] ;  errorstats[modelID,9] <- mean_error_rf[4]
    #errorstats[10,6] <-
    #errorstats[10,6] <-
    error_matrix[,10] <- abs((predicted_rf[,index] - out_mod)/ abs(out_mod)*100)
    
  }  #  ********************************************************************************************
  
  
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
    error_matrix[,2] <- abs((predict_GP[,index] - out_mod)/ abs(out_mod)*100)
    
    errorstats[2,6] <-mean_error_gp[1]
    errorstats[2,7] <-mean_error_gp[2]
    errorstats[2,8] <-mean_error_gp[3]
    errorstats[2,9] <-mean_error_gp[4]
    errorstats[2,10] <-mean_error_gp[5]
    
  }  #  ********************************************************************************************
  
 
   if(fitBayesGP) { 
    predict_GP2 <-matrix(0,n_expdes,1) 
    # alternative bprior = "bmzt", corr="expsep", "matern"
    for (cv in 1: number_cv) {
      a<-(1 + cvfold*(cv-1)) ; b<-(cvfold + cvfold*(cv-1))
      gp_model2 <- bgp(Z[-cvobs[a:b],],y_stand[-cvobs[a:b]],meanfn = "linear", bprior = "bflat", corr = "expsep", BTE = c(1000, 4000,2))  
      predict_GP2[cvobs[a:b]] <-  predict(gp_model2,H_df[cvobs[a:b],])$ZZ.mean *sd(out_mod) + mean(out_mod)
    } 
    
    errorstats[3,1]  <- mean(predict_GP2)
    errorstats[3,2] <-  mean(abs(predict_GP2- out_mod) / abs(out_mod) *100)
    errorstats[3,3] <-  mean(abs(predict_GP2- out_mod))
    errorstats[3,4] <-  mean((predict_GP2 - out_mod)^2)
    errorstats[3,5] <-  mean( sqrt((predict_GP2 - out_mod)^2)/(yrange) )
    error_matrix[,3] <- abs((predict_GP2- out_mod)/ abs(out_mod)*100)
 
     }   #  ********************************************************************************************
  
 
   if(fitGP_fit) {  
    predict_GP3 <-matrix(0,n_expdes,1) 
    for (cv in 1: number_cv) {
      a<-(1 + cvfold*(cv-1)) ; b<-(cvfold + cvfold*(cv-1))
      gp_model3 <- GP_fit(X=Z[-cvobs[a:b],],Y=y_stand[-cvobs[a:b]],control = c(100*nparams, 40* nparams, 2* nparams),maxit=50,trace=TRUE) 
      predict_GP3[cvobs[a:b]] <-  predict(gp_model3,H_df[cvobs[a:b],])$Y_hat*sd(out_mod) + mean(out_mod)
    } 
    
    errorstats[4,1]  <- mean(predict_GP3)
    errorstats[4,2] <-  mean(abs(predict_GP3- out_mod) / out_mod *100)
    errorstats[4,3] <-  mean(abs(predict_GP3- out_mod))
    errorstats[4,4] <-  mean((predict_GP3 - out_mod)^2)
    errorstats[4,5] <-  mean( sqrt((predict_GP3 - out_mod)^2)/(yrange) )
    error_matrix[,4] <- abs((predict_GP3- out_mod)/ out_mod*100)
    
  }  #  ********************************************************************************************
  
  
  
  if (fitNN) {  
    
    # first with package nnet           ***********************************************
    
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
    
    errorstats[5,6] <- mean_error_nn[1]
    errorstats[5,7] <- mean_error_nn[2]
    errorstats[5,8] <- mean_error_nn[3]
    errorstats[5,9] <- mean_error_nn[4]
    errorstats[5,10] <- mean_error_nn[5]
    errorstats[5,11] <-  mean_error_nn[6]
    
    
    #  first with package neuralnet              ***********************************************
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
    
    errorstats[6,6] <- mean_error_nn2[1]
    errorstats[6,7] <- mean_error_nn2[2]
    errorstats[6,8] <- mean_error_nn2[3]
    errorstats[6,9] <- mean_error_nn2[4]
    errorstats[6,10] <- mean_error_nn2[5]
    errorstats[6,11] <-  mean_error_nn2[6]
    
    
    
  }  # fitNN      *************************************************************************
  
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
    errorstats[modelID,6] <- mean_error_gbm[1] ;   errorstats[modelID,7] <- mean_error_gbm[2]
    errorstats[modelID,8] <- mean_error_gbm[3] ;   errorstats[modelID,9] <- mean_error_gbm[4]
    errorstats[modelID,10] <- mean_error_gbm[5] ;  errorstats[modelID,11] <-  mean_error_gbm[6]
  }
  
  
  if(fitmboost) {
    ddata <- data.frame(y_stand,Z)
    predict_mb <- matrix(0,n_expdes,length(shrink))
    for (cv in c(1:number_cv) ) {  
      a<-(1 + cvfold*(cv-1)) ; b<-(cvfold + cvfold*(cv-1))
      for (k in c(1:length(shrink)) )  {
        mboost_model <- mboost(formulamboost,data=ddata[-cvobs[a:b],],family=Gaussian(),baselearner=("bbs"),control=boost_control(mstop=stop[k],nu= shrink[k] ,center=FALSE))
        predict_mb[cvobs[a:b],k] <- predict(mboost_model,ddata[cvobs[a:b],])*sd(out_mod) + mean(out_mod) 
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
    
    errorstats[modelID,6] <- mean_error_mb[1] ;   errorstats[modelID,7] <- mean_error_mb[2]
    errorstats[modelID,8] <- mean_error_mb[3] ;   errorstats[modelID,9] <- mean_error_mb[4]
    errorstats[modelID,10] <- mean_error_mb[5] ;   errorstats[modelID,11] <-  mean_error_mb[6]
    
  }
  
  
  if (fitGAM) {
    
    predict_gam <- matrix(0,n_expdes,6) 
    predict_gam2 <- matrix(0,n_expdes,6) 
    
    for (cv in c(1:number_cv) ) { 
      a<-(1 + cvfold*(cv-1)) ; b<-(cvfold + cvfold*(cv-1))
      gam_model1 <- gam::gam(formulagam,data=H_df[-cvobs[a:b],],family="gaussian")
      gam_model2 <- gam::gam(formulagam2,data=H_df[-cvobs[a:b],],family="gaussian")
      gam_model3 <- gam::gam(formulagam3,data=H_df[-cvobs[a:b],],family="gaussian")
      gam_model4 <- gam::gam(formulagam5,data=H_df[-cvobs[a:b],],family="gaussian")
      
      #note: choosing option select=TRUE introduces penalization in the model
      gam_model21 <- mgcv::gam(formulagam_mgcv1,data=H_df[-cvobs[a:b],],method="REML",select=TRUE)
      gam_model31 <- mgcv::gam(formulagam_mgcv1,data=H_df[-cvobs[a:b],],method="REML",select=FALSE)
      gam_model41 <- mgcv::gam(formulagam_mgcv3,data=H_df[-cvobs[a:b],],method="REML",select=FALSE)
      gam_model51 <- mgcv::gam(formulagam_mgcv3,data=H_df[-cvobs[a:b],],method="REML",select=TRUE)
      
      predict_gam[cvobs[a:b],1] <-predict(gam_model1,H_df[cvobs[a:b],])*sd(out_mod) + mean(out_mod)
      predict_gam[cvobs[a:b],2] <-predict(gam_model2,H_df[cvobs[a:b],])*sd(out_mod) + mean(out_mod)
      predict_gam[cvobs[a:b],3] <-predict(gam_model3,H_df[cvobs[a:b],])*sd(out_mod) + mean(out_mod)
      predict_gam[cvobs[a:b],4] <-predict(gam_model4,H_df[cvobs[a:b],])*sd(out_mod) + mean(out_mod)
      
      predict_gam2[cvobs[a:b],1] <-predict(gam_model21,H_df[cvobs[a:b],])*sd(out_mod) + mean(out_mod)
      predict_gam2[cvobs[a:b],2] <-predict(gam_model31,H_df[cvobs[a:b],])*sd(out_mod) + mean(out_mod)
      predict_gam2[cvobs[a:b],3] <-predict(gam_model41,H_df[cvobs[a:b],])*sd(out_mod) + mean(out_mod)
      predict_gam2[cvobs[a:b],4] <-predict(gam_model51,H_df[cvobs[a:b],])*sd(out_mod) + mean(out_mod)
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
    
    errorstats[modelID,6] <- mean_error_gam[1] ;   errorstats[modelID,7] <- mean_error_gam[2]
    errorstats[modelID,8] <- mean_error_gam[3] ;   errorstats[modelID,9] <- mean_error_gam[4]
    #errorstats[modelID,10] <- mean_error_gam[5] ;   errorstats[modelID,11] <-  mean_error_gam[6]
    
    mean_error_mgcv <-matrix(0,length(predict_gam2[1,]),1)  
    for(k in 1:length(predict_gam2[1,])) {
      mean_error_mgcv[k] <-  mean( sqrt((predict_gam2[,k] - out_mod)^2)/(yrange) ) 
    } 
    index <- which(mean_error_mgcv==min(mean_error_mgcv)) 
    if(length(index)>1) { index <- index[1]}
    modelID <-8
    errorstats[modelID,1]  <- mean(predict_gam2[,index])
    errorstats[modelID,2] <-  mean(abs(predict_gam2[,index] - out_mod) / abs(out_mod) *100)
    errorstats[modelID,3] <-  mean(abs(predict_gam2[,index] - out_mod))
    errorstats[modelID,4] <-  mean((predict_gam2[,index] - out_mod)^2)
    errorstats[modelID,5] <-  mean( sqrt((predict_gam2[,index] - out_mod)^2)/(yrange) )
    error_matrix[,modelID] <- abs((predict_gam2[,index] - out_mod)/ abs(out_mod)*100)
    
    errorstats[modelID,6] <- mean_error_mgcv[1] ;   errorstats[modelID,7] <- mean_error_mgcv[2]
    errorstats[modelID,8] <- mean_error_mgcv[3] ;   errorstats[modelID,9] <- mean_error_mgcv[4]
    #errorstats[modelID,10] <- mean_error_mgcv[5] ;   errorstats[modelID,11] <-  mean_error_mgcv[6]
    
  }   #  ********************************************************************************************
  

  if(fitBN) {  
    predict_BN <-matrix(0,n_expdes,3) 
    for (cv in 1: number_cv) {
      a<-(1 + cvfold*(cv-1)) ; b<-(cvfold + cvfold*(cv-1))
      W <- cbind.data.frame(H_df[-cvobs[a:b],],y=y_stand[-cvobs[a:b]])
      
      # Note: this algorithm is based on multiple significance tests
      # sometimes, one arc is undirected , to solve this we make the 
      # criteria for selection stricter by changing alpha (significance level)
      baynetwork1 <- pc.stable(W, cluster = NULL, whitelist = NULL, blacklist = NULL, test = NULL,
                alpha = 0.10, B =NULL, max.sx = NULL, debug = FALSE, undirected = FALSE)
      
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
    error_matrix[,12] <- abs(( predict_BN[,index]- out_mod)/ out_mod*100)
  }
  
  
  
  for (emul in 1:length(emulatornames)) {
    error_matrix[,emul] <- sort(error_matrix[,emul]) 
    errorstats[emul,12] <-   error_matrix[round(0.025*length(error_matrix[,emul])),emul]
    errorstats[emul,13] <-   error_matrix[round(0.1*length(error_matrix[,emul])),emul]
    errorstats[emul,14] <-   error_matrix[round(0.25*length(error_matrix[,emul])),emul]
    errorstats[emul,15] <-   error_matrix[round(0.5*length(error_matrix[,emul])),emul]
    errorstats[emul,16] <-  error_matrix[round(0.75*length(error_matrix[,emul])),emul]
    errorstats[emul,17] <-  error_matrix[round(0.9*length(error_matrix[,emul])),emul] 
    errorstats[emul,18] <-  error_matrix[round(0.975*length(error_matrix[,emul])),emul] 
  }
  
  
  # OUTPUT ###################################################################
  
  output <- errorstats
  if(out ==1) {outputtable <- output} else {outputtable <- rbind(outputtable,output)}
  
} # outcomes loop 
