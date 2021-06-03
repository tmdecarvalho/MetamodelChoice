# main project file 
# Tiago M de Carvalho : 14012020

# this files runs the whole analyses for a single scenario and for a single repetition
# FILES: SimulationModel_v8,  TrainingData, UncertAnalys 

#memory management
ls() ; gc() ; rm(list=ls())
options(digits=3)  

#package to write results in excel (its C-based)
library(openxlsx)
library(lhs)

# inputs to be pre-defined by the user 
# inputs to be pre-defined by the user 
workstation <-  "work" # "cloud"  # "workcloud" #   "laptop"  # where is the analyses being run?
#set file directory according to work location
if(workstation=="cloud")     { filedir <-"/cloud/project"; separ <- "/" }  
if(!workstation=="cloud")  {separ <- "\\"}
if(workstation=="laptop")    { filedir <-"C:/Users/Tiago/Documents/GP2020/R" }
if(workstation=="work")      { filedir <- "N:\\Documents\\Metamodels\\R code\\Article" }
if(workstation=="workcloud") { filedir <- "C:\\Users\\userEB\\Documents\\Tiago_Projects\\Tiago_metamodel_project2"}



#simulation parameters
N <- 5000        # sample size population for upper/lower CI limit
M <- 10000      # sample size popualtion during training
n_datasets <-50   # number of datasets, i.e. (y,X) pairs used to compute prediction accuracy
n_expdes <-100     # number of training samples 
cvfold <- 10      # how many observations we leave out in each cross-validation iteration?

#data generating process
nparams <- 11      # number of parameters in multistate simulation model
nstates <- 5       # number of health states2
hazm <- matrix(0,nstates,nstates)  # Q matrix (transition intensity matrix)
riskcov1 <- matrix(0,nstates,nstates) ; riskcov2 <- matrix(0,nstates,nstates) ; agecov <- matrix(0,nstates,nstates)

deathOCstate <- 5  # death other causes state
deathCstate  <- 4  # cancer/disease - specific death state
clinstate <-    3  # state which is exactly observed


# cohort data
followup_years <- c(2010:2020)                                                      # calendar years of follow up
start_birthyear <- c(1950,1955,1960,1965) ; end_birthyear <- c(1954,1959,1964,1969) # first/last year of birth 
BeginYear <- followup_years[1] ; EndYear<- followup_years[length(followup_years)]
time.followup<- followup_years[length(followup_years)] - followup_years[1]  
#gender <- 2    # for now its dutch females lifetable          # lifetable is gender specific
ncohorts <- length(start_birthyear)  # number of cohorts (each cohort has a different associated lifetable)
centerage <- 50 # the purpose of this factor is to center age and make this doesnt give numerical error.  


#options
disc.rate <- 0.03  # discount rate for life years
emulatornames <-c("OLS","GP: isotropic","GP: bayesian", "GP: GPFit","ANN: nnet","ANN: neuralnet","GAM: gam","GAM: mgcv","GAM: mboost","RF: ranger","Boost: gbm", "BN: bnlearn") 
accuracynames <- c("pred mean","perc_abs_error","mean_abs_error","mean_sq_error","rmse/range","hyperp1","hyperp2","hyperp3","hyperp4","hyperp5","hyperp6","2.5%","10%","25%","50%","75%","90%","97.5%")
paramnames <- c("q12","q23","q34","agec12","agec23","agec34","riskf1_12","riskf2_12","treat_2","treat_3","gomp")


#which metamodels should be fit?
fitNN <- FALSE  # fit neural networks?
fitmboost <-FALSE # fit GAM boosting ?
fitGAM <- FALSE # FALSE  # fit GAM ?
fitGP <-FALSE # fit gaussian process (isotropic) 
fitBayesGP <- FALSE   # bayesian implementation
fitGP_fit <- FALSE
#decision trees (models not used in basecase)
fitboost <- FALSE #FALSE # fit decision trees boosting?
fitRandomForest <- FALSE
fitBN <- TRUE

n_emulators <- length(emulatornames)  # check how many emulators are being used. 
outcomes <- matrix(0,n_expdes,10)
modeloutcome <- 14   #  # 1:oc death, 2: ds death, 3: clin dx , 4: ly, 5: d.ly, (treatmt) 6:
#11: diff oc death , 12: diff ds death, 13: diff clin dx, 14: diff ly , 15: diff d.ly
Scenario <- "SemiM"   # "OParams" #"Simple"  # "Calib" # #"Corr" # #    # 1. Simple, 2. Correlation, 3. SemiMarkov
saveresults <- TRUE


# matrices to store upper and lower bound values (based on estimated CI's and for original values)
lbound_orig <- matrix(0,1,nparams) ; ubound_orig <- matrix(0,1,nparams) 
exp_des_orig <- matrix(0,n_datasets,nparams)

#choose upper, lower values for model parameters of the simulation
#note: 1/exp(-3.5)=33 , 1/exp(-4)=55
lbound_orig <- c(-3.5,-3.5,-3.5,-0.2,-0.2,-0.2,-1,-1,-0.4,-0.4)
ubound_orig <- c(0,0,0,0.2,0.2,0.2,1,1,-0.05,-0.05)

# generate original parameter values
#set.seed(M*N) 
set.seed(123)
ee <- maximinLHS(n_datasets, nparams, dup=1)
for (j in (c(1:(nparams-1))) ) { 
  for (i in (c(1:n_datasets))) {
    exp_des_orig[i,j] <- qunif(ee[i,j],lbound_orig[j],ubound_orig[j])
  }
}  

if(Scenario=="SemiM") { 
  extraparam <- runif(n_datasets,0.1,0.5); alpha_var<- 0.2
  exp_des_orig[,11] <- extraparam
}

#check whether options are correct
try(if(length(paramnames)!=nparams) stop("ERROR: Number of parameter names does not match number of parameters"))
cat("WARNING: We are assuming in the code, only three outcomes are being measured. If this is not the case code needs to be modified ","\n")

#output storage
# matrices to store estimated model parameters and their CI's
lbound_matrix <- matrix(0,n_datasets,nparams) ; ubound_matrix <- matrix(0,n_datasets,nparams)
estim_model_params <- matrix(0,n_datasets,nparams)  # matrix for estimated model parameters via msm. 
#matrices to store outcomes
y_training1 <- matrix(0,n_expdes,n_datasets) # ; y_training2 <- matrix(0,n_expdes,n_datasets) ; y_training3 <- matrix(0,n_expdes,n_datasets)
accuracy_table1 <- matrix(0,n_datasets*n_emulators,length(accuracynames))  #; accuracy_table2 <- matrix(0,n_datasets*n_emulators,length(accuracynames)) ; accuracy_table3 <- matrix(0,n_datasets*n_emulators,length(accuracynames))


filen <- paste(filedir,"read_lifetables_aut.R",sep=separ)
filen0 <-paste(filedir,"Excel/lifetables_NL_5y.txt",sep=separ)
source(filen)

# set names for other necessary files according to work location
filen2 <-  paste(filedir,"SimulationModel_v8_noscr_aut.R",sep=separ)
# filen3 <-  paste(filedir,"TrainingData_noscr_aut.R",sep=separ) }  # training data
filen3 <-  paste(filedir,"TrainingData_noscr_SemiM_aut.R",sep=separ) 
filen4 <- paste(filedir,"UncertAnalys_v5_aut.R",sep=separ)  # fit metamodels and compute accuracy
filen5 <- paste(filedir,"gen_experimentaldesign.R",sep=separ) # generate experimental design used in training to save in Excel

#n_datasets:
for (dd in 1:1) { 
  set.seed(dd)   # set seed for simulation of original data, CI estimates 
  
  hazm[1,2] <- exp_des_orig[dd,1] ; hazm[2,3] <- exp_des_orig[dd,2] ; hazm[3,4] <-exp_des_orig[dd,3]               #  q_12 ->  q_23 -> q_34
  agecov[2,3] <- exp_des_orig[dd,5] ; agecov[1,2] <-  exp_des_orig[dd,4] ; agecov[3,4] <- exp_des_orig[dd,6]  # age covariate values
  riskcov1[1,2] <- exp_des_orig[dd,7] ; riskcov2[1,2] <- exp_des_orig[dd,8]
  teffect2 <- exp_des_orig[dd,9] ; teffect3 <- exp_des_orig[dd,10]
  
  theta <- c(hazm[1,2],hazm[2,3],hazm[3,4],  # hazm[4,5],  #hazm[5,6],
             agecov[1,2],agecov[2,3], agecov[3,4], riskcov1[1,2], riskcov2[1,2] )
  
  alpha <- exp_des_orig[dd,11]
  teffect_var <- runif(1,min=0.1,max=0.3) # set the uncertainty allowed in treatment effect for each iteration
  alpha_var   <- runif(1,min=0.1,max=0.3)
  
  

  
  cat(" ","\n") ;   cat(" ","\n");   cat("Dataset Number ",dd,"\n") ;   cat(" ","\n")
  
  # run first the simulation model once. This is the socalled "original dataset and 
  # estimated CI's give us the upper/lower bounds for the uncertainty quantification. 
  
  source(filen2) # run simulation model and generate original data and estimated model parameters / CIs 

  lbound_matrix[dd,] <- c(log(model1$ci[1,1]),log(model1$ci[3,1]),log(model1$ci[5,1]),
                          model1$ci[7,1],model1$ci[9,1], model1$ci[11,1],model1$ci[13,1],model1$ci[19,1],
                          teffect2*(1+teffect_var), teffect3*(1+teffect_var), alpha*(1-alpha_var))
  
  ubound_matrix[dd,] <-c(log(model1$ci[1,2]),log(model1$ci[3,2]),log(model1$ci[5,2]), 
                         model1$ci[7,2],model1$ci[9,2], model1$ci[11,2],model1$ci[13,2],model1$ci[19,2],
                         teffect2*(1-teffect_var),teffect3*(1-teffect_var), alpha*(1+alpha_var))
  
  estim_model_params[dd,] <- c(model1$opt$par[1],model1$opt$par[3],model1$opt$par[4], 
                               model1$opt$par[5],model1$opt$par[6], model1$opt$par[7],
                               model1$opt$par[8],model1$opt$par[9], teffect2,teffect3,alpha)
  

  #run the training data assuming no treatment effect 
  effectoftreatment <- FALSE # when obtaining the CI
  set.seed(dd + n_datasets)   # set seed to obtain X matrix (needs to be reproducible) 
  cat(" ","\n") ;   cat(" ","\n"); cat("Dataset Number ",dd,"\n");  cat(" ","\n") ;   cat(" ","\n");
  source(filen3) # run training data with no effect of treatment
  
  # run the training data assuming an effect of treatment.
  effectoftreatment <- TRUE 
  set.seed(dd + n_datasets) # repeat set.seed, since we reset set.seed to other values inside the files.
  cat(" ","\n") ;   cat(" ","\n"); cat("Dataset Number ",dd,"\n");  cat(" ","\n") ;   cat(" ","\n");
  source(filen3) # run training data with no effect of treatment
  cat(" ","\n") ;   cat(" ","\n"); cat("Dataset Number ",dd,"\n");  cat(" ","\n") ;   cat(" ","\n");
  #fit the metamodels and compute its prediction accuracy
  source(filen4) 
  
  # save model outcomes used for training and accuracy
  y_training1[,dd] <- out_train[,1] 
  accuracy_table1[(1 +n_emulators*(dd-1)):(n_emulators +n_emulators*(dd-1)),] <- outputtable[1:n_emulators,]
  
  if(length(modeloutcomes)>1) {
    y_training2[,dd] <- out_train[,2] ; y_training3[,dd] <- out_train[,3]
    accuracy_table2[(1 +n_emulators*(dd-1)):(n_emulators +n_emulators*(dd-1)),] <- outputtable[(n_emulators+1):(2*n_emulators),]
    accuracy_table3[(1 +n_emulators*(dd-1)):(n_emulators +n_emulators*(dd-1)),] <- outputtable[(2*n_emulators+1):(3*n_emulators),]
  } 
}

# generate experimental design matrices (X matrix)
exp_des <- matrix(0,n_datasets*n_expdes,nparams)  # keep this matrix OUT of the loop!
source(filen5) # generate experimental design used in training

# file to print outcomes in Excel
runname <- paste("accur_M",format(M,scientific=F),"N",N,"Scenario",Scenario,".xlsx",sep='_')
filename <- paste(filedir,runname,sep=separ)

#print outcomes in excel: exp_des(X matrix), model outcomes (y matrix), accuracy per outcome, uncertainty bound per dataset
exp_des <- data.frame(exp_des) ; colnames(exp_des) <- paramnames ; 
upperlower <- data.frame(ubound_matrix,lbound_matrix)
y_training1 <- data.frame(y_training1) ;
accuracy_table1 <- data.frame(emulatornames,accuracy_table1) 
colnames(accuracy_table1) <- c("metamodels",accuracynames)

if(length(modeloutcomes)>1) { 
  y_training2 <- data.frame(y_training2) ; y_training3 <- data.frame(y_training3)
  accuracy_table2 <- data.frame(emulatornames,accuracy_table2) ; accuracy_table3 <- data.frame(emulatornames,accuracy_table3)
  colnames(accuracy_table2) <- c("metamodels",accuracynames) ; colnames(accuracy_table3) <-  c("metamodels",accuracynames)
}

exp_des_orig <-as.data.frame(exp_des_orig) ; estim_model_params <- as.data.frame(estim_model_params)

#print each element in a different worksheet in the same file
if(length(modeloutcomes)>1){  
  listoutputs <-list("expdes_trueP"=exp_des_orig,"estimated params"=estim_model_params, "expdes"=exp_des,"unc_bounds"=upperlower,"y1"=y_training1,"y2"=y_training2,"y3"=y_training3,"outcome1"=accuracy_table1,"outcome2"=accuracy_table2,"outcome3"=accuracy_table3)
}

if(length(modeloutcomes)==1){
  listoutputs <-list("expdes_trueP"=exp_des_orig,"estimated params"=estim_model_params, "expdes"=exp_des,"unc_bounds"=upperlower,"y1"=y_training1,"outcome1"=accuracy_table1)
}
openxlsx::write.xlsx(listoutputs,filename,asTable =TRUE,colnames=TRUE,colWidths=12)






