library(openxlsx)
library(ggplot2)

# v2: two version panel or single dataset.
#single version
# parameters: N, M, expdes, ndatasets
# filename
#  read.file => data.frame
# we can choose 1) outcome to be in figure and 
#  double loop 1...n_datasets



# options                               **************************************
workstation <-  "office"  # "laptop" #   "cloud"  # 


if(workstation=="office") { filedir <-"N:\\Documents\\Metamodels\\R code\\Article" } # "N:\\Documents\\Metamodels\\Results_v4" }  # 
if(workstation=="laptop") { filedir <- "C:\\Users\\Tiago\\Documents\\GP2020\\Results_v3"}
if(workstation=="cloud")  { filedir <- "cloud\\project"}

emulatornames <-c("OLS","GP: isotropic","GP: bayesian", "GP: GPFit","ANN: nnet","ANN: neuralnet","GAM: gam","GAM: mgcv","GAM: mboost","RF: ranger","Boost: gbm","GP: GP_fit") 
accuracynames <- c("pred mean","perc_abs_error","mean_abs_error","mean_sq_error","rmse.range","hyperp1","hyperp2","hyperp3","hyperp4","hyperp5","hyperp6","2.5%","10%","25%","50%","75%","90%","97.5%")
paramnames <- c("q12","q23","q34","agec12","agec23","agec34","riskf1_12","riskf2_12","treat_2","treat_3")
# simulation run parameters
N<-5000
M<-10000
n_expdes <- 100
n_datasets <-50 
nparams <- 10
outcome <- 3          # model outcome   
measure <- c(2,3,4,5) # accuracy measure
hyperp <-c(6:11)+1    # hyperparameters
emulatorsused <- c(1,2,3,5,6,7,8,9,10,11,12) # compare with emulatornames which emulators are used
Scenario <- "bnlearn" # "OParams" # "Simple" #  ...
runname <- paste("accur_M",format(M,scientific=F),"N",N,"Scenario",Scenario,".xlsx",sep="_")
if(n_expdes==50) {runname <- paste("accur_M",format(M,scientific=F),"N",N,"Scenario",Scenario,"expd",n_expdes,".xlsx",sep="_") }
filetoread <- paste(filedir,runname,sep="\\")

#accuracy table
table1 <- data.frame (read.xlsx(filetoread, sheet=6,colNames=TRUE,rowNames = FALSE))
# y outcomes
table2 <- data.frame(read.xlsx(filetoread, sheet=5,colNames=TRUE,rowNames =FALSE))
# uncertainty_interval
table3 <- data.frame(read.xlsx(filetoread, sheet=4,colNames=TRUE,rowNames =FALSE))
# estimated model parameters (to compute bias)
table4 <- data.frame(read.xlsx(filetoread, sheet=2,colNames=TRUE,rowNames =FALSE))
table5 <- data.frame(read.xlsx(filetoread, sheet=1,colNames=TRUE,rowNames =FALSE))



# 1. Statistics about experimental design (X matrix): mean, median, stdev: original, estimated parameters, bias and CI

param_value_m <-  matrix(0,12,nparams)
outcome_value_m <-  matrix(0,6,nparams)
for (j in (1:nparams)) { 
  param_value_m [1,j] <- mean( table4[,j]) ;                      param_value_m [2,j] <- mean( table5[,j])
  param_value_m [3,j] <- sd( table4[,j] )
  param_value_m [4,j] <- min( table4[,j])  ;                      param_value_m [5,j] <- max( table4[,j])
  param_value_m [6,j] <- mean( abs(table4[,j] - table5[,j]) )   ; param_value_m [7,j] <- sd(abs(table4[,j] - table5[,j]) ) 
  param_value_m [8,j] <- mean( abs(table3[,j] - table3[,j+10])) ; param_value_m [9,j] <- sd(abs(table3[,j] - table3[,j+10]) ) 
  param_value_m [10,j] <- median( table4[,j]) 
  param_value_m [11,j] <- median(abs(table4[,j] - table5[,j]) )
  param_value_m [12,j] <- median(abs(table3[,j] - table3[,j+10]) )
}

rownames(param_value_m) <- c("true mean","est mean","min true","max true","mean bias","stdev bias"," mean CI size",
                             "stdev CI","stdev true","median true","median bias","median CI size")
colnames(param_value_m) <- paramnames



# y outcome stats
y_outcome <- matrix(0,n_datasets,2)
for (j in (1:n_datasets)) {
 y_outcome[j,1] <- mean(table2[,j])
 y_outcome[j,2] <- sd(table2[,j])
}

# accuracy stats (for all measures => appendix)
mean_error<- matrix(0,length(emulatornames),length(measure))
std_error<- matrix(0,length(emulatornames),length(measure))
colnames(mean_error)<- accuracynames[measure]
colnames(std_error)<- accuracynames[measure]
table1$metamodels <- factor(table1$metamodels, levels=unique(table1$metamodels))
table_fig <- split(table1,table1$metamodels) 

# mean and standard deviation prediction error 
for (j in c(1:length(measure)) ) {
  for (i in c(1:length(emulatornames))) { 
    mean_error[i,j] <- mean( table_fig[[i]] [,j+2] ) #average
    std_error[i,j] <- sd(table_fig[[i]] [,j+2])   #stdev
  }
}

#hyperparameter measures
#  #1: hyperp with minimum error in each iteration
#  #2: average "loss" by not choosing correct hyperparameter

hyperp_table <- table1[,c(1,hyperp)]

#replace zero values in table by a control value  
for (i in c(2:(length(hyperp)+1) )) { 
  for (j in c(2:(length(hyperp_table[,1]))) ) {
    if(hyperp_table[j,i]==0) { hyperp_table[j,i]<- 99 }  
  }
} 


hyperp_table$metamodels <- factor(hyperp_table[,1], levels=unique(hyperp_table[,1]))
hyperp_table_stat <- split(hyperp_table,hyperp_table$metamodels) 
hyper_error <- matrix(0,n_datasets,length(emulatornames)) # matrix to store value of hyperparam with min error
hyper_loss <-  hyperp_table_stat  # loss due to wrong choice of hyperparameter
mean_hyperloss <- matrix(0,1,length(emulatornames))  # average loss per iteration for each emulator

# loop to compute per emulator and dataset, which hyperparameter gives minimum error
for (i in emulatorsused[-1]) { 
  for (j in c(1:n_datasets) ) {
    hyper_error[j,i] <- which(hyperp_table_stat[[i]][j,2:(length(hyperp)+1)]==min(as.numeric(hyperp_table_stat[[i]][j,])))
    hyper_loss [[i]] [j,(2:(length(hyperp)+1))]  <- abs(hyperp_table_stat[[i]] [j,(hyper_error[j,i]+1) ] - hyperp_table_stat[[i]] [j,(2:(length(hyperp)+1))])
    summ <- (mean(hyper_loss[[i]][,2]) + mean(hyper_loss[[i]][,3])+ mean(hyper_loss[[i]][,4])+ mean(hyper_loss[[i]][,5]) )
    l <- 4
    if(mean(hyper_loss[[i]][,6]) < 50) { summ <- summ + mean(hyper_loss[[i]][,6]) ; l <- l + 1 }
    if(mean(hyper_loss[[i]][,7]) < 50) { summ <- summ + mean(hyper_loss[[i]][,7]) ; l <- l + 1 }
    mean_hyperloss[i] <- summ/l           
  }
}
colnames(hyper_error) <- emulatornames
colnames(mean_hyperloss) <- emulatornames

# line 1 : hyper_error [j,i] retrieves the index of hyperparameter with lowest prediction error. 
# line 2 : hyper_loss [[i]] [j, ...] computed the loss per iteration, emulator and for each hyperparameter compared with best hyperparameter chocie
# note: 50 is a "control value"; we dont want to add values to "summ"  that correspond to no hyperparameter (99)
# note2: l is an auxiliary variable; all emulators have at least 4 hyperparameters (except linear model); then we sum 
# to l if more hyperparameters are used up to a maximum of 6
filestats <- paste("accurstats_M",format(M,scientific=F),"N",N,"Scenario",Scenario,"expd",n_expdes,".xlsx",sep="_") 
filetowrite <- paste(filedir,filestats,sep="\\")

param_value_m <- as.data.frame(param_value_m)
y_outcome <- as.data.frame(y_outcome)

listoutputs <-list("experim design"=param_value_m,"y outcome" = y_outcome, "pred_error"=rbind.data.frame(mean_error,std_error),"hyperp"=rbind.data.frame(mean_hyperloss,hyper_error) )
openxlsx::write.xlsx(listoutputs,filetowrite,asTable =TRUE,colnames=TRUE,colWidths=12)
