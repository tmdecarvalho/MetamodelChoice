library(openxlsx)
library(ggplot2)
library(gridExtra)
# v2: two version panel or single dataset.
#single version
# parameters: N, M, expdes, ndatasets
# filename
#  read.file => data.frame
# we can choose 1) outcome to be in figure and 
#  double loop 1...n_datasets



# options                               **************************************
workstation <- "office" # "laptop" #  # "cloud"  # 
typeoffig <-   "panel" # "single" #    
resolution <- 100


if(workstation=="office") { filedir <- "N:\\Documents\\Metamodels\\Results_v4" } 
if(workstation=="laptop") { filedir <- "C:\\Users\\Tiago\\Documents\\GP2020\\Results_v3"}
if(workstation=="cloud")  { filedir <- "/cloud/project"}


if(!workstation=="cloud")  {separ <- "\\"}
if(workstation=="cloud")  {separ <- "/"}

#emulatornames <-c("OLS","GP: isotropic","GP: bayesian", "GP: GPFit","ANN: nnet","ANN: neuralnet","GAM: gam","GAM: mgcv","GAM: mboost","RF: ranger","Boost: gbm") 
emulatornames <-c("lm","kernl","bgp","G","nnet","neuralnet","gam","mgcv","mboost","ranger","gbm","GPFit")
accuracynames <- c("pred mean","perc_abs_error","mean_abs_error","mean_sq_error","rmse.range","hyperp1","hyperp2","hyperp3","hyperp4","hyperp5","hyperp6","2.5%","10%","25%","50%","75%","90%","97.5%")

# which emulators to be shown in figures?
# special_limits is only for scenario S=500, M=2000!!!


outcome <- 3
measure <- 5
Scenario <-  "Main" # "Calib" #"Main" # "ParamV" # "OParams" # "ParamV" # "MainGP" #      # "SemiM" # # # "Simple" # "ParamV"  ..."MainGP"#
n_datasets <-50

# upperlimit for figure (watch out this will change per measure) 
if(measure==5) { upperlim <- 0.08 }
if(measure==4) { upperlim <- 1  }
if(measure==2) { upperlim <- 10 }
if(measure==3) { upperlim <- 0.3 }



# simulation parameters                  **************************************
if(typeoffig == "panel") {
  
   
   
  panelsize <- 6  # 4 : Figure 3 ; 6: Figure 2
  filetoread <- matrix(0, panelsize,1)
  #N <- matrix(0,panelsize,1) ; M <- matrix(0,panelsize,1)
  
  # panel parameters 
  if (panelsize ==4){
   M <- c(10000,10000,10000,10000) ;    N <- c(5000,5000,5000,5000) 
   n_expdes <-c(100,100,100,100)
  }
  
  if (panelsize==6){
    M <- c(10000,10000,1000,2000,10000,10000) ;    N <- c(5000,5000,5000,5000,500,100) 
    n_expdes <-c(100,50,100,500,100,100)
  }
  
  if(panelsize ==4) { Scenario <- c("ParamV1","ParamV2","SemiM","Out7") }  # Fig 4 Article
  if(panelsize == 6) { 
    Scenario <- c("Main","Main","Main","Main","Main","Main")
    special_limits <-c("lm","kernl","neuralnet","mgcv", "gbm")     } 
  
  
  #if(panelsize==2)  { Scenario <-c("ParamV1","ParamV2") }
 

  for (i in c(1:panelsize)) {  
    runname <- paste("accur_M",format(M[i],scientific=F),"N",N[i],"Scenario",Scenario[i],".xlsx",sep="_")
    if(n_expdes[i]==50) {runname <- paste("accur_M",format(M[i],scientific=F),"N",N[i],"Scenario",Scenario[i],"expd",n_expdes[i],".xlsx",sep="_") }
    filetoread[i] <- paste(filedir,runname,sep="\\")  
  }
  
  table1 <- data.frame (read.xlsx(filetoread[1], sheet=6,colNames=TRUE,rowNames = FALSE))
  table1$metamodels <- factor(table1$metamodels, levels=unique(table1$metamodels))
  table1_fig <- split(table1,table1$metamodels) 
  table2 <- data.frame (read.xlsx(filetoread[2], sheet=6,colNames=TRUE,rowNames = FALSE))
  table2$metamodels <- factor(table2$metamodels, levels=unique(table2$metamodels))
  table2_fig <- split(table2,table2$metamodels) 
  errormeasure1<-matrix(0,n_datasets,length(emulatornames)) ; errormeasure2<-matrix(0,n_datasets,length(emulatornames))
  
  if(panelsize>2) {
    table3 <- data.frame (read.xlsx(filetoread[3], sheet=6,colNames=TRUE,rowNames = FALSE))
    table3$metamodels <- factor(table3$metamodels, levels=unique(table3$metamodels))
    table3_fig <- split(table3,table3$metamodels) 
    table4 <- data.frame (read.xlsx(filetoread[4], sheet=6,colNames=TRUE,rowNames = FALSE))
    table4$metamodels <- factor(table4$metamodels, levels=unique(table4$metamodels))
    table4_fig <- split(table4,table4$metamodels) 
    errormeasure3<-matrix(0,n_datasets,length(emulatornames)); errormeasure4<-matrix(0,n_datasets,length(emulatornames))
  }
  
  if(panelsize>4) {
    table5 <- data.frame (read.xlsx(filetoread[5], sheet=6,colNames=TRUE,rowNames = FALSE))
    table5$metamodels <- factor(table5$metamodels, levels=unique(table5$metamodels))
    table5_fig <- split(table5,table5$metamodels) 
    table6 <- data.frame (read.xlsx(filetoread[6], sheet=6,colNames=TRUE,rowNames = FALSE))
    table6$metamodels <- factor(table6$metamodels, levels=unique(table6$metamodels))
    table6_fig <- split(table6,table6$metamodels) 
    errormeasure5<-matrix(0,n_datasets,length(emulatornames)); errormeasure6<-matrix(0,n_datasets,length(emulatornames))
  }
  
  
  
  # select data to be used for figure per emulator 
  for (i in c(1:length(emulatornames))) { 
    errormeasure1[,i] <-  table1_fig[[i]] [,measure+1]
    errormeasure2[,i] <-  table2_fig[[i]] [,measure+1]
  }
  
  if(panelsize > 2) {
   for (i in c(1:length(emulatornames))) { 
    errormeasure3[,i] <-  table3_fig[[i]] [,measure+1]
    errormeasure4[,i] <-  table4_fig[[i]] [,measure+1]
   } 
  }
  
  if(panelsize > 4) {
    for (i in c(1:length(emulatornames))) { 
      errormeasure5[,i] <-  table5_fig[[i]] [,measure+1]
      errormeasure6[,i] <-  table6_fig[[i]] [,measure+1]
    } 
  }
  
  df.fig1 <- data.frame(emulator=factor(rep(emulatornames[c(1,2,3,5,6,7,8,9,10,11,12)],each=n_datasets)),errors=t(cbind(
    t(errormeasure1[,1]), t(errormeasure1[,2]),t(errormeasure1[,3]),t(errormeasure1[,5]),t(errormeasure1[,6]),t(errormeasure1[,7]),
    t(errormeasure1[,8]),t(errormeasure1[,9]),t(errormeasure1[,10]),t(errormeasure1[,11]),t(errormeasure1[,12]))) ) # 10,11 t(errormeasure[,10]),
 
  df.fig2 <- data.frame(emulator=factor(rep(emulatornames[c(1,2,3,5,6,7,8,9,10,11,12)],each=n_datasets)),errors=t(cbind(
    t(errormeasure2[,1]), t(errormeasure2[,2]),t(errormeasure2[,3]),t(errormeasure2[,5]),t(errormeasure2[,6]),t(errormeasure2[,7]),
    t(errormeasure2[,8]),t(errormeasure2[,9]),t(errormeasure2[,10]),t(errormeasure2[,11]),t(errormeasure2[,12]) )))  # 10,11 t(errormeasure[,10]),
  
  if(panelsize>2){
   df.fig3 <- data.frame(emulator=factor(rep(emulatornames[c(1,2,3,5,6,7,8,9,10,11,12)],each=n_datasets)),errors=t(cbind(
    t(errormeasure3[,1]), t(errormeasure3[,2]),t(errormeasure3[,3]),t(errormeasure3[,5]),t(errormeasure3[,6]),t(errormeasure3[,7]),
    t(errormeasure3[,8]),t(errormeasure3[,9]),t(errormeasure3[,10]),t(errormeasure3[,11]),t(errormeasure3[,12]) )))  
   df.fig4 <- data.frame(emulator=factor(rep(emulatornames[c(1,2,3,5,6,7,8,9,10,11,12)],each=n_datasets)),errors=t(cbind(
    t(errormeasure4[,1]), t(errormeasure4[,2]),t(errormeasure1[,2]),t(errormeasure4[,5]),t(errormeasure4[,6]),t(errormeasure4[,7]),
    t(errormeasure4[,8]),t(errormeasure4[,9]),t(errormeasure4[,10]),t(errormeasure4[,11]),t(errormeasure4[,12]) ))) 
  }
  
  if(panelsize>4){
    df.fig5 <- data.frame(emulator=factor(rep(emulatornames[c(1,2,3,5,6,7,8,9,10,11,12)],each=n_datasets)),errors=t(cbind(
      t(errormeasure5[,1]), t(errormeasure5[,2]),t(errormeasure5[,3]),t(errormeasure5[,5]),t(errormeasure5[,6]),t(errormeasure5[,7]),
      t(errormeasure5[,8]),t(errormeasure5[,9]),t(errormeasure5[,10]),t(errormeasure5[,11]),t(errormeasure5[,12]))) ) 
    df.fig6 <- data.frame(emulator=factor(rep(emulatornames[c(1,2,3,5,6,7,8,9,10,11,12)],each=n_datasets)),errors=t(cbind(
      t(errormeasure6[,1]), t(errormeasure6[,2]),t(errormeasure6[,3]),t(errormeasure6[,5]),t(errormeasure6[,6]),t(errormeasure6[,7]),
      t(errormeasure6[,8]),t(errormeasure6[,9]),t(errormeasure6[,10]),t(errormeasure6[,11]),t(errormeasure6[,12]))) )
  }
  
  # make panel figure
  runname2 <- paste("Scen",Scenario,"n_datasets",n_datasets,"outc",outcome,"meas",measure,".tiff",sep="_")
  figurename1 <-  paste(filedir,runname2,sep=separ)
  
  #Figure 2 article
  if(panelsize==6){ 
   title1<- paste("M=",format(M[1],scientific=F),"N=",N[1],"S=",n_expdes[1])
   title2<- paste("M=",format(M[2],scientific=F),"N=",N[2],"S=", n_expdes[2])
   title3<- paste("M=",format(M[3],scientific=F),"N=",N[3],"S=",n_expdes[3])
   title4<- paste("M=",format(M[4],scientific=F),"N=",N[4],"S=", n_expdes[4])
   title5<- paste("M=",format(M[5],scientific=F),"N=",N[5],"S=",n_expdes[5])
   title6<- paste("M=",format(M[6],scientific=F),"N=",N[6],"S=", n_expdes[6])
  }
  
  #Figure 3 article
  if(panelsize==4){
    
    title1<- expression(bold( paste("M= 10000  N= 5000  S= 100, ", beta[12]==-3.5)))
    title2<- expression( bold( paste("M= 10000  N= 5000  S= 100, ", beta[12]==-1)))
    title3<- paste("M=",format(M[3],scientific=F)," N=",N[3]," S=",n_expdes[3], ", semi-Markov")
    title4<- paste("M=",format(M[4],scientific=F)," N=",N[4]," S=", n_expdes[4],", DSM treat")
  }
                                                                    
  #to change order of item: 
  tiff(filename=figurename1,width = 6, height = 6, units = 'in', res = resolution)  # set resolution
  
  #Figure 2
  if(panelsize==6) {
  p1 <- ggplot(df.fig1,aes(emulator,errors)) + geom_boxplot() + labs(y=accuracynames[measure],title=title1)  +ylim(0,upperlim) + theme(
    plot.title = element_text(size = 8,face = "bold") ) + scale_x_discrete(limits=c("lm","GPFit","neuralnet","mgcv", "gbm"))
  p2 <- ggplot(df.fig2,aes(emulator,errors)) + geom_boxplot() + labs(y=accuracynames[measure],title=title2,size=4) +ylim(0,upperlim)+ theme(
    plot.title = element_text(size = 8,face = "bold") )+ scale_x_discrete(limits=c("lm","GPFit","neuralnet","mgcv", "gbm"))
  
   p3 <- ggplot(df.fig3,aes(emulator,errors)) + geom_boxplot() + labs(y=accuracynames[measure],title=title3,size=1)  +ylim(0,upperlim) + theme(
     plot.title = element_text(size = 8,face = "bold") )+ scale_x_discrete(limits=c("lm","GPFit","neuralnet","mgcv", "gbm"))
   p4 <- ggplot(df.fig4,aes(emulator,errors)) + geom_boxplot() + labs(y=accuracynames[measure],title=title4,size=2) +ylim(0,upperlim)+ theme(
     plot.title = element_text(size = 8,face = "bold") )+ scale_x_discrete(limits=special_limits)
   p5 <- ggplot(df.fig5,aes(emulator,errors)) + geom_boxplot() + labs(y=accuracynames[measure],title=title5,size=1)  +ylim(0,upperlim) + theme(
      plot.title = element_text(size = 8,face = "bold") )+ scale_x_discrete(limits=c("lm","GPFit","neuralnet","mgcv", "gbm"))
   p6 <- ggplot(df.fig6,aes(emulator,errors)) + geom_boxplot() + labs(y=accuracynames[measure],title=title6,size=2) +ylim(0,upperlim)+ theme(
      plot.title = element_text(size = 8,face = "bold") )+ scale_x_discrete(limits=c("lm","GPFit","neuralnet","mgcv", "gbm"))
  
    mainfig<- grid.arrange(p1,p2,p3,p4,p5,p6, ncol=2)
  }
  
  #Figure 3
  if(panelsize==4) { 
    p1 <- ggplot(df.fig1,aes(emulator,errors)) + geom_boxplot() + labs(y=accuracynames[measure],title=title1)  +ylim(0,upperlim) + theme(
      plot.title = element_text(size = 8,face = "bold") ) + scale_x_discrete(limits=c("lm","GPFit","neuralnet","mgcv", "gbm"))
    p2 <- ggplot(df.fig2,aes(emulator,errors)) + geom_boxplot() + labs(y=accuracynames[measure],title=title2,size=4) +ylim(0,upperlim)+ theme(
      plot.title = element_text(size = 8,face = "bold") )+ scale_x_discrete(limits=c("lm","GPFit","neuralnet","mgcv", "gbm"))
    
    p3 <- ggplot(df.fig3,aes(emulator,errors)) + geom_boxplot() + labs(y=accuracynames[measure],title=title3,size=1)  +ylim(0,upperlim) + theme(
      plot.title = element_text(size = 8,face = "bold") )+ scale_x_discrete(limits=c("lm","GPFit","neuralnet","mgcv", "gbm"))
    p4 <- ggplot(df.fig4,aes(emulator,errors)) + geom_boxplot() + labs(y=accuracynames[measure],title=title4,size=2) +ylim(0,upperlim)+ theme(
      plot.title = element_text(size = 8,face = "bold") )+ scale_x_discrete(limits=c("lm","GPFit","neuralnet","mgcv", "gbm"))
    
    mainfig<- grid.arrange(p1,p2,p3,p4,ncol=2) 
  }  
  
  
  if(!panelsize>2) { mainfig<- grid.arrange(p1,p2,ncol=2) }

  
  mainfig
  dev.off()
} 

# single figure parameters
if(typeoffig=="single") {
  #run parameters for single file
  N<-5000
  M<-10000
  n_expdes <- 100
  n_datasets <-50 

  runname <- paste("accur_M",format(M,scientific=F),"N",N,"Scenario",Scenario,".xlsx",sep="_")
  if(n_expdes==50) {runname <- paste("accur_M",format(M,scientific=F),"N",N,"Scenario",Scenario,"expd",n_expdes,".xlsx",sep="_") }
  filetoread <- paste(filedir,runname,sep="\\")
  #sheet=6 (alternative)
  table1 <- data.frame (read.xlsx(filetoread, sheet=6,colNames=TRUE,rowNames = FALSE)) #outcome+7
  
  errormeasure<-matrix(0,n_datasets,length(emulatornames))
  
  # reorder data per emulator
  table1$metamodels <- factor(table1$metamodels, levels=unique(table1$metamodels))
  table_fig <- split(table1,table1$metamodels) 
  
  # select data to be used for figure per emulator 
  for (i in c(1:length(emulatornames))) { errormeasure[,i] <-  table_fig[[i]] [,measure+1]  }
  
  
  df.fig <- data.frame(emulator=factor(rep(emulatornames[c(1:12)],each=n_datasets)),errors=t(cbind(
    t(errormeasure[,1]), t(errormeasure[,2]),t(errormeasure[,3]),t(errormeasure[,4]),t(errormeasure[,5]),t(errormeasure[,6]),t(errormeasure[,7]),
    t(errormeasure[,8]),t(errormeasure[,9]),t(errormeasure[,10]),t(errormeasure[,11]),t(errormeasure[,12]))) ) # 10,11 
  
  #figure definitions
  runname2 <- paste("M",format(M,scientific=F),"N",N,"Scen",Scenario,"n_expdes",n_expdes,"n_datasets",n_datasets,"outc",outcome,"meas",measure,".tiff",sep="_")
  figurename1 <-  paste(filedir,runname2,sep="\\")
  emulatornamess <- emulatornames[-4]
  title1<- paste("M=",format(M,scientific=F),"N=",N,"R=",n_expdes)
  tiff(filename=figurename1,width = 6, height = 6, units = 'in', res = resolution)  # set resolution
  
  p1 <- ggplot(df.fig,aes(x=emulator,y=errors)) + geom_boxplot() + labs(y=accuracynames[measure],title=title1)+ylim(0,upperlim)+
    scale_x_discrete(limits=c("lm","GPFit","neuralnet","mgcv", "gbm")) 
  p1
  dev.off()
}
