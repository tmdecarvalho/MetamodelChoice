# author Tiago Marques, VUmc  start:10122019

# build training data for emulator. 

# requires: SimulationModel_v8 (or subsequent versions) should be run first. 
# note: do not add any memory management commands 

# THIS FILE DOES...
#  build training data for emulator, by running simulation model multiple times at DIFFERENT parameter values ;


#packages
library(lhs)

modeloutputs <- matrix(0,n_expdes,5)  # matrix to save decision model outcomes that we wish to emulate
S <- n_expdes  # n_expdes  #n_expdes  # number simulations, should be equal to n_expdes
hazm <- matrix(0,nstates,nstates)
riskcov1 <- matrix(0,nstates,nstates) ; riskcov2 <- matrix(0,nstates,nstates)
agecov <- matrix(0,nstates,nstates)

exp_des <- matrix(0,n_expdes,nparams)

lowerbound <- lbound_matrix[dd,]
upperbound <-  ubound_matrix[dd,]

#select parameter combinations to be ran with latin hypercube sampling
ee <- maximinLHS(n_expdes, nparams, dup=1)
for (j in (c(1:nparams))) { 
  for (i in (c(1:n_expdes))) {
    exp_des[i,j] <- qunif(ee[i,j],lowerbound[j],upperbound[j])
  }
}

# if no treatment effect, set respective parameters to zero.
if(!effectoftreatment) {exp_des[,9] <-  0 ; exp_des[,10] <- 0  }


# Note: from this point on pretty much copy/paste loop 1 from SimulationModel_v8         ******************************
#         *********************************
#MAIN LOOP i=1,....,n_expdes, n_expdes == # training data samples, possibly 10 x # decision model parameters. 


for (s in c(1:S) ) {
  
  cat(" ","\n")  ; cat(" ","\n") ; cat("Simulation Number ",s,"\n")
  
  # parameters used in the model at the Simulation s
  hazm[1,2] <- exp_des[s,1]    #onset/detectable disease  average age of onset : 66
  hazm[2,3] <- exp_des[s,2] + exp_des[s,9]     # initial state  -> advanced disease
  hazm[3,4] <- exp_des[s,3] + exp_des[s,10]    # death cancer
  #hazm[4,5] <- exp_des[s,4] 
  #hazm[5,6] <- exp_des[s,5] 
  riskcov1[1,2] <-exp_des[s,7] 
  riskcov2[1,2] <- exp_des[s,8] 
  agecov[1,2] <-  exp_des[s,4]   # pay attention to the age scale when definiting this parameter
  agecov[2,3] <-  exp_des[s,5] 
  agecov[3,4] <- exp_des[s,6] 
  
  
  # simulate death ages
  deathages <- matrix(0,M/ncohorts,4)
  set.seed(s + 100*M  + S*dd)
  for (ch in (1:ncohorts)) {
    deathages[,ch] <- sample(yearzero[ch]:100,size=M/ncohorts,replace=TRUE,prob=year_deathprob[yearzero[ch]:100,ch])
  }
  
  
  # COPY/PASTE from SimulationModel_v8 (or posterior versions)       #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& &
  
  # DO NOT MAKE ANY CODE CHANGES HERE, OTHERWISE RESULTS COULD BECOME INCONSISTENT
  # if changes are needed:
  # 1) change in the original file (SimulationModel_v8)
  # 2) copy/paste here
  
  cat(" ","\n") ;   cat(" ","\n")
  cat("Simulation Number", s , "\n")
  cat("simulate life history (loop 1)","\n") 
  cohort_i <- 1
  
  for (i in 1:M)   { 
    
    # INITIATE LIFE HISTORY *********************************************************************************************   
    set.seed(n_datasets*2 + i + M*dd) # set seed number
    
    # check which cohort to simulate and simulate birthdate
    if (i > cohort_i * M/ncohorts) { cohort_i <- cohort_i + 1 }
    r_b <- runif(1,0,4.99) # date of birth is uniform within birth cohort. 
    birthdate_i <- start_birthyear[cohort_i]+r_b
    
    #initialization
    age0   <- BeginYear-birthdate_i   # time scale is from begin follow-up
    time0 <- 0  ;     id     <- i
    states <- 1 ;     times <- time0  # time since begin FU
    ages <- age0    ; years <- BeginYear # calendaryear # age
    trans <- 0                       # transition number
    duration <- matrix(0,nstates,1)  # store durations
    skiptransitions <- FALSE # this should be set to true only if event occurs after FU
    
    
    #simulate deathage (and store deathdate)
    r_d <- runif(1,0,1)
    t15 <-  deathages[i -( (M/ncohorts)*(cohort_i-1)) ,cohort_i] + r_d  # retrieving the death time given cohort
    #t15 <- rexp(1,exp(hazm[1,5])) + age0
    #deathdate_i <- min(birthdate_i + t15,birthdate_i+100)
    deathdate_i <- birthdate_i + t15
    timedeath_i <- deathdate_i - BeginYear
    
    #select risk cohorts
    pop_cat <-0 ;   r_pop <- runif(1,0,1)  ; if(r_pop>0.5) { pop_cat <-1} # risk category
    pop_cat2 <-0 ;  r_pop <- runif(1,0,1)  ; if(r_pop>0.8) { pop_cat2 <-1} # risk category
    
    while (!skiptransitions) { # if natural history is not yet finished 
      trans <- trans + 1  # increment transition
      duration[trans] <- rexp(1,exp(hazm[trans,trans+1]+riskcov1[trans,trans+1]*pop_cat+
                                      riskcov2[trans,trans+1]*pop_cat2 +agecov[trans,trans+1]*(age0-centerage)) ) 
      
      if(sum(duration) > time.followup & timedeath_i> time.followup ) {
        duration[trans] <- time.followup - times[length(times)] 
        skiptransitions <- TRUE 
        state <- states[length(states)]
      }
      
      if(!skiptransitions) { state <- `if`(sum(duration)<t15-age0,trans+1,deathOCstate) }
      
      # if person dies from disease stop loop and assign duration
      if (state >= deathCstate) { 
        skiptransitions <- TRUE 
        if (state == deathOCstate) { duration[trans] <- `if`(trans==1,t15-age0,t15-age0-times[length(times)]) }
        if(state==deathCstate) {
          lys.i <-`if`(deathdate_i <EndYear, deathdate_i - (BeginYear+sum(duration)), 
                       EndYear - (BeginYear+sum(duration)) )
          dlys.i <-`if`(deathdate_i <EndYear, lys.i/exp(disc.rate*(deathdate_i  -BeginYear)),
                        lys.i/exp(disc.rate*(EndYear-BeginYear)) ) 
          
          modeloutputs[s,5]  <- modeloutputs[s,5]  + dlys.i # add individual contribution to LYs
          modeloutputs[s,4] <-  modeloutputs[s,4]  + lys.i 
        }
      }  # assign correct duration
      
      #update natural history
      age <- sum(duration)+ age0
      ages<-   c(ages,age) ;  times <- c(times,sum(duration))
      states <- c(states,state) ; years <- c(years,birthdate_i +age)
      
      #store in dataset
      #if (skiptransitions) {
      #  data_i <- data.table(id=i,time=times,age=ages,year=years,state=states,pop_cat=pop_cat,pop_cat2=pop_cat2,b.age=ages[1],doc.date=deathdate_i)
      #  if(i==1){n.data <- data_i}else{n.data <- bind_rows(n.data,data_i)} 
      #} # if transitions are finished
    }  # WHILE
    
    if(max(states)==deathOCstate) { modeloutputs[s,1] <- modeloutputs[s,1]+1 }
    if(max(states)==deathCstate) { modeloutputs[s,2] <- modeloutputs[s,2]+1 }
    if(max(states)==clinstate | states[length(states)-1]==clinstate) { modeloutputs[s,3] <- modeloutputs[s,3]+1 }
    
    
    # END LOOP 1: Natural history  ****************************************************************************
  } # FOR
  
  print(exp_des[s,])
  print(modeloutputs[s,])
  
}  # FOR  s

# END OF SIMULATION                         *********************************************************

#define the output to be store
print(modeloutputs)
modeloutputs <- modeloutputs/M*100               # rescale to percentage 

if(!effectoftreatment){ outcomes[,1:5] <-  modeloutputs}
if(effectoftreatment) { 
  outcomes[,6:10] <- modeloutputs
  trainingdata <- cbind(exp_des,outcomes)
}


# end of file     ****************************************************************************************************************  


