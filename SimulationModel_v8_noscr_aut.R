#tiago m de carvalho 19112018
# vlifetables 20122018 : other cause death simulated from a lifetable
# (partly based on code from Ardo van den Hout 2009)
# body of the code based on Simulate_CalibrationData.R

# this file runs the simulation model in simulate_CalibrationData.R given the parameters
# supplied by the user. 


#VERSION  3
# main motivation for this version was to build simulation model again in such a way that it is relatively easy to debug
# every stage is "modularized"

# divided in 4 loops : natural history, impose FU, atrisk/event counts, impose "design" (a la Ardo!)

#VERSION 4 26062019
# the loops in version 3 for Atrisk  became quite cumbersome to debug, and there were still a few errors.
# in this version i try a simpler approach to computer Atrisk for each year and i try to handle possibility of multiple cohorts

#Version 5 30102019
# goal is to use this for the emulator paper as the "simulator" model.
# observed data are the transitions during a certain follow up time
# initially we see "everything" (no interval censoring?)
# single birth cohort
# estimate model using msm package or calibration
# confidence interval would be then used for PSA. 
# no need for incidence or at risk loops

#version 6
# several birth cohorts
# covariates added to transitions between states
# erased loop 2 (useless and makes computation time quite slow)

# version 7
# compute life years gained ; add treatment effect
# added continuous discounting to LYG; 
# i refer back to year of death OC or End of follow up for discount rate.

#version 7.1
# tried to optimize code for faster running time: 
# replace rbind  by bindrows, if by `if`, erased superflous statements
# && instead & ,  => all this changes had a limited efficacy

# version 8
# no screening loop, end of FU imposed in natural history loop. 

#packages
library(msm)  # to print state table, fit msm models, requires R > 3.5.0
library(data.table)
library(stats)
library(dplyr)
# does
# function run simulation
# 1. functions for transition probabilities (exp gomp)
# 2. find age group function -> for model output
# 3. matrix for simulation outputs
# 4. run the simulation (including natural history and printing outputs)


# *********************************************************************************************************

#Basic information
#N <- 1000 # number of people sampled
#use module function to check if N/ncohorts is integer 
try( if(N%%ncohorts!=0) stop("For simplification it is required that N/ncohorts is an INTEGER"))
deathages <- matrix(0,N/ncohorts,4)
#sample death ages given N
for (ch in (1:ncohorts)) {
   # sample people who are alive at begin FU (plus survive at least one year)
  deathages[,ch] <- sample(yearzero[ch]:100,size=N/ncohorts,replace=TRUE,prob=year_deathprob[yearzero[ch]:100,ch])
}

# true parameter vector (what we are trying to estimate)
#theta <- c(hazm[1,2],hazm[2,3],hazm[3,4],  # hazm[4,5],  #hazm[5,6],
#           agecov[1,2],agecov[2,3], agecov[3,4], riskcov1[1,2], riskcov2[1,2] )

hazm[1,2] <- theta[1] ; hazm[2,3] <- theta[2] ; hazm[3,4] <- theta[3]
agecov[1,2]<-theta[4] ; agecov[2,3]<-theta[5] ; agecov[3,4]<-theta[6] 
riskcov1[1,2] <- theta[7] ; riskcov2[1,2]<- theta[8]

# make sure to understand what are the min and max ages before defining this otherwise estimates could be weird.

# Matrices to store results        ****************************************************

cat("Sample size is ",N,"\n")
cat("Creating baseline data...\n")

# RUN the simulation *******************************************************************************************************************

# Loop 1 :  natural history without imposing limitation on year of follow-up *************************************************
cat("natural history with complete observations ","\n") 

# "BIG LOOP" runs several simulations if check.model.estimates is TRUE  

cat(" ","\n") ;   cat(" ","\n")
#cat("Simulation Number", s , "\n")
cat("simulate life history (loop 1)","\n") 
cohort_i <- 1

 for (i in 1:N)   { 
  
  # INITIATE LIFE HISTORY *********************************************************************************************   
  #set.seed(i) # set seed number
   
  # check which cohort to simulate and simulate birthdate
  if (i > cohort_i * N/ncohorts) { cohort_i <- cohort_i + 1 }
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
  t15 <-  deathages[i -( (N/ncohorts)*(cohort_i-1)) ,cohort_i] + r_d  # retrieving the death time given cohort
  #t15 <- rexp(1,exp(hazm[1,5])) + age0
  #deathdate_i <- min(birthdate_i + t15,birthdate_i+100)
  deathdate_i <- birthdate_i + t15
  timedeath_i <- deathdate_i - BeginYear
  
  #select risk cohorts
  pop_cat <-0 ;   r_pop <- runif(1,0,1)  ; if(r_pop>0.5) { pop_cat <-1}  # risk category
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
       
       #outcomes[s,5]  <- outcomes[s,5]  + dlys.i # add individual contribution to LYs
       #outcomes[s,4] <-  outcomes[s,4]  + lys.i 
     }
    }  # assign correct duration
    
    #update natural history
    age <- sum(duration)+ age0
    ages<-   c(ages,age)
    times <- c(times,sum(duration))
    states <- c(states,state)
    years <- c(years,birthdate_i +age)
    
    #store in dataset
    if (skiptransitions) {
      data_i <- data.table(id=i,time=times,age=ages,year=years,state=states,pop_cat=pop_cat,pop_cat2=pop_cat2,b.age=ages[1],doc.date=deathdate_i)
      if(i==1){n.data <- data_i}else{n.data <- bind_rows(n.data,data_i)} 
    } # if transitions are finished
  }  # WHILE
  
 # if(max(states)==deathOCstate) { outcomes[s,1] <- outcomes[s,1]+1 }
#  if(max(states)==deathCstate) { outcomes[s,2] <- outcomes[s,2]+1 }
#  if(max(states)==clinstate | states[length(states)-1]==clinstate) { outcomes[s,3] <- outcomes[s,3]+1 }
  
  
  # END LOOP 1: Natural history  ****************************************************************************
} # FOR


print(statetable.msm(state,id,data=n.data))
#print(outcomes)
#View(n.data)
n.data$b.age <- n.data$b.age - centerage

q <- exp(-3) # initial value
#Q <- rbind(c(0,q,0,0,0,0,q), c(0,0,q,0,0,0,q),c(0,0,0,q,0,0,q),c(0,0,0,0,q,0,q),c(0,0,0,0,0,q,q),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0))
Q <- rbind(c(0,q,0,0,q), c(0,0,q,0,q),c(0,0,0,q,q),c(0,0,0,0,0),c(0,0,0,0,0))


maxit       <- 5000
fnscale     <- N*10
method      <- "Nelder-Mead"  # "BFGS" #
center      <- FALSE
reltol      <- 1e-8
qconstraint <-c(1,2,3,2,4,2) # need to impose restriction that q15=q25=q35 

#model assuming exactly observed data
 cat("Fit model assuming exact transition times \n")
 model1 <- msm(state~time, subject=id, data=n.data, 
              center=center, qmatrix=Q, exacttimes = TRUE,
              #censor = 99, censor.states=list(c(1,2,3)), 
              covariates=list("1-2" = ~ b.age+pop_cat+pop_cat2, "2-3" = ~ b.age, "3-4" = ~ b.age ), 
              method=method, qconstraint = qconstraint,
              covinits = list(b.age=c(0.1,0.1,0.1),pop_cat=0.5,pop_cat2=0.5 ),
              control=list(trace=0, REPORT=1,maxit=maxit, reltol=reltol,fnscale=fnscale))