# initial version tiago m de carvalho 30102019

# 1. runs the simulation model once  
# 2. generates a dataset
# 3. estimates the model parameters using R package msm. 

#packages
library(msm)  # to print state table, fit msm models, requires R > 3.5.0
library(data.table)
library(stats)
library(dplyr)
# does
# function run simulation
# 1. find age group function -> for model output
# 3. matrix for simulation outputs
# 4. run the simulation (including natural history and printing outputs)


# *********************************************************************************************************

#Basic information
#use module function to check if N/ncohorts is integer 
try( if(N%%ncohorts!=0) stop("For simplification it is required that N/ncohorts is an INTEGER"))
deathages <- matrix(0,N/ncohorts,4)

#sample death ages given N
for (ch in (1:ncohorts)) {
   # sample people who are alive at begin FU (plus survive at least one year)
  deathages[,ch] <- sample(yearzero[ch]:100,size=N/ncohorts,replace=TRUE,prob=year_deathprob[yearzero[ch]:100,ch])
}

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
       # note: lys.i is the main model outcome
       dlys.i <-`if`(deathdate_i <EndYear, lys.i/exp(disc.rate*(deathdate_i  -BeginYear)),
                  lys.i/exp(disc.rate*(EndYear-BeginYear)) ) 
     
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
  
  # END LOOP 1: Natural history  ****************************************************************************
} # FOR


#  fit the parameters of the simulation model using R package msm  ******************************************
print(statetable.msm(state,id,data=n.data))
n.data$b.age <- n.data$b.age - centerage  # avoid large values of age (could cause numerical overflow error)

q <- exp(-3) # initial value
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