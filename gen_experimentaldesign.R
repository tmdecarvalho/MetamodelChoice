#generate experimental design matrices

library(lhs)

for (dd in 1:n_datasets) {
 set.seed(dd + n_datasets) 
 lowerbound <- lbound_matrix[dd,]
 upperbound <- ubound_matrix[dd,]
 
 
 ee <- maximinLHS(n_expdes, nparams, dup=1)
 for (j in (c(1:nparams))) { 
   for (i in (c(1:n_expdes))) {
     exp_des[i + ((dd-1)*n_expdes) , j] <- qunif(ee[i,j],lowerbound[j],upperbound[j])
   }
 }
 
} # for 
 