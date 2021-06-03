# 10022020: read lifetables for other cause mortality

# FUNCTIONS:same as in RunModel, only redefined if in testing mode. 
read_lifetable <- function(filename,birthyear) {
  
  lifetable <- read.table(filename,1) 
  lifetable <- lifetable[lifetable$Year==birthyear,]
  lifetable <- lifetable$dx[1:101]/100000
}


# END functions **************************************************** 

#external input
if (workstation =="laptop") { lifetable_file <- "C:\\Users\\Tiago\\Documents\\Skinvision\\SkinVisionModel\\lifetables_NL_5y.txt" }
#if (workstation == "work")  { lifetable_file  <- "U:\\ProjectJuneSept2019\\Skin Cancer Model\\R\\lifetables_NL_5y.txt"}
if (workstation == "cloud")  { lifetable_file <- "/cloud/project/Excel/lifetables_NL_5y.txt"}
if(workstation=="work") { lifetable_file <- "N:\\Documents\\Metamodels\\Excel\\lifetables_NL_5y.txt"}

year_deathprob <- matrix(0,101,ncohorts)
yearzero <- matrix(0,ncohorts,1)
birth_year_cohort <- matrix(0,ncohorts,1)
birthcohortsize <- 5 # how many calendar years does one lifetable cover (births)


try(if(end_birthyear[1] - start_birthyear[1] != 4) stop("Incorrect Birth Cohort!!! Needs to be 5y!"))
for (ch in (1:ncohorts)) {
  birth_year_cohort[ch] <- paste(toString(start_birthyear[ch]), "-", toString(end_birthyear[ch]),sep="")
  year_deathprob[,ch] <- read_lifetable(lifetable_file,birth_year_cohort[ch])
  yearzero[ch] <- followup_years[1] - start_birthyear[ch] + 1
  # sample people who are alive at begin FU (plus survive at least one year)
}

# end of file, lifetables should be stored in memory during all runs. 
