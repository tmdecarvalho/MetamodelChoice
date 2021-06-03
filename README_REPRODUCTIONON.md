# Metamodel Choice :  This READ ME is for reproduction of runs in metamodels article
## Title: Choosing a  metamodel of a simulation model for uncertainty quantification.
## Authors:  Tiago M de Carvalho, Joost van Rosmalen, Harold B Wolff ,  Hendrik Koffijberg , Veerle MH Coupé .
## Published: 2021, Medical Decision Making

### 1. Inputs, Scenarios and Model Outcomes

##### Inputs: N, M, n_datasets , n_expdes, cv_fold, max(t_effectvar) 

##### default values
n_datasets <-50   (in the article n_datasets is D)

N <- 5000

M <- 10000

n_expdes <-100     (in the article n_expdes is S)   

cv_fold <- 10

max(t_effectvar) <- 0.3  (depends on N ,  N=500 -> 0.5 , N=100 -> 0.8)

##### Scenarios:  Main, ParamV1/ParamV2, Calib 

(Note: with RunAnalyses_v4_SM : SemiM… notice there is an extra model parameter in this scenario)

##### Model outcomes: 14  (used in article)  7 (alternative). 
 model outcomes explainer:
 
1 to 5 : outcomes without absence of treatment 

1:other cause (oc) death, 2: disease specific (ds) death, 3: clinical diagnosis (clin dx) ,  4: life years (ly) , 5: discounted life years (d.ly), 

6 to 10: same outcomes as 1 to 5 but with treatment

11: difference with and without treatment (diff) oc death , 12: diff ds death, 13: diff clin dx, 14: diff ly , 15: diff d.ly


************* ---------------------------------------------------------------------------  ********************


###  2. Workflow of the Simulation Model Code

#### A. Run the Simulation
run RunAnalyses_v4.R (once)

read_lifetables_aut.R  -> reads lifetables_NL_5y.txt  : reads lifetables for death other causes

run n_datasets times  {

 SimulationModel_v8.R -> runs the simulation model once generates an individual patient dataset and estimates model parameters with msm
 
 TrainingData_v8.R   --> create Training Data (n_expdes times) with and without effect of treatment (note: it runs twice)
 
 UncertAnalys_v5_aut.R --> uses the training data to fit the metamodel
 
}

gen_experimentaldesign.R --> generates experimental design used in training this is done to ensure we dont need to save experimental design during the loop.

outputs: list("expdes_trueP"=exp_des_orig,"estimated params"=estim_model_params, "expdes"=exp_des,"unc_bounds"=upperlower,"y1"=y_training1,"outcome1"=accuracy_table1 )

#### B. Compute Statistics on Simulation
Open StatisticsfromLoadedData_v1.R

Load outputs 

Compute statistics of interest

Print table ready to copy/paste to article

#### C. Build Figures

Open FigurefromLoadedData_v3.R

Read excel output from runanalyses_v4

Print Figures (namely Figures 2 and 3 in the article)

************* ---------------------------------------------------------------------------  ********************

###  3.  Workflow of the computation time simulation study 

inputs:  n_expdes, nparams 

n_datasets <- 1

N <- 5000

M <- 10000

n_expdes <- 50, 100, 200

note: we are not interested in the model fits, so we can set all fitmodels to FALSE. This will fit only the linear model. 

Scenarios:  there are 4 scenarios:  figure A:  p5n50, p10n50 , figure B: p10n100, p10n200

run RunAnalyses_v4.R (once per scenario)

run Computationtime_v3 

-	Uses microbenchmark to track computation time for each metamodel

-	Plots figures 

Outputs:

list("n50p10"=computation_times_df1,  "n50p5"= computation_times_df4)

list("n100p10"=computation_times_df2,"n200p10"=computation_times_df3)

Figure A, Figure B  (included in Online Supplement)


************* ---------------------------------------------------------------------------  ********************


###  3.  Workflow of the Case Study

Run UncertAnalys_v5_data.r

Read Dataset: LC_PSA_dataset.xlsx

(note: we can choose between a n=220 or n=440 experimental design , corresponding to 10 x p and 20 x p  runs.)

Scenarios : 3, 4, 8, 9  (these correspond to model outcomes, total costs VATS, SBRT ;  QALYs VATS,  SBRT)

Outputs: PSAdata_outcome_8_nexpdes_220_v2

Run Figure_CaseStudy_v1.r

Output: Figure A (with 8 metamodels observed vs predicted)


************* ---------------------------------------------------------------------------  ********************

END*
