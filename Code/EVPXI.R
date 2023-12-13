
###################################
###Source functions ###############
###################################

###FULL_MODEL_COVID19.R: 
###The Full Desolve Model (This is the main
###model. If you're most interested in the model itself, o
###open up this script)
source(here("Code","Math_Model","FULL_MODEL_COVID19.R"))

###The Simulator_MODEL.R: Allows me to put in different parameter values to run the above model
source(here("Code","Helper_Functions","SIMULATOR_MODEL.R"))

###"output_related.R": Output Calculator/Grapher
###and is mainly used for Figure 2 (Which is the
###figure explaining what the best ge prioritization
###is )
source(here("Code","Helper_Functions","output_related.R"))

###This is for the permutation of country/demography
source(here("Code","Helper_Functions","Simulator_Cases_NewPriors.R"))

###
source(here("Code","Helper_Functions", "00_demography.R"))

######################################
###Begin analysis here for Figure 1###
######################################
sus_l_h <-   c(0.10, 0.5, 1) #Values of susceptibility (3 values)
trans_l_h <- c(0.10, 0.5,1)   #Values of transmissibility (3 values)
sev_l_h  <- c(0.1, 0.25, 0.5) #Values of severity (3 values)
WN_l_h <- c(0, 0.5, 1, 2)     #Values of the waning of immunity  (4 values)
WV1_l_h <- c(0, 0.5, 1, 2)    #Values of the waning of vaccine immunity 1 (4 values)
WV2_l_h <- c(0, 0.5, 1, 2) #Values of the waning of vaccine immunity 1 (4 values)
