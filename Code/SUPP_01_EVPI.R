############################################################################
###The value of information in age-prioritization of COVID-19 vaccination###
############################################################################

###Authors: Damie Pak (Corresponding), Emily Howerton ,William J M Probert,
###Michael C. Runge, Ruiyun Li, Rebecca Borchering,
###Cecile Viboud, Ottar N. Bj?rnstad, and Katriona Shea

#This is the R Script for Pak et al. Covid Paper.
#This has been now revised to have a newer methodology 
#where I use priors instead of baseline value. Last edited on 2/12/2023

####################
###Packages needed##
####################

library(here) # Working directory
library(ggplot2) #Main plotting package
library(deSolve) #Differential equation solver
library(viridis) #Color palette 
library(patchwork) #Multiplot arranger

###################################
###Source functions ###############
###################################

###FULL_MODEL_COVID19.R: 
###The Full Desolve Model (This is the main
###model. If you're most interested in the model itself, o
###open up this script)
source(here("Code","Math_Model","FULL_MODEL_COVID19.R"))

###The Simulator_MODEL.R: Allows me to put in different parameter values to run the above model
source(here("Code","Helper_Functions","FUNC_00_SIMULATOR_MODEL.R"))

###"output_related.R": Output Calculator/Grapher
###and is mainly used for Figure 2 (Which is the
###figure explaining what the best ge prioritization
###is )
source(here("Code","Helper_Functions","FUNC_00_output_related.R"))

###This is for the permutation of country/demography
source(here("Code","Helper_Functions","FUNC_00_Simulator_Cases_NewPriors.R"))

###Demography
source(here("Code","Helper_Functions", "LOADER_00_demography.R"))

######################################
###Begin analysis here for Figure 1###
######################################
sus_l_h <-   c(0.10, 0.5, 1) #Values of susceptibility (3 values)
trans_l_h <- c(0.10, 0.5,1)   #Values of transmissibility (3 values)
sev_l_h  <- c(0.1, 0.25, 0.5) #Values of severity (3 values)
WN_l_h <- c(0, 0.5, 1, 2)     #Values of the waning of immunity  (4 values)
WV1_l_h <- c(0, 0.5, 1, 2)    #Values of the waning of vaccine immunity 1 (4 values)
WV2_l_h <- c(0, 0.5, 1, 2) #Values of the waning of vaccine immunity 1 (4 values)

#######################
###Creating the priors# 
#######################
###Equal priors for the first three uncertainties: 
###because three values for sus/trans/sev

Equal_Priors_3 <- c(1/3,1/3,1/3)

###Eaual priors for the last three uncertaines uncertainties: 
###because three values for WN/SV1/SV2

Equal_Priors_4 <- c(1/4,1/4,1/4,1/4)

###For the simulator, we need to feed it the data.frame including
###the identification parameters (id), the associating values (see line 86-91)
Equal_DF <- cbind.data.frame(
            id = c(rep(c('sus','trans','sev'), each =3),
                   rep(c('wn','wv1','wv2'), each=4)),
            value = as.numeric(c(sus_l_h, trans_l_h, sev_l_h,
                    WN_l_h , WV1_l_h, WV2_l_h)),
            priors = c(rep(Equal_Priors_3,3),
                    c(rep(Equal_Priors_4,3))))

###Default 

default_yprop <- 
  c(S = 0.95, #susceptibles 
    SV = 0, #susceptible vaccinated
    SU = 0, #susceptibles unvaccinated 
    A = 0.01, #asymptomatic infected
    I =  0.02 , #symptomatic infected
    AV = 0 , #asymptomatic vaccinated
    IV = 0 , #asymptomatic unvaccinated
    R = 0.02, #recovered
    RV = 0 , #recovered vaccinated 
    KI_U = 0, 
    KI_V = 0 ,
    KA = 0 ,
    VacS= 0 ,
    VacR = 0 )


EQUAL_SINGLE <- Simulator_Cases_NewPriors(Equal_DF,default_yprop  )
EQUAL_SINGLE_DF <- PARM_GETTER(Equal_DF) ###Some wonkiness where I have to make a data.frame version

EQUAL_OUTPUT <- NULL 

for (k in seq(1,6)){
    tmp <-  EQUAL_SINGLE[[k]]
    tmp_df <- EQUAL_SINGLE_DF [[k]]
    tmp_output <- Output_Calculator(tmp, tmp_df,1)
    colnames(tmp_output)[7:11] <- c("vacrate","vacall",'inc','R0','country')
  EQUAL_OUTPUT [[k]] <-  tmp_output
}

###I know better now but we are going through each parameter 

###################
###SUSCEPTIBILITY##
###################
SUS_OUTPUT_GG <- grapher_output(
  x = Minimizer_Highlighter(EQUAL_OUTPUT[[1]],'sus'),'sus','not')

###Cases
(SUS_OUTPUT_GG[[1]]+ ggtitle("Susceptibility: Total Cases per 100,000 (Japan)"))/
(SUS_OUTPUT_GG[[2]]+ ggtitle("Susceptibility: Total Cases per 100,000 (South Africa)"))/
(SUS_OUTPUT_GG[[3]]+ ggtitle("Susceptibility:Total Cases per 100,000 (United States)"))

ggsave(
  "SUS_CASES_JSU.pdf",
  width = 10,
  height = 14,
  units = c("in"),
  dpi = 300)

###Mortality
(SUS_OUTPUT_GG[[4]]+ ggtitle("Susceptibility: Total Mortality per 100,000 (Japan) "))/
(SUS_OUTPUT_GG[[5]]+ ggtitle("Susceptibility: Total Mortality per 100,000 (South Africa) "))/
(SUS_OUTPUT_GG[[6]]+ ggtitle("Susceptibility: Total Mortality per 100,000 (United States) "))

ggsave(
  "SUS_MORT_JSU.pdf",
  width = 10,
  height = 14,
  units = c("in"),
  dpi = 300)

######################
###TRANSMISSIBILITY###
######################

###This is the transmissibility paramater that determines how
###the vaccine reduces transmissibility. A high reduction is 0.1 (90% reduction),
###a medium reduction is 0.5 (50% reduction) and 1 is a no reduction (0% reduction).
###This term appears in both the unvaccinated susceptible and vaccinated susceptible
###forces of infection. 

TRANS_OUTPUT_GG <- grapher_output(
  x= Minimizer_Highlighter(EQUAL_OUTPUT[[2]],'trans'),'trans','not')

###Cases
(TRANS_OUTPUT_GG[[1]]+ ggtitle("Transmissibility: Total Cases per 100,000 (Japan) "))/
(TRANS_OUTPUT_GG[[2]]+ ggtitle("Transmissibility: Total Cases per 100,000 (South Africa) "))/
(TRANS_OUTPUT_GG[[3]]+ ggtitle("Transmissibility:Total Cases per 100,000 (United States) "))

ggsave(
  "TRANS_CASES_JSU.pdf",
  width = 10,
  height = 14,
  units = c("in"),
  dpi = 300)

###Mortality
(TRANS_OUTPUT_GG[[4]]+ ggtitle("Transmissibility: Total Mortality per 100,000 (Japan) "))/
(TRANS_OUTPUT_GG[[5]]+ ggtitle("Transmissibility: Total Mortality per 100,000 (South Africa) "))/
(TRANS_OUTPUT_GG[[6]]+ ggtitle("Transmissibility: Total Mortality per 100,000 (United States) "))

ggsave(
  "TRANS_MORT_JSU.pdf",
  width = 10,
  height = 14,
  units = c("in"),
  dpi = 300)

##############
###Severity###
##############

###The severity is the parameter that controls the probability of a vaccinated 
###individual going to the asymptomatic state or the symptomatic state.
###For the 0.1, that means this is low severity (10% become symptomatic). 
###For that 0.25, that means this is medium severity (25% become symptomatic). 
###for that 0.50 that means this is still high severity (50% become symptomatic)



SEV_OUTPUT_GG <- grapher_output(Minimizer_Highlighter(EQUAL_OUTPUT[[3]],'sev') ,uncertainty='sev','not')

###Cases
(SEV_OUTPUT_GG[[1]]+ ggtitle("Severity: Total Cases per 100,000 (Japan) "))/
  (SEV_OUTPUT_GG[[2]]+ ggtitle("Severity: Total Cases per 100,000 (South Africa) "))/
  (SEV_OUTPUT_GG[[3]]+ ggtitle("Severity:Total Cases per 100,000 (United States) "))

ggsave(
  "SEV_CASES_JSU.pdf",
  width = 10,
  height = 14,
  units = c("in"),
  dpi = 300)

###Mortality
(SEV_OUTPUT_GG[[4]]+ ggtitle("Severity: Total Mortality per 100,000 (Japan) "))/
(SEV_OUTPUT_GG[[5]]+ ggtitle("Severity: Total Mortality per 100,000 (South Africa) "))/
(SEV_OUTPUT_GG[[6]]+ ggtitle("Severity: Total Mortality per 100,000 (United States) "))


ggsave(
  "SEV_MORT_JSU.pdf",
  width = 10,
  height = 14,
  units = c("in"),
  dpi = 300)


##############################
###Natural Waning of immunity#
##############################

#The natural immunity parameter controls how fast the 
#unvaccinated, recovered individuals lose their immunity (R-> S).
#0 means life-long immuntiy, 0.5 means that they lose it in two year, 
#1 means that they lose it in one year, 2 means they lose it in six months

WN_OUTPUT_GG <- grapher_output(Minimizer_Highlighter(EQUAL_OUTPUT[[4]] ,'wn') ,uncertainty='wn','vac')

###Cases
(WN_OUTPUT_GG[[1]]+ ggtitle("Waning immunity (N): Total Cases per 100,000 (Japan) "))/
  (WN_OUTPUT_GG[[2]]+ ggtitle("Waning immunity (N): Total Cases per 100,000 (South Africa) "))/
  (WN_OUTPUT_GG[[3]]+ ggtitle("Waning immunity (N): Total Cases per 100,000 (USA) "))

ggsave(
  "WN_CASES_JSU.pdf",
  width = 10,
  height = 14,
  units = c("in"),
  dpi = 300)

###Mortality
(WN_OUTPUT_GG[[4]]+ ggtitle("Waning immunity (N): Total Mortality per 100,000 (Japan) "))/
  (WN_OUTPUT_GG[[5]]+ ggtitle("Waning immunity (N): Total Mortality per 100,000 (South Africa) "))/
  (WN_OUTPUT_GG[[6]]+ ggtitle("Waning immunity (N): Total Mortality per 100,000 (USA) "))

ggsave(
  "WN_MORT_JSU.pdf",
  width = 10,
  height = 14,
  units = c("in"),
  dpi = 300)


###########################
###Vaccinated Immunity (1)#
############################


#The vaccinated immunity parameter controls how fast the vaccinated, 
#susceptibles individuals lose their immunity (SV-> S). 0 means life-long immuntiy,
#0.5 means that they lose it in two year, 1 means that they lose it in one year, 
#2 means they lose it in six months

WV1_OUTPUT_GG <-
  
  grapher_output(Minimizer_Highlighter(EQUAL_OUTPUT[[5]] ,'wv1') ,uncertainty='wv1','vac')

###Cases
(WV1_OUTPUT_GG[[1]]+ ggtitle("Waning immunity (1): Total Cases per 100,000 (Japan) "))/
(WV1_OUTPUT_GG[[2]]+ ggtitle("Waning immunity (1): Total Cases per 100,000 (South Africa) "))/
(WV1_OUTPUT_GG[[3]]+ ggtitle("Waning immunity (1): Total Cases per 100,000 (USA) "))

ggsave(
  "WV1_CASES_JSU.pdf",
  width = 10,
  height = 14,
  units = c("in"),
  dpi = 300)

#Mortality
(WV1_OUTPUT_GG[[4]]+ ggtitle("Waning immunity (1): Total Mortality per 100,000 (Japan) "))/
(WV1_OUTPUT_GG[[5]]+ ggtitle("Waning immunity (1): Total Mortality per 100,000 (South Africa) "))/
(WV1_OUTPUT_GG[[6]]+ ggtitle("Waning immunity (1): Total Mortality per 100,000 (USA) "))

ggsave(
  "WV1_MORT_JSU.pdf",
  width = 10,
  height = 14,
  units = c("in"),
  dpi = 300)

######################################
###Vaccinated Waning of Immunity  2###
######################################

#The vaccinated immunity parameter controls how fast the vaccinated, 
#recovered individuals lose their immunity (RV-> SV). 0 means life-long immuntiy, 
#0.5 means that they lose it in two year, 1 means that they lose it in one year, 
#2 means they lose it in six months

WV2_OUTPUT_GG  <- grapher_output(Minimizer_Highlighter(EQUAL_OUTPUT[[6]],'wv2'),
                                 'wv2','vac')

###Cases
(WV2_OUTPUT_GG[[1]]+ ggtitle("Waning immunity (2): Total Cases per 100,000 (Japan) "))/
(WV2_OUTPUT_GG[[2]]+ ggtitle("Waning immunity (2): Total Cases per 100,000 (South Africa) "))/
(WV2_OUTPUT_GG[[3]]+ ggtitle("Waning immunity (2): Total Cases per 100,000 (USA) "))

ggsave(
  "WV2_CASES_JSU.pdf",
  width = 10,
  height = 14,
  units = c("in"),
  dpi = 300)

###Mortality
(WV2_OUTPUT_GG[[4]]+ ggtitle("Waning immunity (2): Total Mortality per 100,000 (Japan) "))/
(WV2_OUTPUT_GG[[5]]+ ggtitle("Waning immunity (2): Total Mortality per 100,000 (South Africa) "))/
(WV2_OUTPUT_GG[[6]]+ ggtitle("Waning immunity (2): Total Mortality per 100,000 (USA) "))

ggsave(
  "WV2_MORT_JSU.pdf",
  width = 10,
  height = 14,
  units = c("in"),
  dpi = 300)

##End supplementary figures

