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
source("Model/FULL_MODEL_COVID19.R")

###The Simulator_MODEL.R: Allows me to put in different parameter values to run the above model
source("Model/Simulator_MODEL.R")

###"output_related.R": Output Calculator/Grapher
###and is mainly used for Figure 2 (Which is the
###figure explaining what the best ge prioritization
###is )
source("output_related.R")

###EVPI_TOTAL_related.R"
source("EVPI_TOTAL_related.R")


###This is for the permutation of country/demography
source('Simulator_Cases_NewPriors.R')

####################################################
###Demography and contact matrices - From Ruiyun Li#
####################################################
###USA (United States)
load("Demography/D_USA.RData")
load("Demography/W_USA.RData")
W_USA[W_USA < 0 ]<-0 ###Ensure nothing's negative

###JAPAN
load("Demography/D_JPN.RData")
load("Demography/W_JPN.RData")
W_JPN[W_JPN < 0 ]<-0 ###Ensure nothing's negative

###SOUTH AFRICA
load("Demography/D_SAF.RData")
load("Demography/W_SAF.RData")
W_SAF[W_SAF < 0 ]<-0 ###Ensure nothing's negative

###Normalize these to 1
D_JPN <- D_JPN/(sum(D_JPN))
###Normalize these to 1
D_SAF <- D_SAF/(sum(D_SAF))

###EVPI_ALL.R: For analysis of EVPI for the
###total country
source('EVPI_ALL.R')
load("TSS_JPN_ALL.RData")
load("TSS_US_ALL.RData")
load("TSS_SAF_ALL.RData")

######################################
###Begin analysis here for Figure 2###
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
###Eaual priors for the first three uncertainties: 
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
  value=  as.numeric(c(sus_l_h, trans_l_h, sev_l_h,
                       WN_l_h , WV1_l_h, WV2_l_h)),
  priors = c(rep(Equal_Priors_3,3),
             c(rep(Equal_Priors_4,3))))

EQUAL_SINGLE <- Simulator_Cases_NewPriors(Equal_DF)
EQUAL_SINGLE_DF <-Simulator_Cases_NewPriors_DF(Equal_DF) ###Some wonkiness where I have to make a data.frame version

EQUAL_OUTPUT =NULL 
for (k in seq(1,6)){
  tmp = EQUAL_SINGLE[[k]]
  tmp_df =EQUAL_SINGLE_DF [[k]]
  tmp_output <- Output_Calculator(tmp, tmp_df,1)
  colnames(tmp_output)[7:11] <-
    c("vacrate","vacall",'inc','R0','country')
  EQUAL_OUTPUT [[k]]=tmp_output
}

###I know better now but we are going through each parameter 

###################
###SUSCEPTIBILITY##
###################
SUS_OUTPUT_GG <- grapher_output(
  x= Minimizer_Highlighter(EQUAL_OUTPUT[[1]],'sus'),'sus','not')

###Cases
(SUS_OUTPUT_GG[[1]]+ ggtitle("Susceptibility: Total Cases per 100,000 (Japan)"))/
(SUS_OUTPUT_GG[[2]]+ ggtitle("Susceptibility: Total Cases per 100,000 (South Africa)"))/
(SUS_OUTPUT_GG[[3]]+ ggtitle("Susceptibility:Total Cases per 100,000 (United States)"))

#ggsave(
#  "SUS_CASES_JSU.png",
#  width = 10,
#  height = 14,
#  units = c("in"),
#  dpi = 300)

###Mortality
(SUS_OUTPUT_GG[[4]]+ ggtitle("Susceptibility: Total Mortality per 100,000 (Japan) "))/
(SUS_OUTPUT_GG[[5]]+ ggtitle("Susceptibility: Total Mortality per 100,000 (South Africa) "))/
(SUS_OUTPUT_GG[[6]]+ ggtitle("Susceptibility: Total Mortality per 100,000 (United States) "))

#ggsave(
#  "SUS_MORT_JSU.png",
#  width = 10,
#  height = 14,
#  units = c("in"),
#  dpi = 300)

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

#ggsave(
#  "TRANS_CASES_JSU.png",
#  width = 10,
#  height = 14,
#  units = c("in"),
#  dpi = 300)

###Mortality
(TRANS_OUTPUT_GG[[4]]+ ggtitle("Transmissibility: Total Mortality per 100,000 (Japan) "))/
(TRANS_OUTPUT_GG[[5]]+ ggtitle("Transmissibility: Total Mortality per 100,000 (South Africa) "))/
(TRANS_OUTPUT_GG[[6]]+ ggtitle("Transmissibility: Total Mortality per 100,000 (United States) "))

#ggsave(
#  "TRANS_MORT_JSU.png",
#  width = 10,
#  height = 14,
#  units = c("in"),
#  dpi = 300)

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

#ggsave(
#  "SEV_CASES_JSU.png",
#  width = 10,
#  height = 14,
#  units = c("in"),
#  dpi = 300)

###Mortality
(SEV_OUTPUT_GG[[4]]+ ggtitle("Severity: Total Mortality per 100,000 (Japan) "))/
(SEV_OUTPUT_GG[[5]]+ ggtitle("Severity: Total Mortality per 100,000 (South Africa) "))/
(SEV_OUTPUT_GG[[6]]+ ggtitle("Severity: Total Mortality per 100,000 (United States) "))


#ggsave(
#  "SEV_MORT_JSU.png",
#  width = 10,
#  height = 14,
#  units = c("in"),
#  dpi = 300)


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
  "WN_CASES_JSU.png",
  width = 10,
  height = 14,
  units = c("in"),
  dpi = 300)

###Mortality
(WN_OUTPUT_GG[[4]]+ ggtitle("Waning immunity (N): Total Mortality per 100,000 (Japan) "))/
  (WN_OUTPUT_GG[[5]]+ ggtitle("Waning immunity (N): Total Mortality per 100,000 (South Africa) "))/
  (WN_OUTPUT_GG[[6]]+ ggtitle("Waning immunity (N): Total Mortality per 100,000 (USA) "))

#ggsave(
#  "WN_MORT_JSU.png",
#  width = 10,
#  height = 14,
#  units = c("in"),
#  dpi = 300)


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

#ggsave(
#  "WV1_CASES_JSU.png",
#  width = 10,
#  height = 14,
#  units = c("in"),
#  dpi = 300)

#Mortality
(WV1_OUTPUT_GG[[4]]+ ggtitle("Waning immunity (1): Total Mortality per 100,000 (Japan) "))/
(WV1_OUTPUT_GG[[5]]+ ggtitle("Waning immunity (1): Total Mortality per 100,000 (South Africa) "))/
(WV1_OUTPUT_GG[[6]]+ ggtitle("Waning immunity (1): Total Mortality per 100,000 (USA) "))

#ggsave(
#  "WV1_MORT_JSU.png",
#  width = 10,
#  height = 14,
#  units = c("in"),
#  dpi = 300)

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

#ggsave(
#  "WV2_CASES_JSU.png",
#  width = 10,
#  height = 14,
#  units = c("in"),
#  dpi = 300)

###Mortality
(WV2_OUTPUT_GG[[4]]+ ggtitle("Waning immunity (2): Total Mortality per 100,000 (Japan) "))/
(WV2_OUTPUT_GG[[5]]+ ggtitle("Waning immunity (2): Total Mortality per 100,000 (South Africa) "))/
(WV2_OUTPUT_GG[[6]]+ ggtitle("Waning immunity (2): Total Mortality per 100,000 (USA) "))

#ggsave(
#  "WV2_MORT_JSU.png",
#  width = 10,
#  height = 14,
#  units = c("in"),
#  dpi = 300)

##End supplementary figures

##################################
###Super grid expanded countries##
##################################

#Instead of looking at one uncertainty at a time, now we run the model 
#with all different combinations of the six uncertainties (it is a very large
#file because like combinatorics). This makes Figure 2
#The data set are all  found in the TSS_ALL_CREATOR FILE 

#For easier analysis, I decided to split it up by country-
#  looked at United States (first)

#Var1 = Susceptibility
#Var2 = Transmissibility
#Var 3 = Severity
#Var 4 = Waning of Natural Immunity
#Var 5 = Waning of vaccine immunity 1
#Var 6 = Waning of vaccine immunity 2
#Var 7 = Vaccine rate 
#Var 8 = Vaccination coverage
#Var 9 = Priority group(the action)
#Var 10 = R0
#Var 11 = Country

###UNITED STATES

### This splits up the data based on the R0 (Var10) AND vaccine rate (Var7)
TSS_US_ALL$mort <- round(TSS_US_ALL$mort,2)


TSS_US_ALL_SPLIT <- split(
  TSS_US_ALL, #The US
  list(
    TSS_US_ALL$R0, # R0
    TSS_US_ALL$vacrate   # Vaccination rate
  )
)
###############################################################
###This looks at the total EVPI across all the uncertainties###                       ###########
###############################################################

TSS_EVPI_US <- lapply(TSS_US_ALL_SPLIT, function(x) EVPI_ALL_SUPER(x))

### We add in the different R0 and vacrate
EVPI_US_F <- cbind.data.frame(
  R0 = rep(c(1.15, 1.5, 2.5), 3),
  vacrate = rep(c(0.287, 0.69, 1.38), each = 3),
  do.call(rbind, TSS_EVPI_US)
)


vacrate_labellar <- c("25%","50%","75%")

###This makes one part of the figure 
##############################
###Cases for United States####
##############################
EVPI_US <- 
  ggplot(EVPI_US_F, aes(x = as.factor(R0), y = abs(evpi))) +
  geom_bar(stat = "identity", 
           fill = "#0d2848") +
  facet_wrap(~vacrate, 
             labeller = as_labeller(c(`0.287` = "25%",
                                      `0.69` = "50%",
                                      `1.38` = "75%")))+
  geom_hline(yintercept= 0)+
  geom_vline(xintercept = 0.4 ) +
  xlab("R0") +
  ylab("EVPI (Cases)") +
  ylim(0, 900) +
  ggtitle("USA")+
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 14, color = 'black'),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 15, color = 'black'),
        strip.text = element_text(size = 16),
        panel.border=element_blank())  

###This makes one part of the figure 
###################################
###Mortality for United States#####
###################################
EVPI_US_M <- ggplot(EVPI_US_F, aes(x = as.factor(R0), y = abs(evpi_m))) +
  geom_bar(stat = "identity", fill = "#0d2848",color = 'black', size = 1) +
  facet_wrap(~vacrate, 
             labeller = as_labeller(c(`0.287` = "25%",
                                      `0.69` = "50%",
                                      `1.38` = "75%")))+
  geom_hline(yintercept= 0)+
  geom_vline(xintercept = 0.4 ) +
  xlab("R0") +
  ylab("EVPI (Mortality)") +
  ylim(0, 1)+
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 14, color = 'black'),
   
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 15, color = 'black'),
        strip.text = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        panel.border=element_blank())  


  

#Combine
#EVPI_US / EVPI_US_M                                                                                    

###########################
###South Africa ###########
###########################

TSS_SAF_ALL_SPLIT <- split(
  TSS_SAF_ALL,
  list(
    TSS_SAF_ALL$R0, # R0
    TSS_SAF_ALL$vacrate # Vaccination rate
  )
)

TSS_EVPI_SAF <- lapply(TSS_SAF_ALL_SPLIT, function(x) EVPI_ALL_SUPER(x))

EVPI_SAF_F <- cbind.data.frame(
  R0 = rep(c(1.15, 1.5, 2.5), 3),
  vacrate = rep(c(0.287, 0.69, 1.38), each = 3),
  do.call(rbind, TSS_EVPI_SAF)
)


###This makes one part of the figure 
###################################
###Cases for South Africa     #####
###################################
EVPI_SAF <- ggplot(EVPI_SAF_F, aes(x = as.factor(R0), y = abs(evpi))) +
  geom_bar(stat = "identity", fill = "#009d9a") +
  facet_wrap(~vacrate, 
             labeller = as_labeller(c(`0.287` = "25%",
                                      `0.69` = "50%",
                                      `1.38` = "75%")))+ 
  geom_hline(yintercept= 0)+
  geom_vline(xintercept = 0.4 ) +
  xlab("R0") +
  ylab("") +
  ylim(0, 900) +
  ggtitle("South Africa")+
  theme_bw() +
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 14, color = 'black'),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 15, color = 'black'),
        strip.text = element_text(size = 16),
        panel.border=element_blank())  

  

###################################
###Mortality for South Africa #####
###################################
EVPI_SAF_M <- ggplot(EVPI_SAF_F, aes(x = as.factor(R0), y =abs(evpi_m))) +
  geom_bar(stat = "identity", fill = "#009d9a",color = 'black', size = 1) +
  facet_wrap(~vacrate, 
             labeller = as_labeller(c(`0.287` = "25%",
                                      `0.69` = "50%",
                                      `1.38` = "75%")))+ 
  geom_hline(yintercept= 0)+
  geom_vline(xintercept = 0.4) +
  xlab("R0") +
  ylab("") +
  ylim(0, 1)+
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 14, color = 'black'),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 15, color = 'black'),
        strip.text = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        panel.border=element_blank())  



  

#EVPI_SAF / EVPI_SAF_M                                                                             

######################################
###JAPAN (look above for comments )###
######################################

TSS_JPN_ALL_SPLIT <- split(
  TSS_JPN_ALL,
  list(
    TSS_JPN_ALL$vacrate,
    TSS_JPN_ALL$R0
  )
)

TSS_EVPI_JPN <- lapply(TSS_JPN_ALL_SPLIT, function(x) EVPI_ALL_SUPER(x))

EVPI_JPN_F <- cbind.data.frame(
  R0 = rep(c(1.15, 1.5, 2.5), 3),
  vacrate = rep(c(0.287, 0.69, 1.38), each = 3),
  do.call(rbind, TSS_EVPI_JPN)
)


###This makes one part of the figure 
######################
###Cases for Japan  #          
#####################
EVPI_JPN <- ggplot(EVPI_JPN_F, aes(x = as.factor(R0), y = abs(evpi))) +
  geom_bar(stat = "identity", fill = "#9f1853") +
  facet_wrap(~vacrate, 
             labeller = as_labeller(c(`0.287` = "25%",
                                      `0.69` = "50%",
                                      `1.38` = "75%")))+ 
  geom_hline(yintercept= 0)+
  geom_vline(xintercept = 0.4 ) +
  xlab("R0") +
  ylab("Cases") +
  ylim(0, 900) +
  ggtitle("Japan")+
  theme_bw()+
theme(strip.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5),
      axis.text = element_text(size = 14, color = 'black'),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title = element_text(size = 15, color = 'black'),
      strip.text = element_text(size = 16),
      panel.border=element_blank())  

  
###This makes one part of the figure 
######################
###Cases for Japan  #          
#####################
EVPI_JPN_M <- ggplot(EVPI_JPN_F, aes(x = as.factor(R0), y = abs(evpi_m))) +
  geom_bar(stat = "identity", fill = "#9f1853") +
  facet_wrap(~vacrate,
             labeller = as_labeller(c(`0.287` = "25%",
                                      `0.69` = "50%",
                                      `1.38` = "75%"))) +
  geom_hline(yintercept= 0)+
  geom_vline(xintercept = 0.4 ) +
  xlab("R0") +
  ylab("EVPI (Mortality)") +
  ylim(0, 1)+
  theme_bw()+
  theme(strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 14, color = 'black'),
        axis.title = element_text(size = 15, color = 'black'),
        strip.text = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        panel.border=element_blank())  
#EVPI_JPN / EVPI_JPN_M                                                                             


###THIS IS FIGURE 2: TOTAL EVPI THAT I PUT THROUGH ILLUSTRATOR
###BUT SHOULD BE THE SKELETON
(EVPI_JPN + EVPI_SAF + EVPI_US) / 
  (EVPI_JPN_M + EVPI_SAF_M + EVPI_US_M)


ggsave("EVPI_TOTAL.pdf", width = 12, height = 5, units = 'in')
##############################################
###EVPXI (Expected value of partial perfect )#
##############################################
#For the expected value of partial perfect information
#I kept this in a seperate script to make it easier
#to read 
#You can find it as Main_Analysis_EVPXI.R and this is used to create 
#FIGURE 2B                                            

###################
#END SCRIPT HERE###
###################
