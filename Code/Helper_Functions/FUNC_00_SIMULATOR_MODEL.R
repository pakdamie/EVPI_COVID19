############################################################
###This is a function for simulating the model with        #
###different parameters values- specifically,              #
###this is the code that we use to figure out the different#
###output with the different values ########################
############################################################
###Cleaned and Last updated 7/29/2021
Simulator_model <- function(sus1,trans1,sev,WN,
                            WV_1,WV_2,vacrate,
                            vacall, 
                            inc, 
                            R0,
                            country,
                            y_initial_prop){
  
  ##This determines what country one is looking at and
  #ensures we're using the same demography and contact rate
  
   Rec <- switch(as.character(country),
                      "USA" =  D_USA,
                      "JPN" =  D_JPN,
                      "SAF" =  D_SAF,
                      stop("Invalid input"))
   
   Contact <- switch(as.character(country),
                     "USA" =  W_USA,
                     "JPN" =  W_JPN,
                     "SAF" =  W_SAF,
                     stop("Invalid input"))
  
  ###This is the initial population that is also
  ###dependent on the country's demography
  ###
  
  #We start with 95% of the population as being
  #susceptible, 1% asymtomatically infectious (unvac),
  #2% symptomatically infectious and 2% recovered
  
   
  y_initial <- unlist(lapply(y_initial_prop, function(x) x * Rec))
  
  
  ### The priority group- Age1 (18-30 year olds), Age2 (31-59
  ###year olds),and Age3 (60-80 year olds)
  
  if (inc == "age1") {
    age1 <- seq(18, 30)
    inc <- age1
    prop_age <- sum(Rec[18:30])
    
  } else if (inc == "age2") {
    age2 <- seq(31, 59)
    inc <- age2
    prop_age <- sum(Rec[31:59])
    
  } else if (inc == "age3") {
    age3 <- seq(60, 80)
    inc <- age3
    prop_age <- sum(Rec[60:80])
  }
  
  ###The parameters that we feed in
  parms <-  list(n = 80, #The age compartments
              sus = sus1,  #Susceptibility multiplier
              tran = trans1, #Transmissibility multiplier
              gamma =365/14 , #recovery rate (average = 14 Days)
              pv = 0.98,  #vaccination success (98% effectiveness)
              p_i = 0.80, #symptomatic probability when not vaccinated (20% )
              p_iv = sev, #symptomatic probability when vaccinated
              W = Contact, #The contact matrix of interest
              mu = 1/80, #Mortality rate (inverse of the maximum life-span)
              a = rep(1,80), #Ageing Rate
              N = 1, #The total population
              R0 = R0, # The Reproductive Number
              omega_N = WN, #Waning of natural immunity #365/180- default is 6 months,
              omega_V1 = WV_1,#Waning of vaccinated immunity 1 (365/365) #Vaccinated Susceptible -> Unvaccinated Susceptible
              omega_V2 = WV_2, #Waning of vaccinated immunity 2 (365/180) #Recovered Vaccinated Susceptible -> Vaccinated Susceptible
              r = vacrate, ###Vaccination Rates (25%, 50%, 75%)
              r_sub = vacrate * (1/(sum(Rec[18:80]))),  #Vaccination Rate Adjusted for the fact that only 18-80 are vaccinated
              vacprop = vacall , #How much to vaccinate the priority group before opening up (Default is 75%)
              inc_prop = prop_age,
              inc = inc
              
  )
  
  ###I used a coarser time scale because the qualitative patterns
  ###still remain the same with using a (1/365) AND the numerical difference
  ###is very small.
  times = seq(0, 1, by=1/12)
  
  
  ###Our desolver
  dat = as.data.frame(lsoda(y_initial, times = times,
                            func = SAIRV_M_Complex_NVac ,
                            parms = parms,
                            atol = 1e-6, 
                            rtol= 1e-7))
  return(dat)
}


Simulator_func_C <- function(list){
  ###You feed in the list of parameters
  ###This function simulates the model depending on what you feed into it

  ###The parameter values here:
  ###sus1 = susceptibility modifier
  ###trans1 = transmissibility modifier
  ###sev = severity modifier
  ###WN = waning of natural immunity
  ###WV_1 = waning of vaccine immunity 1
  ###Wv_2 = waning of vaccine immunity 2
  ###vacrate = Percent of the total population vaccinated
  ###inc = who gets it first (age1,age2, and age3)
  ###R0 = self explanatory
  ###country = South Africa, USA, Japan




temp_list <- NULL

for (k in seq(1, length(list))) {
  temp_param <- list[[k]]


  tmp <- Simulator_func_3(
    temp_param[[1]],
    temp_param[[2]],
    temp_param[[3]],
    temp_param[[4]],
    temp_param[[5]],
    temp_param[[6]],
    temp_param[[7]],
    temp_param[[8]],
    temp_param[[9]],
    temp_param[[10]],
    temp_param[[11]])


  temp_list[[k]] <- tmp
}

return(temp_list)
}

############################################################
###This is a function for simulating the model with        #
###different parameters values- specifically,              #
###this is the code that we use to figure out the different#
###output with the different values ########################
############################################################
###Cleaned and Last updated 7/29/2021


Simulator_func_C <- function(list, yprop){
  ###You feed in the list of parameters
  ###This function simulates the model depending on what you feed into it

  ###The parameter values here:
  ###sus1 = susceptibility modifier
  ###trans1 = transmissibility modifier
  ###sev = severity modifier
  ###WN = waning of natural immunity
  ###WV_1 = waning of vaccine immunity 1
  ###Wv_2 = waning of vaccine immunity 2
  ###vacrate = Percent of the total population vaccinated
  ###inc = who gets it first (age1,age2, and age3)
  ###R0 = self explanatory
  ###country = South Africa, USA, Japan




temp_list <- NULL

for (k in seq(1, length(list))) {
  temp_param <- list[[k]]


  tmp <- Simulator_model(
    temp_param[[1]],
    temp_param[[2]],
    temp_param[[3]],
    temp_param[[4]],
    temp_param[[5]],
    temp_param[[6]],
    temp_param[[7]],
    temp_param[[8]],
    temp_param[[9]],
    temp_param[[10]],
    temp_param[[11]],
    yprop)


  temp_list[[k]] <- tmp
}

return(temp_list)
}
