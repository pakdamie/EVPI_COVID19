##########################################################
###This is the main mathematical framework               #
###that is used in the paper- the function               #
###will be used for deSolve                              #
##########################################################
SAIRV_M_Complex_NVac <-
  function(t, x, params) {

     n <- params$n # The number of subcompartments
                  # (related to maximum lifexpectency- 80)

    #Note that all state compartments have 80 age compartments- meaning that we should have
    # 9 * 80 = 720 equations

    #################
    ##States Class###
    #################

    S <- x[1:n]   ### The Susceptible Unvaccinated Class
    SV <- x[(n + 1):(2 * n)] ### The Susceptible Vaccinated Class
    SU <- x[((2 * n) + 1):(3 * n)] # The Susceptible Vaccinated (failed) Class
    A <- x[((3 * n) + 1):(4 * n)] # The Asymptomatic infectious Class
    I <- x[((4 * n) + 1):(5 * n)] ### The Symptomatic infectious class
    AV <- x[((5 * n) + 1):(6 * n)] ### The Asymptomatic infectious Class (Vac)
    IV <- x[((6 * n) + 1):(7 * n)] ### The Symptomatic infectious class (Vac)
    R <- x[((7 * n) + 1):(8 * n)] ### The Recovered (Unvaccinated/Failed Take)
    RV <- x[((8 * n) + 1):(9 * n)] ### The Recovered (Vaccinated Class)

    ########################
    ### Incidence Classes###
    ########################

    ###For book-keeping the total flux into a compartment

    KI_U <- x[((9 * n) + 1):(10 * n)] ### The symptomatic infection incidence (Unvac) class
    KI_V <- x[((10 * n) + 1):(11 * n)] ### The symptomatic infection incidence class (Vac)
    KA <- x[((11 * n) + 1):(12 * n)] ### The asymptomatic infection incidence class
    VacS<- x[((12 * n) + 1):(13 * n)] ### The Vaccine incidence of susceptible individuals
    VacR<- x[((13 * n) + 1):(14 * n)] ### The Vaccine incidence of recovered individuals

    ######################################
    ### The vaccine allocation code#######
    ######################################

    ###We know which age group would be vaccinated

    with(as.list(params), {

    #Here is the starting vector that has 0 coressponding
    #to all age group

    v <- rep(0,80)

    #Here, we use the equation -log(1-prop)/DT = r, if we know
    #the proportion that will be vaccinated and in how many
    #time units we can get the rate (don't get proportion
    #and rate mixed up!). So if 50% of the total population
    #is vaccinated in one time step (a year, for example),
    ###the rate has to be -log(1-0.5)/1 = 0.69. However, we are vaccinating
    #one age group first before opening it up!

    #Here, I look at the first time-point where 75% of the
    #prioritizated age group will be vaccinated with a total rate
    #of the total population. Wecan do this by using the
    #equation DT = -log(1-prop)/r, this tells us
    ###that depending on the proportion we want vaccinated
    #and the rate,it will take this long!

    ###r/(inc_prop/N) is because the rate has to be adjusted
    ###as it accounts for the TOTAL POPULATION when we
    ###are now vaccinating only one group

    ###Vaccines per (total population * time) has to be
    ###multiplied by total population/sub population to
    ###then get vaccines per (sub population * time)


    DT1 = -log(1-vacprop)/(r/(inc_prop/N))

    #DT1 represents the end timepoint where before it
    #we're only vaccinating the
    #priority age group

    r1 <- ifelse (t <= DT1, (r / (inc_prop/N)), r_sub)

    #r-sub is the adjusted vaccination rate as only the
    #18 to 80 years are being vaccinated

    r2 <- ifelse (t > DT1, r_sub, 0)

    ###This says if the time point is less than the DT1,
    #use this vaccination rate r1, if it's greater
    #Then 18:80 year olds are all vaccinated

    if (t <= DT1){
    v[inc] <- r1
    }
    else if (t > DT1){
      v[18:80] <- r2
    }

    ### This is the contact-matrix part of the FOI
    ### Here W is the contact matrix, tran is the parameter
    ### that controls the transmissibility of the vaccinated
    ### population. The second part (tran *(AV+ IV)+(A+I)/N) is
    ### the infection prevalence with N representing the total populatoin
    ### This is presumed to be 1 because the total population should
    ### be constant

      WI <- W %*% ((tran * (AV + IV) + (A + I)) / N) # The contact matrix times the prevalence

      # Insures no wonkiness (if WI turns out to have negative number )
      WI[!is.finite(WI)] <- 0

      # The first part of the FOI is the transmission coefficient that
      ### is represented by R0*(gamma + mu). We have a susceptibility paramater
      ### sus that reduces the susceptibility of the vaccinated population.

      phi_S <- R0 * (gamma + mu) * (WI) # FOI for unvaccinated/failed
      phi_V <- (sus * (R0 * (gamma + mu)) * (WI)) # FOI for vaccinated

      ### For the susceptible unvaccinated population,
      ### we have the birth rate as the first part.
      ### Remember the only death we have is the last age-compartment
      ### Ottar's Recommendation; we have the ageing out.
      ### To balance out we need to have the last compartment
      ### of each state times those that aged out (a) and died (mu).
      ### All stages then have ageing in and out
      ### Some are going to be infected (phi_S), some are going
      ### to be vaccinated (Depending on age group)

      # There is an inflow of those that lost their immunity
      # aka those from natural population (omega_N) and those from the
      # vaccinated population (omega_V)

      ######################################
      # Susceptible Unvaccinated Population#
      ######################################
      dS <- c((mu + 1) * (S[n] + SV[n] + SU[n] + I[n] + A[n] + AV[n] + IV[n]
        + R[n] + RV[n]), rep(0, n - 1)) +
        c(0, a[1:(n - 1)] * S[1:(n - 1)]) - (a * S) -
        (phi_S * S) - (c(rep(0, n - 1), mu) * S) - (v * S) + (omega_N * R) + (omega_V1 * SV)

      ### The susceptible vaccinated population is dependent on
      ### pv which determines the probability of successful vaccination
      ### We then have susceptibles being lost to infection controlled
      ### by FOI of vaccinated people. Then we have inflow
      ### of the recovered individuals who have lost immunity

       ####################################
       # Susceptible Vaccinated Population#
       ####################################
      dSV <- (pv * v * S) + c(0, a[1:(n - 1)] * SV[1:(n - 1)]) -
        (a + phi_V) * SV - c(rep(0, n - 1), mu) * SV - (omega_V1 * SV) + (omega_V2 * RV)

        ### The  susceptible failed vaccinated population is dependent on
        ### 1-pv which determines the probability of unsuccessful vaccination
        ### We then have susceptibles being lost to infection controlled
        ### by FOI of unvaccinated  people.

      ###########################################
      # Susceptible Failed Vaccinated Population#
      ###########################################

      dSU <- (1 - pv) * v * S + (1 - pv) *  v * R +
        c(0, a[1:(n - 1)] *SU[1:(n - 1)]) -
        (a * SU) - (phi_S * SU) - c(rep(0, n - 1), mu) * SU

      ### The  Asymptomatic Failed/Unvaccinated Population
      ### is dependent on 1-pi which determines the
      ### probability that an infectious individual have asymptomatic
      ### disease. We then have the recovery rate of gamma

       ###########################################
      # Asymptomatic Failed/Unvaccinated Population
      ###########################################
      dA <- (1 - p_i) * phi_S * (S) + (1 - p_i) * phi_S * SU +
        c(0, a[1:(n - 1)] * A[1:(n - 1)]) -
        (a * A) - c(rep(0, n - 1), mu) * A - (gamma * A)

      ### The  Symptomatic Failed/Unvaccinated Population
      ### is dependent on pi which determines the
      ### probability that an infectious individual have symptomatic
      ### disease. We then have the recovery rate of gamma

       #############################################
      # Symptomatic Failed/Unvaccinated Population#
      #############################################

      dI <- p_i * phi_S * (S) + p_i * phi_S * SU +
        c(0, a[1:(n - 1)] * I[1:(n - 1)]) -
        (a * I) - c(rep(0, n - 1), mu) * I - (gamma * I)

      ### The Asymptomatic Vaccinated Population
      ### is dependent on 1-p_iv which determines the
      ### probability that an infectious individual have asymptomatic
      ### disease. We then have the recovery rate of gamma

      ###########################################
      # Asymptomatic Vaccinated Population
      ###########################################

      dAV <- (1 - p_iv) * phi_V * (SV) +
        c(0, a[1:(n - 1)] * AV[1:(n - 1)]) -
        (a) * AV - c(rep(0, n - 1), mu) * AV - (gamma * AV)

       ### The  Symptomatic Vaccinated Population
      ### is dependent on p_iv which determines the
      ### probability that an infectious individual have asymptomatic
      ### disease. We then have the recovery rate of gamma

      ###########################################
      # Symptomatic Vaccinated Population       #
      ###########################################

      dIV <- p_iv * phi_V * (SV) +
        c(0, a[1:(n - 1)] * IV[1:(n - 1)]) -
        (a) * IV - c(rep(0, n - 1), mu) * IV - (gamma * IV)

       ### The Failed/Unvaccinated Recovered Population
      ### We have an inflow depending on gamma (recovery rate) and then
      ### we have the loss due to waning of immunity

      ###########################################
      # Failed/Unvaccinated Recovered Population#
      ###########################################

      dR <- gamma * (I + A) + c(0, a[1:(n - 1)] * R[1:(n - 1)]) -
        a * R - c(rep(0, n - 1), mu) * R - (omega_N * R) -  (v * R)

       ### The Vaccinated Recovered Population
      ### We have an inflow depending on gamma (recovery rate) and then
      ### we have the loss due to waning of immunity

      ###########################################
       # Vaccinated Recovered Population        #
      ###########################################
      dRV <- (pv * v * R) + (gamma * (IV + AV)) + c(0, a[1:(n - 1)] * RV[1:(n - 1)]) -
        a * RV - c(rep(0, n - 1), mu) * RV - omega_V2 * RV


      ##############
      ### INCIDENCE #
      ##############

      ### The symptomatic infectious (unvaccinated)
      KI_U <- (p_i) * phi_S * (S + SU)
      ###The symptomatic infectious (Vaccinated )
      KI_V <- (p_iv) * phi_V * (SV)

       ### The asymptomatic infectious (all)
      KA <- (1 - p_i) * phi_S * (S + SU) + (1 - p_iv) * phi_V * (SV) ### The asymptomatic infection incidence class


       ### The total vaccination (doesn't depend if it's failed or not)

      VacS <- (v * S) ### The Vaccine incidence of those getting vaccinated through the suscseptible
      VacR <- (v * R) ### The vaccine incidene of those getting vaccinated through the recovered


       res <- c(dS, dSV, dSU, dA, dI, dAV, dIV, dR, dRV, KI_U,KI_V, KA, VacS,VacR)

      list((res))

    })
  }
