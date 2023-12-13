#################################################
###This is for calculating the EVPI for a    ####
###whole country based on the output of      ####
###TSS_US_ALL, TS_JPN_ALL, AND TSS_SAF_ALL   ####
### Calculate the total EVPI for a country   ####
#################################################
###I call this EVPI_CASES_SUPER

EVPI_ALL_SUPER <- function(x) {

  times <- seq(0, 1, 1 / 12)

  ###Split by the actions
  splitted_uncertainty_action <- split(x, x$inc)

  ###Ensure that you have three actions
  #length(splitted_uncertainty_action)
  ###These are for the cases
  ACTIONS_C <- cbind(
    splitted_uncertainty_action[[1]]$x,
    splitted_uncertainty_action[[2]]$x,
    splitted_uncertainty_action[[3]]$x
  )
  ###There should be 1555252 different ways
  ###of uncertainty combination and three actions

  ###THIS IS MORTALITY
  ACTIONS_M <- cbind(
    splitted_uncertainty_action[[1]]$mort,
    splitted_uncertainty_action[[2]]$mort,
    splitted_uncertainty_action[[3]]$mort
  )

  ###Across the different uncertainties, find
  ###the best action (minimal)

  ###We're assuming equal weighting for
  ###all analysis involving TSS_US/JPN/SAF_ALL
  ###this is for the EVPI under CERTAINTY
  ###What this does is that for each parameter combination
  ###(each row), it looks for the best action (minimum
  ###cases or mortality)
  ###

  ##EV_C = certainty of cases (C) and mortality (M)
  EV_C_C <- round(mean(apply(ACTIONS_C, 1, min)),2)
  EV_C_M <- round(mean(apply(ACTIONS_M, 1, min)),2)

  ###Average across actions and find the minimium
  ###EV_UC = expected value under UNCERTAINTY
  EV_UC_C <- round(min(colMeans(ACTIONS_C)),2)
  EV_UC_M <- round(min(colMeans(ACTIONS_M)),2)

  ###This should return the EVPI
  return(data.frame(
    evpi = EV_C_C - EV_UC_C, ###EVPI OF CASES
    evpiper = ((EV_C_C - EV_UC_C) / EV_C_C) * 100,
    evpi_m = EV_C_M - EV_UC_M, ###EVPI OF MORTALITY
    evpi_mper = ((EV_C_M - EV_UC_M) / EV_C_M) * 100
  ))
}
