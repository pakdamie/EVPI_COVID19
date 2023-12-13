###############################
###R SCRIPT TO MAKE FIGURE 2###
################################
###Important script for outputs

###THIS FUNCTION CALCULATES THE OUTPUT- CASELOAD AND MORTALITY-
###You run this before you create the graph
Output_Calculator <- function(list1,listofvariables1,time){

   ###The times we are doing with (2 years with 1/12 months)
  times = seq(0,1,1/12)

  ###This gets the total incidence- this is the total incidence
  ###of the symptomatic (both unvaccinated and vaccinated) with the
  ###asymptomatic, because everything is in proportion we multiplied
  ###it
  TOTAL_INC_ALL <- lapply(list1, function (x) 1e5 * rowSums(x[,722:961]))

  ###This adds the time data.fame to each list entry so
  ###we can subset-based on what time we want

  TOTAL_INC_ALL_T <- lapply(TOTAL_INC_ALL, function (x) data.frame(times, x))

  ###Allows user to pick what time they want to subset-
  ###I chose 1
  TOTAL_INC_ALL_T_S<- lapply(TOTAL_INC_ALL_T ,function(x) x[x$times==time,])

  ###Combines the variable together and d represents
  ###the total cases
  TOTAL_INC_ALL_T_F<- cbind.data.frame(do.call(rbind, TOTAL_INC_ALL_T_S),listofvariables1)

  ###Decide to split it up for easier reading

  ###Death- the CFR was gotten from Verity- This was in percentages and I
  ###multiplied it by 1/100 to get the actualvalue. Only,
  ###symptomatic people can die (it wouldn't make sense
  ###if you were asymptomatic and die from COVID-19?)
  CFR_Verity <- c(.00260, .0148, .0600, 0.146,.295, 1.25, 3.99, 8.61, 13.40)/100

  ###From Ruyiun's code where I use splines to annualize
  ###contact her if you want more information
  CFR = array(NA, 80)

  CFR_sp <- spline(log(CFR_Verity[1:8]), xout=1:8) # log: to keep pos
  CFR_sp_pred = spline(CFR_sp$y, xout=seq(0,8,1/10))

  ### This is the CFR for unvaccinated
  CFR = exp(CFR_sp_pred$y)[-1]
  ### This is the CFR for vaccinated (90% reduction)
  CFR_V= exp(CFR_sp_pred$y)[-1] * 0.1

  ###This is the for unvaccinated,sympomatic total cases
  a_IU<- lapply(list1, function (x)  (x[,722:801]))

  ###This is the for unvaccinated,sympomatic total cases
  a_IV <- lapply(list1, function (x) (x[,802:881]))

  ###We check for times
  b_IU <- lapply(a_IU, function (x) data.frame(times, x))
  b_IV <- lapply(a_IV, function (x) data.frame(times, x))

  ###We get the total deaths by taking the total cases multiplied by the CFR
  c_IU <- lapply(b_IU, function(x) sum((1e5 * x[x$times==time, 2:81]) *(CFR)))
  c_IV <- lapply(b_IV, function(x) sum((1e5 * x[x$times==time, 2:81]) *(CFR_V)))
  
  death_IU<- do.call(rbind, c_IU)
  death_IV<- do.call(rbind, c_IV)

  ###ADD this together to get the total mortality
  TOTAL_INC_ALL_T_F$mort <-   death_IU + death_IV

  return(TOTAL_INC_ALL_T_F)


}